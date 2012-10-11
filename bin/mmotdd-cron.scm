#!/usr/bin/env gosh
;; -*- mode:scheme; coding:utf-8 -*-

;; STDOUT, STDERR に色々と出力します。
;; それが嫌なら > /dev/null 2>&1 をつけておいてください。

;; 設定について:
;; 環境変数を使って設定します。
;; つまり、起動コマンドは以下のようになります。
;; HOGE=123 bin/mmotdd-cron.scm
;; (普通にexportとかしても問題ありません)
;; 設定項目は以下となります。

;; 全体的なTODOは、README.mdに書きます。


(add-load-path "../lib")

(use srfi-1)
(use file.util)
(use gauche.fcntl)
(use dbm.fsdbm)
(use gauche.process)
(use util.digest)
(use rfc.sha)
(use text.tree)
(use text.html-lite)
(use util.list)

(use srfi-19)
(use rfc.822)

(use srfi-27)
(use math.const)
(random-source-randomize! default-random-source)

;; alist of (name checker-proc fallback)
(define *env-entries* '())

(define-macro (define-env-entry name checker-proc fallback)
  `(let1 env-entry (list ,name ,checker-proc ,fallback)
     (set! *env-entries* (cons env-entry *env-entries*))))
(define env-entry->name car)
(define env-entry->checker-proc cadr)
(define env-entry->fallback caddr)

(define (get-script-dir)
  (sys-dirname *program-name*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 利用可能な環境変数の設定

(define-env-entry "MMOTDD_TITLE"
  (lambda _ #t)
  "mmotdd.tir.jp")
(define-env-entry "MMOTDD_ROOT_URL"
  ;; NB: URLの終端は必ず / である事
  (lambda _ #t)
  "http://mmotdd.tir.jp/")
(define-env-entry "MMOTDD_DESC"
  (lambda _ #t)
  "mmotdd.tir.jpからの俺格言配信")
(define-env-entry "MMOTDD_SRC_URL"
  (lambda _ #t)
  "https://dl.dropbox.com/u/9755436/mmotdd.txt")
(define-env-entry "MMOTDD_CSS_URL"
  (lambda _ #t)
  "http://css.tir.jp/tir.css")

(define-env-entry "WGET_CMD"
  (lambda _ #t)
  "wget")
(define-env-entry "WGET_ARGS_MAKER"
  (lambda _ #t)
  ;; TODO: 現在のところ、これをenvから書き換える事はできない…
  (lambda (output-file url)
    `("-O" ,output-file ,url)))

(define-env-entry "HTDOCSDIR"
  (lambda (dir)
    ;; TODO: ディレクトリかどうか等のチェックを行う事
    #t)
  (format "~a/../htdocs" (get-script-dir)))

(define-env-entry "DATADIR"
  (lambda (dir)
    ;; TODO: ディレクトリかどうか等のチェックを行う事
    #t)
  (format "~a/../data" (get-script-dir)))

(define-env-entry "TMPDIR"
  (lambda (dir)
    ;; TODO: ディレクトリかどうか等のチェックを行う事
    #t)
  (format "~a/../tmp" (get-script-dir)))

(define-env-entry "LOCKFILE"
  (lambda (path)
    ;; TODO: チェック
    #t)
  (format "~a/../tmp/mmotdd.lock" (get-script-dir)))

(define-env-entry "INTERVAL_SEC_NORM"
  (lambda (sec)
    ;; TODO: 数値かどうか等のチェックを行う事
    #t)
  (* 24 60 60))










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-tmpfilename template)
  (receive (_ tmpfilename) (sys-mkstemp template)
    (sys-unlink tmpfilename)
    tmpfilename))

(define (read-config name)
  (let1 env-entry (assoc name *env-entries*)
    (unless env-entry
      (error "invalid env name" name))
    (or
      (sys-getenv name)
      (env-entry->fallback env-entry))))

;; usage:
;; (with-dbm
;;   <qdbm>
;;   :path "/path/to/dbm.file"
;;   :rw-mode :read
;;   :key-convert #f
;;   :value-convert #t
;;   (lambda (dbm)
;;     ;; dbmを使った操作を行う
;;     ;; このprocの実行が完了すると、自動的にdbmは閉じられる
;;     ))
(define (with-dbm dbm-type . dbm-open-args&proc)
  (let ((dbm-open-args (drop-right dbm-open-args&proc 1))
        (proc (car (last-pair dbm-open-args&proc))))
    (let1 dbm #f
      (dynamic-wind
        (lambda ()
          (set!
            dbm
            (apply dbm-open dbm-type dbm-open-args)))
        (lambda ()
          (proc dbm))
        (lambda ()
          (dbm-close dbm)
          (set! dbm #f))))))


(define (get-dbm-path name)
  (format "~a/~a.dbm" (read-config "DATADIR") name))
(define (get-content-dbm-path)
  (get-dbm-path "content"))
(define (get-state-dbm-path)
  (get-dbm-path "state"))

(define mmotdd-entry->version car)
;; TODO: mmotdd-entry->contents と mmotdd-entry->keywords の共通部分を統合
(define (mmotdd-entry->contents mmotdd-entry)
  (let next ((result '())
             (cur (cdr mmotdd-entry)))
    (cond
      ((null? cur) (reverse result))
      ((string? (car cur)) (next (cons (car cur) result) (cdr cur)))
      ((keyword? (car cur)) (next result (cddr cur)))
      (else (error "assertion" cur)))))
(define (mmotdd-entry->keywords mmotdd-entry)
  (let next ((result '())
             (cur (cdr mmotdd-entry)))
    (cond
      ((null? cur) result)
      ((string? (car cur)) (next result (cdr cur)))
      ((keyword? (car cur)) (next
                              (list* (car cur)
                                     (cadr cur)
                                     result)
                              (cddr cur)))
      (else (error "assertion" cur)))))
(define (mmotdd-entry->key mmotdd-entry)
  (get-keyword :key (mmotdd-entry->keywords mmotdd-entry)))
(define (mmotdd-entry->deliverable? mmotdd-entry)
  (get-keyword :deliverable? (mmotdd-entry->keywords mmotdd-entry)))
(define (mmotdd-entry->remarks mmotdd-entry)
  (get-keyword :remarks (mmotdd-entry->keywords mmotdd-entry)))

(define (parse-mmotdd-entry sexpr)
  ;; - sexprがlistである
  ;; - 最初の要素がmmotdd-entryである
  ;; この条件を満たすなら、cdrを返す
  ;; TODO: チェックはあとで追加する
  (cdr sexpr))
(define (check-mmotdd-entry new-content-dbm mmotdd-entry)
  ;; 必要な書式を満たしているかをチェック
  ;; - carが「MMOTDD/0.1」のシンボルである事
  ;;;; TODO: あとで
  ;; - :key を持つ事
  ;;;; TODO: あとで
  ;; - :key が空文字列でない事
  (when (equal? "" (mmotdd-entry->key mmotdd-entry))
    (error "key is null string" mmotdd-entry))
  ;; - コンテンツ本体である文字列を含む事
  ;;;; TODO: あとで
  ;; - 既にdbmにエントリがあったら重複なのでエラーを投げる
  (when (dbm-get new-content-dbm (mmotdd-entry->key mmotdd-entry) #f)
    (error "duplicate entry" (mmotdd-entry->key mmotdd-entry)))
  #t)

(define (with-content-dbm proc)
  (with-dbm
    <fsdbm>
    :path (get-content-dbm-path)
    :rw-mode :read
    :key-convert #f
    :value-convert #t
    proc))

(define (update-content-dbm! state-dbm mmotdd-src-path mmotdd-digest)
  ;; ソースを順にreadし、dbmに入れていく。
  ;; エラーがあったらその時点で例外を投げる。
  ;; エラーがあった場合は作成途中のdbmも削除する必要がある
  (let1 tmp-dbm-path (make-tmpfilename (string-append
                                         (read-config "TMPDIR")
                                         "/tmp_dbm_"))
    (with-input-from-file
      mmotdd-src-path
      (lambda ()
        (guard (e (else
                    (when (file-exists? tmp-dbm-path)
                      (remove-directory* tmp-dbm-path))
                    (error e)))
          (with-dbm
            <fsdbm>
            :path tmp-dbm-path
            :rw-mode :write
            :key-convert #f
            :value-convert #t
            (lambda (new-content-dbm)
              ;; TODO: ファイル内のエントリが0個だった場合もエラーとする
              (let next ((sexpr (read)))
                (if (eof-object? sexpr)
                  #t
                  (let1 mmotdd-entry (parse-mmotdd-entry sexpr)
                    (check-mmotdd-entry new-content-dbm mmotdd-entry)
                    (dbm-put! new-content-dbm
                              (mmotdd-entry->key mmotdd-entry)
                              mmotdd-entry)
                    (next (read))))))))))
    ;; 最後に、content-dbmを一括更新する
    (let1 old-dbm-path (make-tmpfilename (string-append
                                           (read-config "TMPDIR")
                                           "/old_dbm_"))
      ;; まず古いcontent-dbmを移動(あるなら)
      (guard (e (else
                  ;; これがうまく動かなかった場合は、
                  ;; どういうリカバリ処理が必要か不明、危険だが
                  ;; どうしようもない
                  (error e)))
        (when (file-exists? (get-content-dbm-path))
          (dbm-db-move <fsdbm> (get-content-dbm-path) old-dbm-path)))
      (guard (e (else
                  ;; rollbackを行う
                  (when (file-exists? (get-content-dbm-path))
                    (remove-directory* (get-content-dbm-path)))
                  (dbm-db-move <fsdbm> old-dbm-path (get-content-dbm-path))
                  (error e)))
        ;; 次に今生成したcontent-dbmを正しいpathへ移動
        (dbm-db-move <fsdbm> tmp-dbm-path (get-content-dbm-path))
        ;; 最後に古いcontent-dbmを削除
        (when (file-exists? old-dbm-path)
          (dbm-db-remove <fsdbm> old-dbm-path))))))


(define (get-rss-url)
  (string-append (read-config "MMOTDD_ROOT_URL") "mmotdd.rss"))

(define (make-html html-body . keywords)
  (list
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
    (html-doctype :type :xhtml-1.0-transitional)
    (html:html
      (html:head
        (html:meta :http-equiv "Content-Type" :content "text/html; charset=utf-8")
        (html:meta :http-equiv "Content-Style-Type" :content "text/css")
        ;(html:meta :http-equiv "Content-Script-Type" :content "text/javascript")
        (html:meta :name "ROBOTS" :content "INDEX,FOLLOW")
        (html:link :rel "alternate" :type "application/rss+xml" :title "RSS 2.0" :href (get-rss-url))
        (html:link :rel "Stylesheet" :media "screen" :type "text/css" :href (read-config "MMOTDD_CSS_URL") :title "default")
        (html:title (html-escape-string (read-config "MMOTDD_TITLE"))))
      (html:body
        html-body
        (html:hr)
        (html:address
          :style "text-align:right; font-size: 0.8em"
          "distributed by "
          (html:a :href (read-config "MMOTDD_ROOT_URL")
                  :target "_top"
                  (html-escape-string (read-config "MMOTDD_TITLE")))
          "."
          )))))


(define (update-entry-html! state-dbm)
  ;; TODO: 既にhtmlが生成されており、しかも生成された内容と全く同一の場合は
  ;;       ファイルを更新しないようにする
  ;;       (httpdのLast-Modified: ヘッダを更新してしまわないように)
  (with-content-dbm
    (lambda (content-dbm)
      (dbm-for-each
        content-dbm
        (lambda (key mmotdd-entry)
          ;; 書き出す
          (with-output-to-file
            (format "~a/entry/~a.html" (read-config "HTDOCSDIR") key)
            (lambda ()
              (write-tree
                (make-html (render-html mmotdd-entry state-dbm))))))))))

(define (update-index-html! state-dbm)
  (with-content-dbm
    (lambda (content-dbm)
      (with-output-to-file
        (format "~a/entry/index.html" (read-config "HTDOCSDIR"))
        (lambda ()
          (write-tree
            (make-html
              (render-index-html content-dbm state-dbm))))))))

(define (format-rss-title date)
  (format
    (date->string date "~~aより、~m月~d日~H時~M分に配信")
    (read-config "MMOTDD_TITLE")))

(define (make-rss-date date)
  ;; "Sat,  4 Aug 2012 17:00:00 +0900"
  (date->rfc822-date date))


(define *url-rx* #/https?:\/\/(\/\/[^\/?#\s]*)?([^?#\s\"]*(\?[^#\s\"]*)?(#[^\s\"]*)?)/)
(define (render-html mmotdd-entry state-dbm)
  (define (safe-text str)
    (let next ((result '())
               (left str))
      (cond
        ((equal? "" left) (reverse result))
        ((#/ / left) => (lambda (m)
                          (next
                            (list*
                              "&nbsp;"
                              (html-escape-string (m 'before))
                              result)
                            (m 'after))))
        (else
          (next
            (cons (html-escape-string left) result)
            "")))))
  (define (text->html str)
    ;; ここで、strのhtml化を行う。以下の変換を全て同時に行う。
    ;; - urlの自動リンク化(↓で行う)
    ;; - 空白の&nbsp;化(safe-textで行う)
    ;; - html-escape-string(safe-textで行う)
    (let next ((result '())
               (left str))
      (cond
        ((equal? "" left) (reverse result))
        ((*url-rx* left) => (lambda (m)
                              (next
                                (list*
                                  (html:a
                                    :href (m)
                                    :target "_blank"
                                    (html-escape-string (m)))
                                  (safe-text (m 'before))
                                  result)
                                (m 'after))))
        (else
          (next
            (cons (safe-text left) result)
            "")))))

  (let ((entry-contents (mmotdd-entry->contents mmotdd-entry))
        (remarks (mmotdd-entry->remarks mmotdd-entry))
        )
    (list
      ;; :deliverable?フラグの表示
      (if (mmotdd-entry->deliverable? mmotdd-entry)
        '()
        (html:div
          :style "text-align:center; font-size:0.9em"
          "(このエントリは現在、配信選択されない設定にされています)"))
      ;; 本文
      (html:div
        :style "border:double medium black; margin:0.5em; padding:0.5em; font-size: 1.1em"
        (intersperse
          (html:br)
          (map
            (lambda (line)
              (text->html line))
            entry-contents)))
      ;; 備考欄をつける
      (if (equal? "" remarks)
        '()
        (html:div
          :style "text-align:right; font-size: 0.8em"
          "("
          (text->html remarks)
          ")"
          ))
      ;; エントリ登録日時/エントリ更新日時をつける
      ;; TODO: あとで
      ;; permalinkをつける
      (html:div
        :style "text-align:right; font-size: 0.8em"
        (html:a
          :href (get-entry-url mmotdd-entry)
          :target "_top"
          "permalink"))
      )))

(define (render-index-html content-dbm state-dbm)
  (list
    (html:h1
      (html-escape-string (read-config "MMOTDD_TITLE"))
      "のエントリ一覧")
    (html:div
      (html:a
        :href (read-config "MMOTDD_ROOT_URL")
        "戻る"))
    (html:table
      :style "border:double medium black; margin:0.2em; padding:0.2em"
      :border "1"
      (html:tr
        (html:th "固有キー")
        (html:th "本文抜粋")
        (html:th "備考抜粋")
        (html:th "配信選択")
        (html:th "登録日時")
        (html:th "更新日時")
        ;(html:th "")
        )
      (filter-map
        (lambda (key)
          (let* ((mmotdd-entry (dbm-get content-dbm key))
                 (key (mmotdd-entry->key mmotdd-entry))
                 (url (get-entry-url mmotdd-entry))
                 )
            (define (abstract str)
              (if (< 20 (string-length str))
                (string-append (substring str 0 16) "...")
                str))

            (if (equal? "" key)
              #f
              (html:tr
                (html:td
                  (html:code
                    (html:a :href url (html-escape-string key))))
                (html:td (html-escape-string
                           (abstract
                             (apply string-append
                                    (mmotdd-entry->contents mmotdd-entry)))))
                (html:td (html-escape-string
                           (abstract (mmotdd-entry->remarks mmotdd-entry))))
                (html:td (if (mmotdd-entry->deliverable? mmotdd-entry)
                           (html:span :style "color:blue" "配信可能")
                           (html:span :style "color:red" "配信不可")))
                (html:td "(未実装)")
                (html:td "(未実装)")
                ))))
        (sort
          (dbm-map content-dbm (lambda (k v) k)))))
    (html:div
      (html:a
        :href (read-config "MMOTDD_ROOT_URL")
        "戻る"))
    ))

(define (render-rss entry-contents)
  ;; RSS2.0の仕様について調べたが、descriptionの中身は
  ;; textともhtmlとも決まってないらしい…
  ;; (なのでサイトによってtextのみだったり、htmlタグが入ってたりする)
  ;; ここではとりあえずtextのみで送る事にする
  (intersperse
    "\n"
    (map
      (lambda (line)
        ;; NB 他にも整形処理を入れてよい
        (html-escape-string line))
      entry-contents)))

(define (get-entry-url mmotdd-entry)
  (string-append
    (read-config "MMOTDD_ROOT_URL")
    "entry/"
    (mmotdd-entry->key mmotdd-entry)
    ".html"))

(define (make-rss entries state-dbm)
  (let ((e/title (html-escape-string (read-config "MMOTDD_TITLE")))
        (e/link (html-escape-string (read-config "MMOTDD_ROOT_URL")))
        (e/description (html-escape-string (read-config "MMOTDD_DESC")))
        (e/builddate (html-escape-string (make-rss-date (current-date))))
        )
    `(
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<rss version=\"2.0\">\n"
      "<channel>\n"
      "<title>" ,e/title "</title>\n"
      "<link>" ,e/link "</link>\n"
      "<description>" ,e/description "</description>\n"
      "<language>ja</language>\n"
      "<lastBuildDate>" ,e/builddate "</lastBuildDate>\n"
      ,(map
         (lambda (e)
           (let ((e/guid (html-escape-string
                           (string-append (get-entry-url e)
                                          "#"
                                          (x->string (sys-time)))))
                 )
             (list
               " <item>\n"
               "   <title>"
               (html-escape-string (format-rss-title (current-date)))
               "</title>\n"
               "   <description>"
               (render-rss (mmotdd-entry->contents e))
               "</description>\n"
               "   <link>"
               e/guid
               "</link>\n"
               "   <guid>"
               e/guid
               "</guid>\n"
               ;; GUIDについて：
               ;; - mmotddは同じメッセージを複数回フィードする事が前提の
               ;;   システムだが、GUIDが同じだと「既に投稿されている」判定が
               ;;   なされて通知されないrssリーダーがある。
               ;;   それを避ける為に、毎回違うGUIDを生成する。
               ;;   GUID等についての詳細は以下を参考にした。
               ;;   http://www.futomi.com/lecture/japanese/rss20.html#hrelementsOfLtitemgt
               ;;   また、GUIDを見ずにlinkでその判定を行うrssリーダーもあるので
               ;;   こちらも同様の処理を行う事とする
               "   <pubDate>" ; これはsort順に影響するらしい？
               (html-escape-string (make-rss-date (current-date)))
               "</pubDate>\n"
               " </item>\n"
               )))
         entries)
      "</channel>\n"
      "</rss>")))

(define (update-latest-rss! save-entries state-dbm)
  ;; 書き出す
  (with-output-to-file
    (string-append
      (read-config "HTDOCSDIR")
      "/mmotdd.rss")
    (lambda ()
      (write-tree (make-rss save-entries state-dbm)))))


(define (wget&get-digest mmotdd-src-path)
  ;; wget時に何か異常が発生した時は、そのままエラー例外を投げてよいものとする
  (let1 p (run-process
            (cons
              (read-config "WGET_CMD")
              ((read-config "WGET_ARGS_MAKER") mmotdd-src-path
                                               (read-config "MMOTDD_SRC_URL")))
            :wait #t)
    ;; 正常に取得できていなければエラーを投げて終了
    (unless (eqv? 0 (process-exit-status p))
      (error "wget failed")))
  ;; 正常に取得できた。ファイルを読み込みdigestを取る
  (digest-hexify
    (with-input-from-file
      mmotdd-src-path
      sha512-digest)))


(define (digest-is-not-match? state-dbm mmotdd-digest)
  (not
    (equal?
      mmotdd-digest
      (dbm-get state-dbm "%mmotdd-src-digest" #f))))

(define (can-update-rss? state-dbm)
  ;; TODO: あとでrss更新するか今回はパスするかの判定を実装する事
  ;; (要は、ここで、ランダム更新のゆらぎを実装する)
  #t)

(define (choose-new-entry state-dbm)
  (with-content-dbm
    (lambda (content-dbm)
      ;; まず過去の記録なしに、ランダムに選ぶ
      (let* ((all-entries (dbm-map content-dbm (lambda (key val) val)))
             (deliverable-entries (filter
                                    (lambda (e)
                                      (mmotdd-entry->deliverable? e))
                                    all-entries))
             (get-entry (lambda ()
                          (list-ref deliverable-entries
                                    (random-integer
                                      (length deliverable-entries)))))
             )
        (get-entry)))))

(define (merge-entry state-dbm new-entry)
  ;; TODO: 今は一個だけでいいものとする
  (list new-entry))

(define (update-latest-html! new-entries state-dbm)
  ;; ここでは、シンボリックリンクを貼りかえるだけ
  ;; 今のところ、 new-entries は、一番先頭が一番新しいものとして判定する
  ;; (実際には state-dbm から見るべき？)
  (let* ((latest-entry (car new-entries))
         (latest-key (mmotdd-entry->key latest-entry))
         ;; ↓これは、生成するsymbolic-linkの中に入るべきpath
         (src-path (format "entry/~a.html" latest-key))
         ;; ↓これは、cwdからアクセスするlatest.htmlへの実path
         (dst-path (format "~a/latest.html" (read-config "HTDOCSDIR")))
         )
    (sys-unlink dst-path)
    (sys-symlink src-path dst-path)
    #t))

(define (is-update-interval-ok? state-dbm)
  ;; TODO: この判定はかなり適当で考え直す余地が多い
  ;;       (厳密にやるなら更新後に「次は何時何分以降に更新」とdbmに保存すべき)
  (let ((interval-sec
          (normal-distribution-random
            :mu (read-config "INTERVAL_SEC_NORM")
            :sigma (/ (read-config "INTERVAL_SEC_NORM") 2)
            :min 1
            :max (* 6 (read-config "INTERVAL_SEC_NORM"))
            ))
        (last-update-epoch (dbm-get state-dbm "%last-update-epoch" 0))
        (now-epoch (sys-time))
        (now-date (current-date))
        )
    (and
      ;; 0時-8時は更新しないようにする
      (< 8 (date-hour now-date))
      ;; 上記での判定結果を返す
      (< now-epoch (+ last-update-epoch interval-sec)))))

(define (mmotdd-cron-main)
  ;; state-dbmは常時openしておく
  ;; content-dbmは必要な時のみopenして使うようにする
  ;; (こっちのみ丸ごと生成し直しがある為)
  (with-dbm
    <fsdbm>
    :path (get-state-dbm-path)
    :rw-mode :write
    :key-convert #f
    :value-convert #t
    (lambda (state-dbm)
      (let1 mmotdd-src-path (make-tmpfilename (string-append
                                                 (read-config "TMPDIR")
                                                 "/mmotdd_src_"))
        ;; まず更新処理を実行するかどうかの判定を行う
        (if (is-update-interval-ok? state-dbm)
          (print "passed by interval.") ; まだ更新から間がないと判定
          (begin
            ;; 必要な処理は以下の通り
            (dynamic-wind
              (lambda () #t)
              (lambda ()
                (print "wget mmotdd.txt")
                (let1 mmotdd-digest (wget&get-digest mmotdd-src-path)
                  ;; - 現在のdbmの元となったmmotdd.txtとdigest値が違うなら更新処理実行
                  (when (digest-is-not-match? state-dbm mmotdd-digest)
                    (print "mmotdd.txt is modified.")
                    (print "updating static html ...")
                    ;; content-dbmと、state-dbm内のdigestを更新
                    (update-content-dbm! state-dbm mmotdd-src-path mmotdd-digest)
                    ;; dbmから静的htmlを再生成
                    (update-entry-html! state-dbm)
                    (update-index-html! state-dbm)
                    (dbm-put! state-dbm "%mmotdd-src-digest" mmotdd-digest)
                    (print "done."))))
              (lambda ()
                (sys-unlink mmotdd-src-path)))
            ;; rssを更新するか判定
            (when (can-update-rss? state-dbm)
              (print "new entry found.")
              (let* ((new-entry (choose-new-entry state-dbm))
                     (new-entries (merge-entry state-dbm new-entry)))
                ;; htmlを更新する
                (print "updating latest html ...")
                (update-latest-html! new-entries state-dbm)
                (print "done.")
                ;; rssを更新する
                (print "updating latest rss ...")
                (update-latest-rss! new-entries state-dbm)
                (print "done.")
                ))
            ;; タイムスタンプ更新
            (dbm-put! state-dbm "%last-update-epoch" (sys-time))
            (print "all done.")))))))


(define (normal-distribution-random . keywords)
  ;; note: この関数は二つの正規乱数を多値で返す。
  ;;       (一つしか必要ない場合は片方は適当に捨てれば良い)
  ;;       キーワード引数は以下の通り。
  (let-keywords* keywords (
                           (sigma 1) ; 分散度を指定。
                           ;; sigmaが大きいと平均的に分散し、
                           ;; sigmaが0に近いとmu近辺しか出なくなる。
                           (sigma-plus #f) ; +方向と-方向で、sigmaの値を
                           (sigma-minus #f) ; 変更したい場合に個別に指定する。
                           ;; 尚、sigma-plusかsigma-minusにマイナスの値を
                           ;; 指定する事で、半分に折り返した正規分布を
                           ;; 作る事も可能。
                           (mu 0) ; 一番出やすい期待値(平均の中央)を指定。
                           (clamp-min :min #f) ; 結果をclampする場合に指定
                           (clamp-max :max #f) ; 結果をclampする場合に指定
                           )
    ;; 「（0, 1］」の乱数を二つ用意する
    ;; TODO: 今には「(0, 1)」になっている、が面倒なのでこれで通す
    (define get-r random-real)
    (let ((alpha (get-r))
          (beta (get-r)))
      ;; 以下の計算を行う事で、二つの相関の無い正規乱数が得られる。
      ;; ((-2 * log(α))^0.5) * sin(2 * PI * β)
      ;; ((-2 * log(α))^0.5) * cos(2 * PI * β)
      ;; 左辺(result-alpha-part)と、sin/cosの中味(result-beta-part)を先に求める
      (let ((result-alpha-part (expt
                                 (* -2 (log alpha))
                                 0.5))
            (result-beta-part (* 2 pi beta)))
        ;; 二つの正規乱数を得る。
        (let ((result1 (* result-alpha-part (sin result-beta-part)))
              (result2 (* result-alpha-part (cos result-beta-part))))
          ;; 上記二つの正規乱数を加工して返す
          (define (post-process r)
            (clamp
              (+ mu
                 (* r (or
                        (if (positive? r) sigma-plus sigma-minus)
                        sigma)))
              clamp-min
              clamp-max))
          (values (post-process result1) (post-process result2)))))))


(define (mmotdd-cron-usage)
  (print "usage:")
  (print "HOGE=123 bin/mmotdd-cron.scm")
  (print "environ:")
  ;; TODO: あとで一覧とデフォルト値を表示するようにする事
  (print "(atode)")
  #t)



(define (all-env-is-ok?)
  ;; TODO: あとでちゃんとチェックするコードを書く
  #t)

(define (with-lock/block thunk)
  (let ((f-rdlck (make <sys-flock> :type F_RDLCK))
        (f-wrlck (make <sys-flock> :type F_WRLCK))
        (f-unlck (make <sys-flock> :type F_UNLCK))
        (lock-file (read-config "LOCKFILE"))
        )
    (define (write-lock path)
      (let1 p (open-output-file
                path
                :if-does-not-exist :create
                :if-exists :append
                )
        (sys-fcntl p F_SETLKW f-wrlck)
        p))
    (define (unlock p)
      (when (output-port? p)
        (flush p))
      (sys-fcntl p F_SETLKW f-unlck)
      (cond
        ((input-port? p) (close-input-port p))
        ((output-port? p) (close-output-port p))
        (else (errorf "cannot unlock ~s" p))))

    (let1 p #f
      (dynamic-wind
        (lambda ()
          (set! p (write-lock lock-file)))
        thunk
        (lambda ()
          (unlock p))))))

(define (main args)
  (let/cc return
    (if (all-env-is-ok?)
      (with-lock/block mmotdd-cron-main)
      (mmotdd-cron-usage)))
  0)

;; vim:set ft=scheme lispwords+=define-env-entry:
;; Local variables:
;; mode: scheme
;; end:
