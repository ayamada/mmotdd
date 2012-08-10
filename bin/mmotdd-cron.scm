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

  ;; 既にdbmにエントリがあったら重複なのでエラーを投げる
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


(define (make-entry-html mmotdd-entry state-dbm)
  ;; TODO: テンプレート分離できるようにしたい、が…
  (html:html
    (html:head
      )
    (html:body
      (render-html (mmotdd-entry->contents mmotdd-entry))
      )))

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
              (write-tree (make-entry-html mmotdd-entry state-dbm)))))))))

(define (update-index-html! state-dbm)
  (with-content-dbm
    (lambda (content-dbm)
      (with-output-to-file
        (format "~a/entries.html" (read-config "HTDOCSDIR"))
        (lambda ()
          (write-tree
            ;; TODO: あとで
            "まだだよ！ごめんね"
            ))))))

(define (format-rss-title date)
  (format
    (date->string date "~~aより、~m月~e日~H時~M分に配信")
    (read-config "MMOTDD_TITLE")))

(define (make-rss-date date)
  ;; "Sat,  4 Aug 2012 17:00:00 +0900"
  (date->rfc822-date date))


(define *url-rx* #/https?:\/\/(\/\/[^\/?#\s]*)?([^?#\s\"]*(\?[^#\s\"]*)?(#[^\s\"]*)?)/)
(define (render-html entry-contents)
  ;; ↑の entry-contents は、ある一つのentry内の全てのcontent文字列のリスト。
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

  (html:p
    (intersperse
      (html:br)
      (map
        (lambda (line)
          ;; ここで、lineのhtml化を行う。以下の変換を全て同時に行う。
          ;; - urlの自動リンク化(↓で行う)
          ;; - 空白の&nbsp;化(safe-textで行う)
          ;; - html-escape-string(safe-textで行う)
          (let next ((result '())
                     (left line))
            (cond
              ((equal? "" left) (reverse result))
              ((*url-rx* left) => (lambda (m)
                                    (next
                                      (list*
                                        (html:a
                                          :href (m)
                                          (html-escape-string (m)))
                                        (safe-text (m 'before))
                                        result)
                                      (m 'after))))
              (else
                (next
                  (cons (safe-text left) result)
                  "")))))
        entry-contents))))

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
           (let1 entry-url (string-append
                             (read-config "MMOTDD_ROOT_URL")
                             "entry/"
                             (mmotdd-entry->key e)
                             ".html")
             (list
               " <item>\n"
               "   <title>"
               (html-escape-string (format-rss-title (current-date)))
               "</title>\n"
               "   <description>"
               (render-rss (mmotdd-entry->contents e))
               "</description>\n"
               "   <link>"
               (html-escape-string entry-url)
               "</link>\n"
               "   <guid>"
               (html-escape-string entry-url)
               "</guid>\n"
               "   <pubDate>" ; これはuniqueである必要がある？(sort順に影響？)
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
      ;; 必要な処理は以下の通り
      (let1 mmotdd-src-path (make-tmpfilename (string-append
                                                (read-config "TMPDIR")
                                                "/mmotdd_src_"))
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
                (print "done."))))
          (lambda ()
            (sys-unlink mmotdd-src-path))))
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
      (print "all done."))))



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

(define (main args)
  ;; TODO: 多重起動を防ぐ必要あり！必須！
  (let/cc return
    (if (all-env-is-ok?)
      (mmotdd-cron-main)
      (mmotdd-cron-usage)))
  0)

;; vim:set ft=scheme lispwords+=define-env-entry:
;; Local variables:
;; mode: scheme
;; end:
