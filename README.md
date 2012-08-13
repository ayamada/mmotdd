My MOTD Distributor
===================

sorry, most this site written in japanese.
you can use any translate services(e.c. http://translate.google.com/ ).

概要
----

これは、crontabによってスクリプト起動され、
特定のURLに書いてあるS式を読み込み、
それを元データとして、特定のルールによってランダム選択を行い、
それをRSS化するものです。
結果として、このRSSを購読しているユーザに対して、
ある程度定期的に、元データS式のエントリが送信されます。
つまり、man 1 fortuneやman 5 motdのような機能を果たします。

また、上記と同時に、全エントリのhtml化も実行されます。
この際に各エントリのURLはpermanent linkとなり、
全ページのインデクスも生成されます。


サンプル
--------

http://mmotdd.tir.jp/


利用方法
--------

あとで書きます。
とりあえずGaucheが必要です。


内部用一時メモ
--------------

内部で保持するdbmは二つ
- content-dbm。これはmmotdd.txtをparseして取り込んだdbm。mmotdd.txtがparseされる毎に毎回全更新される
    - この全更新によってエントリが消える事がある点に注意
- state-dbm。上記dbmの各エントリに対する集計情報等を保持するdbm
  - 各エントリのkeyに対して、評価等を記録するもの。上記の通り、上のdbmは毎回全更新される為。
    - ただし、keyが""(空文字列)のエントリのみ特別扱いとして、ランダムrssの履歴情報等をS式dumpの形式で保持する
    - これはより良い名前を考える必要あり


TODO
----

優先度高

- rssに、過去の履歴もある程度保持するようにする
- latest更新(rss更新含む)タイミングのランダム化(更新タイミングを不定にする実装)
- 最近選択されたエントリほど選択されにくくなるようにする
- state-dbmに「エントリが生成された日時」「エントリが更新された日時」を記録するようにする
- mmotdd.txtの元エントリの書式チェックを実装する
- mmotdd.txtが空だった(S式が一個もなかった)時のチェックが必要


優先度中

- mmotdd.txtのダイジェスト値保存対応
- 多重起動防止
- state-dbmのより良い名前を考える
    - 各エントリの状況保存と、共通設定保存の両方を保存している為、いい名前をつけづらい
- mmotdd.tir.jp の各コンテンツは、別ブランチに保存する
  (アップデート時はrebaseを使う)
    - 上記に伴い、サンプルのmmotdd.txt等も別に用意する。
      具体的には、リポジトリ内にサンプルのmmotdd.txtを置いておき、
      デフォルトではgithub内の上記サンプルのrawのurlを設定しておくようにする。
- html生成時に、全く更新がない場合はファイルをいじらないようにする
    - これは要はhttpdのLast-Modified対応の為のもの
- 最低でもあとでrss生成部分はモジュール化した方がいい(他でも使う為)
- twitter配信対応
- 各エントリのスコア付け(人気投票？)機能
    - スコアを誰でもつけられるようにするのか(荒らし対応必要？)、
      それともメンテナだけとするのか、等の仕様を考える必要あり
- 設定されている環境変数の書式等が正しいかどうかのチェックを実装
- コマンド実行時のusageに、設定可能な環境変数一覧を表示する


優先度低

- あとで本体部分をモジュールに移動させる事を考える
- fsdbm以外も選べるようにする
- ドキュメント/コメントの英語化(の練習)
- tumblr、はてブ、facebook、google+1、等々のボタンをつける
- wget引数を変更できるうまい方法を考える
    - 要は、wget以外のcurl等にも対応させたい為
- htmlテンプレート分離機能






