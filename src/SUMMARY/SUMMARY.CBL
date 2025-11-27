       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         SUMMARY.
      ******************************************************************
      * システム名    ：研修
      * サブシステム名：
      * プログラム名  ：売上集計
      * 作成日／作成者：２０２５年１１月２６日  ホ
      * 変更日／変更者：
      *       変更内容：
      ******************************************************************
      *
       ENVIRONMENT         DIVISION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
       
       DATA                DIVISION.
       FILE                SECTION.

       WORKING-STORAGE     SECTION.
       01  INSERT-CNT      PIC  9(9) PACKED-DECIMAL VALUE 0.
       01  FETCH-CNT       PIC  9(9) PACKED-DECIMAL VALUE 0.
       01  FETCH-END       PIC  X VALUE 'N'.
      ******************************************************************
      *  データベースアクセス関連
      *     ※ COBOL ⇔ DB する変数は DECLARE SECTION で行う。
      ******************************************************************
       EXEC SQL  BEGIN DECLARE SECTION  END-EXEC.
      * DB接続用文字列 
       01  DSN             PIC  X(256).
      * キー識別用
       01  TOKU-COD-2  PIC  X(04).
       01  JUCHU-YY-2  PIC  9(02).
       01  JUCHU-MM-2  PIC  9(02).
       01  KINGAKU-2   PIC S9(09) PACKED-DECIMAL.

      *EXEC SQL INCLUDE USERS-DECLARE END-EXEC.
      * FETCHした情報を格納するデータ項目の定義
       EXEC SQL  INCLUDE SUMMARYIN  END-EXEC.
      
      * FETCHする対象が記載された「カーソル」の定義
       EXEC SQL  DECLARE 
           JUCHU-CURSOR CURSOR FOR
               SELECT DATA_KBN, JUCHU_YY, JUCHU_MM, TOKU_COD, KINGAKU
               FROM T_JUCHU
               ORDER BY TOKU_COD ASC, JUCHU_YY ASC, JUCHU_MM
       END-EXEC.

       EXEC SQL  END DECLARE SECTION  END-EXEC.

      * DBの状態を保存するデータ項目は DECLARE SECTION 外に記述する。
      * プリコンパイル時にエラーになる。 
       EXEC SQL  INCLUDE SQLCA  END-EXEC.

      ******************************************************************
      * メインルーチン
      ******************************************************************
       PROCEDURE               DIVISION.
           PERFORM INIT-RTN.
           PERFORM MAIN-RTN  UNTIL FETCH-END = "Y".
           PERFORM SUCCESSFUL-TERM-RTN.
           STOP RUN.

      ******************************************************************
      * 初期化処理
      ******************************************************************
       INIT-RTN                SECTION.
      * プログラムの初期化
           DISPLAY  "*** SUMMARY START ***".
           PERFORM DBCONNECT-RTN.
           PERFORM FETCH-RTN.
       EXEC SQL
           DELETE FROM T_TOKU_SUMMARY
       END-EXEC.
       EXT.
           EXIT.
         
      ******************************************************************
      * データベースへ接続する処理
      ******************************************************************
       DBCONNECT-RTN           SECTION.
      * ODBC経由で接続するための接続文字列生成
           STRING
             "DRIVER={Postgresql Unicode};"                             *> Postgresql ODBCドライバ
             "SERVER=db;"                                               *> 【要変更】サーバ名又はIPアドレス
             "DBQ=postgres;"                                            *> 【要変更】データベース名
             "UID=postgres;"                                            *> 【要変更】ユーザ名
             "PWD=postgres;"                                            *> 【要変更】パスワード
             "CONNSETTINGS=SET CLIENT_ENCODING to 'SJIS';"              *> クライアント文字コード
             INTO DSN
           END-STRING.

           EXEC SQL CONNECT TO :DSN END-EXEC.
           IF SQLCODE NOT = ZERO
             PERFORM DBERR-TERM-RTN
           END-IF.

      *    SQLカーソルを開く
           EXEC SQL  OPEN JUCHU-CURSOR  END-EXEC.
       EXT.
           EXIT.

      ******************************************************************
      * 目的のカーソル(テーブル)から1レコード読み取る
      ******************************************************************
       FETCH-RTN               SECTION.
      * カーソルでFETCHしてデータを取得する。
      * COBOLの環境によってはFETCHした情報を集団項目に転記できないため、
      * その場合はそれぞれの項目に転記するように記述する。
           EXEC SQL  
               FETCH JUCHU-CURSOR
                INTO :DATA-KBN,
                     :JUCHU-YY,
                     :JUCHU-MM,
                     :TOKU-COD,
                     :KINGAKU
           END-EXEC.

           EVALUATE SQLCODE
      *    SQLCODE = 0 ⇒ 正常終了。
             WHEN 0
               ADD 1 TO FETCH-CNT
      *    SQLCODE = 100 ⇒ これ以上データがない
             WHEN 100
               MOVE "Y" TO FETCH-END
      *    上記以外は何らかのエラー。ABEND対象。
             WHEN OTHER
               PERFORM DBERR-TERM-RTN
           END-EVALUATE.
       EXT.
           EXIT.
       
      
      ******************************************************************
      * キーの識別処理
      ******************************************************************
       MAIN-RTN       SECTION.

      * 集計処理
      *    得意先コードと受注年月チェック
           IF TOKU-COD = TOKU-COD-2
                AND (JUCHU-YY = JUCHU-YY-2
                  AND JUCHU-MM = JUCHU-MM-2) THEN
             CONTINUE
      *    得意先コードと受注年月が違えば
           ELSE
             IF TOKU-COD-2 NOT = SPACE THEN
      *      前のキーのINSERT
               PERFORM INSERT-RTN
             END-IF
      *      新しいキーをセット
               MOVE TOKU-COD             TO TOKU-COD-2
               MOVE JUCHU-YY             TO JUCHU-YY-2
               MOVE JUCHU-MM             TO JUCHU-MM-2
      *      新レコードの計算の為にゼロにする
               MOVE ZERO                 TO KINGAKU-2
           END-IF.
      *    1なら加算9なら減算
           IF DATA-KBN = "1" THEN
             ADD KINGAKU               TO KINGAKU-2
           ELSE
             SUBTRACT KINGAKU          FROM KINGAKU-2
           END-IF.

           PERFORM FETCH-RTN.
       EXT.
           EXIT.

      ******************************************************************
      * T_TOKU_SUMMARYテーブルにINSERT
      ******************************************************************
       INSERT-RTN              SECTION.
           
           EXEC SQL
             INSERT INTO T_TOKU_SUMMARY
                         (TOKU_COD, JUCHU_YY, JUCHU_MM, KINGAKU)
             VALUES (:TOKU-COD-2, :JUCHU-YY-2, :JUCHU-MM-2, :KINGAKU-2)
           END-EXEC.

           EVALUATE SQLCODE
      *    SQLCODE = 0 ⇒ 正常終了。
             WHEN 0
               ADD 1 TO INSERT-CNT
      *    上記以外は何らかのエラー。ABEND対象。
             WHEN OTHER
               PERFORM DBERR-TERM-RTN
           END-EVALUATE.
       EXT.
           EXIT.
      ******************************************************************
      * 問題なく処理が完了した時の終了処理
      ******************************************************************
       SUCCESSFUL-TERM-RTN     SECTION.
           IF TOKU-COD-2 NOT = SPACE THEN
             PERFORM INSERT-RTN
           END-IF.
           PERFORM TERM-RTN.
           EXEC SQL COMMIT END-EXEC.
       EXT.
           EXIT.

      ******************************************************************
      * 処理宙にエラーが発生した時の終了処理
      ******************************************************************
       DBERR-TERM-RTN          SECTION.
           EXEC SQL ROLLBACK END-EXEC.
           DISPLAY "SUMMARY ABEND : DATABSE ACCESS ERRROR"
           DISPLAY "SQLCODE = " SQLCODE.
           DISPLAY "SQLERRMC = " SQLERRMC.
           DISPLAY "ロールバックが発生した為レコードが追加されません"
           PERFORM TERM-RTN.
       EXT.
           EXIT.

      *****************************************************************
      * 共通のプログラム終了処理
      ******************************************************************
       TERM-RTN                SECTION.

      *    カーソルを閉じる
           EXEC SQL  CLOSE JUCHU-CURSOR  END-EXEC.
      *    DBから切断する。
           EXEC SQL  DISCONNECT ALL      END-EXEC.
           DISPLAY "FETCHCNT=" FETCH-CNT.
           DISPLAY "INSERTCNT=" INSERT-CNT.
           DISPLAY "*** SUMMARY END ***".

           STOP RUN.
