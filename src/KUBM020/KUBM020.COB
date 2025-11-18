       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     KUBM020.
      ******************************************************************
      * システム名　　：研修
      * サブシステム名；売上
      * プログラム名  ：売上集計
      * 作成日／作成者：２０２５年１１月１３日 ホ
      * 変更日／変更者：
      * 変更内容：
      ******************************************************************
       
       ENVIRONMENT                     DIVISION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *入力ファイル
           SELECT ITF-FILE             ASSIGN TO ITF
                                       ORGANIZATION IS SEQUENTIAL
                                       FILE STATUS IS WK-INFILE-STATUS.
      *出力ファイル
           SELECT OTF-FILE             ASSIGN TO OTF
                                       ORGANIZATION IS SEQUENTIAL
                                       FILE STATUS IS WK-OUTFILE-STATUS.

       DATA                            DIVISION.
       FILE                            SECTION.

       FD  ITF-FILE.
       01  ITF-REC.
           COPY KUCF010.

       FD  OTF-FILE.
       01  OTF-REC.
           COPY KUCF020.
       
      * 作業領域の定義
       WORKING-STORAGE                 SECTION.

       01  WK-WORK-AREA.
           03 ITF-END-FLG              PIC X(001).
           03 WK-IN-CNT                PIC 9(006).
           03 WK-OUT-CNT               PIC 9(006).

           03 WK-TOKU-FLG              PIC X(004).
           03 WK-DATE-FLG              PIC 9(006).

       01  WK-MSG-ADD                  PIC X(150).

       01  WK-MESSAGE-AREA.
           03 FILLER                   PIC X(021)
                                         VALUE "*** KUBM020 START ***".
       
       01  WK-MESSAGE-AREA2.
           03 FILLER                   PIC X(021)
                                         VALUE "KUBM020 ITF=".
           03 WK-MSG2-CNT              PIC ZZZ,ZZ9.

       01  WK-MESSAGE-AREA3.
           03 FILLER                   PIC X(021)
                                         VALUE "KUBM020 OTF=".
           03 WK-MSG3-CNT              PIC ZZZ,ZZ9.

       01  WK-MESSAGE-AREA4.
           03 FILLER                   PIC X(020)
                                         VALUE "*** KUBM020 END ***".

       01  WK-MESSAGE-AREA5.
           03 FILLER                   PIC X(020)
                                         VALUE "*** KUBM020 ABEND".
           03 WK-ERROR-FLG             PIC XX VALUE ZERO.
       01  WK-INFILE-STATUS            PIC XX.
       01  WK-OUTFILE-STATUS           PIC XX.

      /

       PROCEDURE                       DIVISION.
           PERFORM INIT-RTN.
           PERFORM MAIN-RTN UNTIL ITF-END-FLG = 'Y'.
           PERFORM TERM-RTN.

      ******************************************************************
      * 初期処理
      ******************************************************************
       INIT-RTN                        SECTION.
           
           DISPLAY WK-MESSAGE-AREA.
      * 作業領域の初期化
           MOVE SPACE                  TO ITF-END-FLG
                                          UF020-TOKU-COD.
           MOVE ZERO                   TO WK-IN-CNT
                                          WK-OUT-CNT
                                          UF020-KINGAKU.

      * ファイルのオープン                                
           OPEN INPUT ITF-FILE.
           IF WK-INFILE-STATUS NOT = "00"
             DISPLAY "ITF-FILEオープンに失敗しました"
             MOVE WK-INFILE-STATUS     TO WK-ERROR-FLG
             PERFORM TERM-RTN
           END-IF.
           OPEN OUTPUT OTF-FILE.
           IF WK-OUTFILE-STATUS NOT = "00"
             DISPLAY "OTF-FILEオープンに失敗しました"
             MOVE WK-OUTFILE-STATUS    TO WK-ERROR-FLG
             PERFORM TERM-RTN
           END-IF.

      * 入力データ読み込み
           PERFORM ITFFILE-READ-RTN.
       EXT.
           EXIT.

      ******************************************************************
      * 終了処理
      ******************************************************************
       TERM-RTN                        SECTION.
      * 最終終了処理、プログラム終了する

      * 最後に残ったレコードを書き込む
           
           WRITE OTF-REC
           ADD 1                       TO WK-OUT-CNT
           
      * ファイルのクローズ
           CLOSE ITF-FILE.
           CLOSE OTF-FILE.

           IF WK-ERROR-FLG = "00"      THEN
      *      入出力件数の表示
             MOVE WK-IN-CNT            TO WK-MSG2-CNT
             MOVE WK-OUT-CNT           TO WK-MSG3-CNT

             DISPLAY WK-MESSAGE-AREA2
             DISPLAY WK-MESSAGE-AREA3
             DISPLAY WK-MESSAGE-AREA4
           ELSE
             DISPLAY WK-MESSAGE-AREA5
           END-IF.
           STOP RUN.
      ******************************************************************
      * 主処理
      ******************************************************************
       MAIN-RTN                    SECTION.

           PERFORM OUTFILE-WRITE-RTN.
      * 入力データ読み込み
           PERFORM ITFFILE-READ-RTN.
       EXT.
           EXIT.
      ******************************************************************
      * 出力ファイルの編集・書き込み処理
      ******************************************************************
       OUTFILE-WRITE-RTN           SECTION.

      * 集計処理

      *    得意先コードと受注年月チェック
           IF UF010-TOKU-COD = UF020-TOKU-COD
                AND (UF010-JUCHU-YY = UF020-JUCHU-YY
                  AND UF010-JUCHU-MM = UF020-JUCHU-MM) THEN
             CONTINUE
      *    得意先コードと受注年月が違えば
           ELSE
             IF UF020-TOKU-COD NOT = SPACE THEN
      *      前のキーの書き込み
             WRITE OTF-REC
      *      書き込み件数カウント
             ADD 1                  TO WK-OUT-CNT
             END-IF
      *      新しいキーをセット
             MOVE UF010-TOKU-COD    TO UF020-TOKU-COD
             MOVE UF010-JUCHU-YY    TO UF020-JUCHU-YY
             MOVE UF010-JUCHU-MM    TO UF020-JUCHU-MM
      *      新レコードの計算の為にゼロにする
             MOVE ZERO              TO UF020-KINGAKU
      *      1なら加算9なら減算
           END-IF.
             IF UF010-DATA-KBN = "1" THEN
               ADD UF010-KINGAKU TO UF020-KINGAKU
             ELSE
               SUBTRACT UF010-KINGAKU FROM UF020-KINGAKU
             END-IF.
       EXT.
           EXIT.
      ******************************************************************
      * ファイル読み込み処理
      ******************************************************************
       ITFFILE-READ-RTN            SECTION.

           READ ITF-FILE
           AT END MOVE "Y"         TO ITF-END-FLG
           NOT AT END ADD 1        TO WK-IN-CNT
           END-READ.

       EXT.
           EXIT.
