       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 KJBM020.
      ******************************************************************
      * システム名　　：研修
      * サブシステム名；受注
      * プログラム名  ：受注データ形式チェック
      * 作成日／作成者：２０２５年１１月１０日 ホ
      * 変更日／変更者：
      * 変更内容：
      ******************************************************************
       
       ENVIRONMENT                 DIVISION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *入力ファイル
           SELECT ITF-FILE         ASSIGN TO ITF
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WK-INFILE-STATUS.
      *出力ファイル
           SELECT OTF-FILE         ASSIGN TO OTF
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WK-OUTFILE-STATUS.

       DATA                        DIVISION.
       FILE                        SECTION.
           
       FD  ITF-FILE.
       01  ITF-REC.
           COPY KJCF020.

       FD  OTF-FILE.
       01  OTF-REC.
           COPY KJCF020.
       
      * 作業領域の定義
       WORKING-STORAGE             SECTION.

       01  WK-WORK-AREA.
           03 ITF-END-FLG          PIC X(001).
           03 WK-IN-CNT            PIC 9(006).
           03 WK-OUT-CNT           PIC 9(006).
       01  WK-MSG-ADD              PIC X(150).
       01  WK-LF                   PIC X
                                     VALUE X'0A'.
       01  WK-MESSAGE-AREA.
           03 FILLER               PIC X(021)
                                     VALUE "*** KJBM020 START ***".
       
       01  WK-MESSAGE-AREA2.
           03 FILLER               PIC X(021)
                                     VALUE "KJBM020 ITF=".
           03 WK-MSG2-CNT          PIC ZZZ,ZZ9.

       01  WK-MESSAGE-AREA3.
           03 FILLER               PIC X(021)
                                     VALUE "KJBM020 OTF=".
           03 WK-MSG3-CNT          PIC ZZZ,ZZ9.

       01  WK-MESSAGE-AREA4.
           03 FILLER               PIC X(020)
                                     VALUE "*** KJBM020 END ***".

       01  WK-MESSAGE-AREA5.
           03 FILLER               PIC X(020)
                                     VALUE "*** KJBM020 ABEND".
           03 WK-ERROR-FLG         PIC XX VALUE ZERO.
       03  WK-INFILE-STATUS        PIC X(002).
       03  WK-OUTFILE-STATUS       PIC X(002).

           COPY KCBS010P.
      /

       PROCEDURE                   DIVISION.
           PERFORM INIT-RTN.
           PERFORM MAIN-RTN UNTIL ITF-END-FLG = 'Y'.
           PERFORM TERM-RTN.

      ******************************************************************
      * 初期処理
      ******************************************************************
       INIT-RTN                    SECTION.
           
           DISPLAY WK-MESSAGE-AREA.
      * 作業領域の初期化
           MOVE SPACE              TO ITF-END-FLG.
           MOVE ZERO               TO WK-IN-CNT
                                      WK-OUT-CNT.
      * ファイルのオープン                                
           OPEN INPUT ITF-FILE.
           IF WK-INFILE-STATUS NOT = "00"
             DISPLAY "ITF-FILEオープンに失敗しました"
             MOVE WK-INFILE-STATUS TO WK-ERROR-FLG
             PERFORM TERM-RTN
           END-IF.
           OPEN OUTPUT OTF-FILE.
           IF WK-OUTFILE-STATUS NOT = "00"
             DISPLAY "OTF-FILEオープンに失敗しました"
             MOVE WK-OUTFILE-STATUS TO WK-ERROR-FLG
             PERFORM TERM-RTN
           END-IF.
      * 入力データ読み込み
           PERFORM ITFFILE-READ-RTN.
       EXT.
           EXIT.

      ******************************************************************
      * 終了処理
      ******************************************************************
       TERM-RTN                    SECTION.
      * 最終終了処理、プログラム終了する

      * ファイルのクローズ
           CLOSE ITF-FILE.
           CLOSE OTF-FILE.

           IF WK-ERROR-FLG = "00" THEN
      *      入出力件数の表示
             MOVE WK-IN-CNT        TO WK-MSG2-CNT
             MOVE WK-OUT-CNT       TO WK-MSG3-CNT

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

           MOVE SPACE              TO OTF-REC.
           MOVE ITF-REC            TO OTF-REC.

           MOVE JF020-JUCHU-DATE OF ITF-REC
                                   TO S010-DATE.

      * データ区分チェック
           IF JF020-DATA-KBN OF ITF-REC NOT = "1" AND
                JF020-DATA-KBN OF ITF-REC NOT = "9" THEN
             MOVE "1"              TO JF020-ERR-KBN OF OTF-REC (1)
           END-IF.
      * 受注番号チェック
           IF JF020-JUCHU-NO OF ITF-REC IS NOT NUMERIC
                OR JF020-JUCHU-NO OF ITF-REC = ZERO THEN
             MOVE "1"              TO JF020-ERR-KBN OF OTF-REC (2)
           END-IF.
      * 受注日付チェック
           MOVE JF020-JUCHU-DATE OF ITF-REC
                                   TO S010-DATE.
           CALL "KCBS010"          USING REFERENCE KCBS010-P1.
                                   
           IF S010-RCD = "E" THEN
             MOVE "1"              TO JF020-ERR-KBN OF OTF-REC (3)
           ELSE
             MOVE S010-DATE        TO JF020-JUCHU-DATE OF OTF-REC
           END-IF.
      * 得意先コードチェック
           IF JF020-TOKU-COD OF ITF-REC = SPACE THEN
             MOVE "1"              TO JF020-ERR-KBN OF OTF-REC (4)
           END-IF.
      * 商品番号チェック
           IF JF020-SHOHIN-NO OF ITF-REC IS NOT NUMERIC
                OR JF020-SHOHIN-NO OF ITF-REC = ZERO THEN
             MOVE "1"              TO JF020-ERR-KBN OF OTF-REC (5)
           END-IF.
      * 数量チェック
           IF JF020-SURYO OF ITF-REC IS NOT NUMERIC
                OR JF020-SURYO OF ITF-REC < 1 OR
                  JF020-SURYO OF ITF-REC > 999 THEN
             MOVE "1"              TO JF020-ERR-KBN OF OTF-REC (6)
           END-IF.

      * 出力ファイルへ書き込む
           WRITE OTF-REC.
           IF WK-OUTFILE-STATUS NOT = "00"
             DISPLAY "OTF-FILE書き込みに失敗しました"
             MOVE WK-OUTFILE-STATUS TO WK-ERROR-FLG
             PERFORM TERM-RTN
           END-IF.
      * 書き込み件数カウント
           ADD 1                   TO WK-OUT-CNT.
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
