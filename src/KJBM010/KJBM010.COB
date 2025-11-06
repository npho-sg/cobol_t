       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 KJBM010.
      ******************************************************************
      * システム名　　：研修
      * サブシステム名；受注
      * プログラム名  ：受注チェックファイル作成
      * 作成日／作成者：１９９９年１２月９日 山田 太郎
      * 変更日／変更者：
      * 変更内容：
      ******************************************************************

       ENVIRONMENT                 DIVISION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *入力ファイル
           SELECT ITF-FILE         ASSIGN TO ITF
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS IS WK-INFILE-STATUS.
      *出力ファイル
           SELECT OTF-FILE         ASSIGN TO OTF
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WK-OUTFILE-STATUS.

       DATA                        DIVISION.
       FILE                        SECTION.
           
       FD  ITF-FILE.
       01  ITF-REC.
           COPY KJCF010.COB.

       FD  OTF-FILE.
       01  OTF-REC.
           COPY KJCF020.COB.
       
      * 作業領域の定義
       WORKING-STORAGE             SECTION.

       01  WK-WORK-AREA.
           03 ITF-END-FLG         PIC X(001).
           03 WK-IN-CNT           PIC 9(006).
           03 WK-OUT-CNT          PIC 9(006).
       01 WK-MSG-ADD              PIC X(150).
       01 WK-LF                   PIC X
                                  VALUE X'0A'.
       
       01  WK-MESSAGE-AREA.
           03 FILLER               PIC X(021)
                                 VALUE "*** KJBM010 START ***".
       
       01  WK-MESSAGE-AREA2.
           03 FILLER               PIC X(021)
                                 VALUE "KJBM010 ITF=".
           03 WK-MSG2-CNT             PIC ZZZ,ZZ9.

       01  WK-MESSAGE-AREA3.
           03 FILLER                  PIC X(021)
                                 VALUE "KJBM010 OTF=".
           03 WK-MSG3-CNT             PIC ZZZ,ZZ9.

       01  WK-MESSAGE-AREA4.
           03 FILLER               PIC X(020)
                                 VALUE "*** KJBM010 END ***".

       01  WK-MESSAGE-AREA5.
           03 FILLER               PIC X(020)
                                 VALUE "*** KJBM010 ABEND".
           03 WK-ERROR-MSG         PIC X(150).

       01  WK-INFILE-STATUS        PIC XX.
       01  WK-OUTFILE-STATUS       PIC XX.
      /
       PROCEDURE                   DIVISION.
           PERFORM INIT-RTN.
           PERFORM MAIN-RTN UNTIL ITF-END-FLG = 'Y'.
           PERFORM TERM-RTN.
           STOP RUN.

      ******************************************************************
      * 初期処理
      ******************************************************************
       INIT-RTN                    SECTION.
           
           DISPLAY WK-MESSAGE-AREA UPON CONSOLE.
      * 作業領域の初期化
           MOVE SPACE              TO ITF-END-FLG.
           MOVE ZERO               TO WK-IN-CNT
                                      WK-OUT-CNT.
      * ファイルのオープン                                
           OPEN INPUT ITF-FILE
           IF WK-INFILE-STATUS NOT = "00"
                DISPLAY "ITF-FILE OPEN FAIL" 
                   UPON CONSOLE
                DISPLAY WK-INFILE-STATUS
                   UPON CONSOLE
                STOP RUN
           END-IF
           OPEN OUTPUT OTF-FILE
           IF WK-OUTFILE-STATUS NOT = "00"
                DISPLAY "OTF-FILE OPEN FAIL"
                   UPON CONSOLE
                DISPLAY WK-OUTFILE-STATUS
                   UPON CONSOLE
                CLOSE ITF-FILE
                STOP RUN
           END-IF
      * 入力データ読み込み
           PERFORM ITFFILE-READ-RTN.
       EXT.
           EXIT.

      ******************************************************************
      * 終了処理
      ******************************************************************
       TERM-RTN                    SECTION.

      * ファイルのクローズ
           CLOSE ITF-FILE.
           IF WK-INFILE-STATUS NOT = "00"
                DISPLAY "ITF-FAIL CLOSE FAIL"
                       UPON CONSOLE
                DISPLAY WK-INFILE-STATUS
                   UPON CONSOLE
                CLOSE OTF-FILE
                STOP RUN
       
           END-IF
           CLOSE OTF-FILE.
           IF WK-OUTFILE-STATUS NOT = "00"
                DISPLAY "OTF-FAIL CLOSE FAIL"
                   UPON CONSOLE
                DISPLAY WK-OUTFILE-STATUS
                   UPON CONSOLE
                CLOSE ITF-FILE
                STOP RUN
           END-IF
       
      *      入出力件数の表示
             MOVE WK-IN-CNT          TO WK-MSG2-CNT.
             MOVE WK-OUT-CNT         TO WK-MSG3-CNT.

             DISPLAY WK-MESSAGE-AREA2 UPON CONSOLE.
             DISPLAY WK-MESSAGE-AREA3 UPON CONSOLE.
             DISPLAY WK-MESSAGE-AREA4 UPON CONSOLE.
       EXT.
           EXIT.

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
       OUTFILE-WRITE-RTN          SECTION.
           
           MOVE SPACE              TO OTF-REC.

           MOVE JF010-DATA-KBN     TO JF020-DATA-KBN.
           MOVE JF010-JUCHU-NO-X   TO JF020-JUCHU-NO-X.
           MOVE 0                  TO JF020-JUCHU-Y1.
           MOVE JF010-JUCHU-DATE   TO JF020-JUCHU-DATE6.
      *     MOVE JF010-JUCHU-YY     TO JF020-JUCHU-Y2.
      *     MOVE JF010-JUCHU-MM     TO JF020-JUCHU-MM.
      *     MOVE JF010-JUCHU-DD     TO JF020-JUCHU-DD.
           MOVE JF010-TOKU-COD     TO JF020-TOKU-COD.
           MOVE JF010-SHOHIN-NO    TO JF020-SHOHIN-NO.
           MOVE JF010-SURYO-X      TO JF020-SURYO-X.
           MOVE SPACE              TO JF020-ERR-KBN-TBL.
           MOVE SPACE              TO JF020-TOKU-MEI.
           MOVE SPACE              TO JF020-SHOHIN-MEI.
           MOVE 0                  TO JF020-TANKA.
           MOVE 0                  TO JF020-KINGAKU.

      * 出力ファイルへ書き込む
           WRITE OTF-REC.
           IF WK-OUTFILE-STATUS NOT = "00"
                DISPLAY "OTF-FILE WRITE FAIL"
                   UPON CONSOLE
                DISPLAY WK-OUTFILE-STATUS
                   UPON CONSOLE
                CLOSE ITF-FILE
                CLOSE OTF-FILE
                STOP RUN
           END-IF
      * 書き込み件数カウント
           ADD 1                   TO WK-OUT-CNT.
       EXT.
           EXIT.

      ******************************************************************
      * ファイル読み込み処理
      ******************************************************************
       ITFFILE-READ-RTN            SECTION.

           READ ITF-FILE AT END MOVE "Y" TO ITF-END-FLG
           NOT AT END ADD 1                TO WK-IN-CNT
           END-READ.
           IF WK-INFILE-STATUS NOT = "00"
              AND WK-INFILE-STATUS NOT = "10"
                DISPLAY "ITF-FILE READ FAIL"
                   UPON CONSOLE
                DISPLAY WK-INFILE-STATUS
                   UPON CONSOLE
                CLOSE ITF-FILE OTF-FILE
                STOP RUN
           END-IF
           EXIT.
