       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     KJBM050.
      ******************************************************************
      * システム名　　：研修
      * サブシステム名；受注
      * プログラム名  ：受注データ振り分け
      * 作成日／作成者：２０２５年１１月１２日 ホ
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
           SELECT OTF2-FILE            ASSIGN TO OTF2
                                       ORGANIZATION IS SEQUENTIAL
                                       FILE STATUS IS
                                         WK-OUTFILE2-STATUS.

       DATA                            DIVISION.
       FILE                            SECTION.

       FD  ITF-FILE.
       01  ITF-REC.
           COPY KJCF020.

       FD  OTF-FILE.
       01  OTF-REC.
           COPY KJCF020.
       FD  OTF2-FILE.
       01  OTF2-REC.
           COPY KJCF020.
       
      * 作業領域の定義
       WORKING-STORAGE                 SECTION.

       01  WK-WORK-AREA.
           03 ITF-END-FLG              PIC X(001).
           03 WK-IN-CNT                PIC 9(006).
           03 WK-OUT-CNT               PIC 9(006).
           03 WK-OUT2-CNT              PIC 9(006).
       01  WK-MSG-ADD                  PIC X(150).

       01  WK-MESSAGE-AREA.
           03 FILLER                   PIC X(021)
                                         VALUE "*** KJBM050 START ***".
       
       01  WK-MESSAGE-AREA2.
           03 FILLER                   PIC X(021)
                                         VALUE "KJBM050 ITF=".
           03 WK-MSG2-CNT              PIC ZZZ,ZZ9.

       01  WK-MESSAGE-AREA3.
           03 FILLER                   PIC X(021)
                                         VALUE "KJBM050 OTF=".
           03 WK-MSG3-CNT              PIC ZZZ,ZZ9.

           03 FILLER                   PIC X(001)
                                         VALUE X"0A".       

           03 FILLER                   PIC X(021)
                                         VALUE "KJBM050 OTF2=".
           03 WK-MSG3-2-CNT            PIC ZZZ,ZZ9.

       01  WK-MESSAGE-AREA4.
           03 FILLER                   PIC X(020)
                                         VALUE "*** KJBM050 END ***".

       01  WK-MESSAGE-AREA5.
           03 FILLER                   PIC X(020)
                                         VALUE "*** KJBM050 ABEND".
           03 WK-ERROR-FLG             PIC XX VALUE ZERO.
       01  WK-INFILE-STATUS            PIC XX.
       01  WK-OUTFILE-STATUS           PIC XX.
       01  WK-OUTFILE2-STATUS          PIC XX.

           COPY KCBS010P.
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
           MOVE SPACE                  TO ITF-END-FLG.
           MOVE ZERO                   TO WK-IN-CNT
                                          WK-OUT-CNT
                                          WK-OUT2-CNT.
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
           OPEN OUTPUT OTF2-FILE.
           IF WK-OUTFILE2-STATUS NOT = "00"
             DISPLAY "OTF-FIEL-2オープンに失敗しました"
             MOVE WK-OUTFILE2-STATUS  TO WK-ERROR-FLG
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

      * ファイルのクローズ
           CLOSE ITF-FILE.
           CLOSE OTF-FILE.
           CLOSE OTF2-FILE.

           IF WK-ERROR-FLG = "00"      THEN
      *      入出力件数の表示
             MOVE WK-IN-CNT            TO WK-MSG2-CNT
             MOVE WK-OUT-CNT           TO WK-MSG3-CNT
             MOVE WK-OUT2-CNT          TO WK-MSG3-2-CNT

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
           MOVE SPACE              TO OTF2-REC.

      * エラー区分テーブルチェック
           IF JF020-ERR-KBN-TBL OF ITF-REC = SPACE
             MOVE ITF-REC          TO OTF-REC
      *      出力ファイルへ書き込む
             WRITE OTF-REC
      *      書き込み件数カウント
             ADD 1                 TO WK-OUT-CNT
           ELSE
             MOVE ITF-REC          TO OTF2-REC
      *      出力ファイルへ書き込む
             WRITE OTF2-REC
      *      書き込み件数カウント
             ADD 1                 TO WK-OUT2-CNT
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
