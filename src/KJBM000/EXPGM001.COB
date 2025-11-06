      *****************************************************************
      * 見出し
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 hello.
       AUTHOR.                     HO.
       DATE-WRITTEN.               2025/10/27.
       DATE-COMPILED.              2025/10/27.
      *****************************************************************
      * 環境部
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SOURCE-COMPUTER.            MYCOMPUTER.
       OBJECT-COMPUTER.            MYCOMPUTER.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      
      * 入力ファイル
           SELECT IN-FILE          ASSIGN TO ID01
                                   ORGANIZATION IS LINE SEQUENTIAL.
      * 出力ファイル
           SELECT OUT-FILE         ASSIGN TO OD01
                                   ORGANIZATION IS LINE SEQUENTIAL.
      *****************************************************************
      * データ部
       DATA                        DIVISION.
       FILE                        SECTION.
       
      * 入力ファイルのレイアウト定義
       FD  IN-FILE.

       01  IN-RECORD.
           03  IN-SHIIRE.
               05 IN-SHIIRE-SAKI   PIC 9(005).
               05 IN-SHIIRE-DATE   PIC 9(008).
           03  IN-SHOHIN.
               05 IN-SHOHIN-CODE   PIC 9(005).
               05 IN-SHOHIN-MEI    PIC X(040).
               05 IN-SHURUI        PIC 9(003).
           03  IN-SUURYO           PIC 9(003).
           03  IN-TANKA            PIC 9(005).
           03  FILLER              PIC X(011).

      * 出力ファイルのレイアウト定義
       FD  OUT-FILE.
       01  OUT-RECORD.
           03  OUT-SHIIRE-SAKI     PIC 9(005).
           03  OUT-SHOHIN-CODE     PIC 9(005).
           03  OUT-SHOHIN-MEI      PIC X(040).
           03  OUT-SUURYO          PIC 9(003).
           03  OUT-TANKA           PIC 9(005).
           03  OUT-KINGAKU         PIC 9(008).

      * 作業領域の定義
       WORKING-STORAGE             SECTION.
       77  CST-END                 PIC X(004) VALUE "END ".

       01  WRK-WORK-AREA.
           03 WRK-AT-END           PIC X(004).
           03 WRK-IN-COUNT         PIC 9(006).
           03 WRK-OUT-COUNT        PIC 9(006).

       01  MS1-MESSAGE-AREA.
           03  FILLER              PIC X(018)
                                   VALUE "EXPGM001の処理結果".
       01  MS2-MESSAGE-AREA.
           03  FILLER              PIC X(018)
                                   VALUE"入力ファイル件数：".
           03  MSG2-COUNT          PIC ZZZ,ZZ9.
       01  MS3-MESSAGE-AREA.
           03  FILLER              PIC X(018)
                                   VALUE "出力ファイル件数：".
           03  MSG3-COUNT          PIC ZZZ,ZZ9.
      *****************************************************************
      * 手続き部
       PROCEDURE                   DIVISION.

           PERFORM INIT-PROC.
           PERFORM MAIN-PROC UNTIL WRK-AT-END = CST-END.
           PERFORM TERM-PROC.
           STOP RUN.

      * 初期処理-------------------------------------------------------
       INIT-PROC                   SECTION.

      * 作業領域の初期化 
           MOVE SPACE              TO WRK-AT-END.
           MOVE ZERO               TO WRK-IN-COUNT WRK-OUT-COUNT.

      * ファイルのオープン
           OPEN INPUT IN-FILE OUTPUT OUT-FILE.

      * 入力データの読み込み
           PERFORM INFILE-READ-PROC.
       INIT-PROC-EXIT.
           EXIT.

      * 終了処理-------------------------------------------------------
       TERM-PROC                   SECTION.

      * ファイルのクローズ
           CLOSE IN-FILE OUT-FILE.

      * 入出力件数の表示
           MOVE WRK-IN-COUNT       TO MSG2-COUNT.
           MOVE WRK-OUT-COUNT      TO MSG3-COUNT.

           DISPLAY MS1-MESSAGE-AREA UPON CONSOLE.
           DISPLAY MS2-MESSAGE-AREA UPON CONSOLE.
           DISPLAY MS3-MESSAGE-AREA UPON CONSOLE.

       TERM-PROC-EXIT.

           EXIT.

      * 主処理---------------------------------------------------------
       MAIN-PROC                   SECTION.
           
           IF IN-SHURUI >= 100 AND IN-SHURUI <= 199 THEN

               PERFORM OUTFILE-WRITE-PROC

           END-IF.

      * 入力データーの読み込み
           PERFORM INFILE-READ-PROC.

       MAIN-PROC-EXIT.

           EXIT.

      * 出力ファイルの編集・書き込み処理-------------------------------
       OUTFILE-WRITE-PROC          SECTION.

           MOVE IN-SHIIRE-SAKI     TO OUT-SHIIRE-SAKI.
           MOVE IN-SHOHIN-CODE     TO OUT-SHOHIN-CODE.
           MOVE IN-SHOHIN-MEI      TO OUT-SHOHIN-MEI.
           MOVE IN-SUURYO          TO OUT-SUURYO.
           MOVE IN-TANKA           TO OUT-TANKA.

      * 金額の計算
           COMPUTE OUT-KINGAKU = IN-SUURYO * IN-TANKA.
      
      * 出力ファイルへ書き込む
           WRITE OUT-RECORD.
      
      * 書き込み件数のアカウント
           ADD 1                   TO WRK-OUT-COUNT.

       OUTFILE-WRITE-PROC-EXIT.

           EXIT.

      * ファイル読み込み-----------------------------------------------
       INFILE-READ-PROC            SECTION.

           READ IN-FILE AT END MOVE "END"  TO WRK-AT-END

           NOT AT END ADD 1                TO WRK-IN-COUNT

           END-READ.

       INFILE-READ-PROC-EXIT.

           EXIT.
