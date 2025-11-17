       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         TRZ2P.

       ENVIRONMENT         DIVISION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      * 入力側は行順ファイルが取り扱いしやすい
           SELECT ITF-FILE ASSIGN TO ITF
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OTF-FILE ASSIGN TO OTF
               ORGANIZATION SEQUENTIAL.
      
       DATA                DIVISION.
       FILE                SECTION.
       FD ITF-FILE.
       01  ITF-REC.
           03  UF010-DATA-KBN              PIC  X(01).
           03  UF010-TOKU-COD              PIC  X(04).
           03  UF010-JUCHU-DATE.
             05  UF010-JUCHU-YY            PIC  9(04).
             05  UF010-JUCHU-MM            PIC  9(02).
             05  UF010-JUCHU-DD            PIC  9(02).
           03  UF010-JUCHU-NO              PIC  9(04).
           03  UF010-SHOHIN-NO             PIC  X(05).
           03  UF010-TOKU-MEI              PIC  X(20).
           03  UF010-SHOHIN-MEI            PIC  X(20).
           03  UF010-TANKA                 PIC S9(05)V9(2).
           03  UF010-SURYO                 PIC S9(05).
           03  UF010-KINGAKU               PIC S9(09).
           03  FILLER                      PIC  X(26).
           
       FD OTF-FILE.
       01  OTF-REC.
           COPY KUCF010.

       WORKING-STORAGE      SECTION.
       01  ITF-END-FLG     PIC   X     VALUE  'N'.
       01  OTF-CNT         PIC   9(003).

       PROCEDURE           DIVISION.
           OPEN  INPUT   ITF-FILE.
           OPEN  OUTPUT  OTF-FILE.

           PERFORM  UNTIL  ITF-END-FLG  =  "Y"
             READ  ITF-FILE
               AT  END
                 MOVE  "Y"  TO  ITF-END-FLG
               NOT  AT  END
                 MOVE CORR ITF-REC  TO  OTF-REC
                 WRITE  OTF-REC
                 ADD 1 TO OTF-CNT

             END-READ
           END-PERFORM.
           
           DISPLAY OTF-CNT.
           CLOSE  OTF-FILE.
           CLOSE  ITF-FILE.
      
           STOP RUN.
