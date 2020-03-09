       PROGRAM-ID.   RELATVEND.
       AUTHOR. CLEIDINEI.
       DATE-WRITTEN.  08 MARCO 2020.
      *
      *-----------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-VENDEDOR   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS SEQUENTIAL
                  RECORD  KEY   IS COD-VEND
                  ALTERNATE RECORD KEY IS CPF
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FSTATUS-VEND.
           SELECT ARQ-SORT      ASSIGN TO "RELATVEND.TMP"
                  FILE STATUS   IS FS-SORT.
           SELECT REL-VENDEDOR ASSIGN TO "RELATVEND.CSV"
           ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-VENDEDOR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'BOOKVEND'.
       COPY "BOOKVEND.CPY".
       SD  ARQ-SORT.
       01  REG-SORT.
           03 SORT-COD-VEND         PIC  9(003).
           03 SORT-CPF                  PIC  9(011).
           03 SORT-NOME-VEND        PIC  X(040).
           03 SORT-LATI             PIC S9(003)V9(008).
           03 SORT-LONGI            PIC S9(003)V9(008).

       FD  REL-VENDEDOR.
       01	REL-REGISTRO PIC X(100).
      *-----------------------------------*
       WORKING-STORAGE SECTION.
      *-----------------------------------*
       77  FSTATUS-VEND           PIC  X(002)      VALUE "00".
       77  FS-SORT                PIC  X(002)      VALUE "00".
      *-----------------------------------*
       01  WS-CAB                   PIC  X(100)    VALUE
           "COD VEND;CPF VEND;NOME VEND;LATITUDE;LONGITUDE".
       01  WS-DET.
           03 WS-DET-COD-VEND     PIC 9(003)       VALUE ZEROS.
           03 FILLER              PIC X            VALUE ";".
           03 WS-DET-CPF          PIC 9(011)       VALUE ZEROS.
           03 FILLER              PIC X            VALUE ";".
           03 WS-DET-NOME-VEND    PIC X(040)       VALUE SPACES.
           03 FILLER              PIC X            VALUE ";".
           03 WS-DET-LATI         PIC -ZZ9,99999999 VALUE ZEROS.
           03 FILLER              PIC X             VALUE ";".
           03 WS-DET-LONGI      PIC -ZZ9,99999999   VALUE ZEROS.
      *-----------------------------------*
       LINKAGE SECTION.
       01  PARAMETROS.
           03  ORDENACAO         PIC X       VALUE SPACES.
           03  CLASSIFICA        PIC 9       VALUE ZEROS.
           03  VENDEDOR          PIC 9(003)  VALUE ZEROS.
           03  CODCLI            PIC 9(007)  VALUE ZEROS.
           03  RZNOME            PIC X(040)  VALUE SPACES.
           03  MSG               PIC X(040)  VALUE SPACES.
      *-----------------------------------*
       PROCEDURE DIVISION USING PARAMETROS.
      *-----------------------------------*
      *
       000-INICIO.
           IF ORDENACAO EQUAL "A"
              IF CLASSIFICA EQUAL 1
                 SORT ARQ-SORT
                      ON ASCENDING KEY SORT-COD-VEND
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              ELSE
                 SORT ARQ-SORT
                      ON ASCENDING KEY SORT-NOME-VEND
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              END-IF
           ELSE
              IF CLASSIFICA EQUAL 1
                 SORT ARQ-SORT
                      ON DESCENDING KEY SORT-COD-VEND
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              ELSE
                 SORT ARQ-SORT
                      ON DESCENDING KEY SORT-NOME-VEND
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              END-IF
           END-IF

           MOVE "RELATORIO GERADO COM SUCESSO" TO MSG.
           GOBACK.

      *-----------------------------------*
      *
       100-SORT.
           OPEN INPUT ARQ-VENDEDOR

           READ ARQ-VENDEDOR

           PERFORM 110-GERA-ARQ
             UNTIL FSTATUS-VEND NOT EQUAL "00"

           CLOSE ARQ-VENDEDOR.

      *-----------------------------------*
      *
       110-GERA-ARQ.
           IF VENDEDOR EQUAL ZEROS
              IF RZNOME EQUAL SPACES
                 RELEASE REG-SORT FROM ARQ-VENDEDOR-REG
              ELSE
                 IF NOME-VENDEDOR EQUAL RZNOME
                    RELEASE REG-SORT FROM ARQ-VENDEDOR-REG
                 END-IF
              END-IF
           ELSE
              IF COD-VEND EQUAL VENDEDOR
                 RELEASE REG-SORT FROM ARQ-VENDEDOR-REG
              END-IF
           END-IF

           READ ARQ-VENDEDOR.

      *-----------------------------------*
      *
       200-RELAT SECTION.

           OPEN OUTPUT REL-VENDEDOR

           RETURN ARQ-SORT

           WRITE REL-REGISTRO FROM WS-CAB
           PERFORM 210-IMP-REL
             UNTIL FS-SORT NOT EQUAL "00"

           CLOSE REL-VENDEDOR.
      *-----------------------------------*
      *
       210-IMP-REL.
           MOVE SORT-COD-VEND          TO WS-DET-COD-VEND
           MOVE SORT-CPF               TO WS-DET-CPF
           MOVE SORT-NOME-VEND         TO WS-DET-NOME-VEND
           MOVE SORT-LATI              TO WS-DET-LATI
           MOVE SORT-LONGI             TO WS-DET-LONGI
           WRITE REL-REGISTRO FROM WS-DET
           RETURN ARQ-SORT

       END PROGRAM RELATVEND.
      *-----------------------------------*
      *
