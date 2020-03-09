       PROGRAM-ID.   RELATCLI.
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
           SELECT ARQ-CLIENTE   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS SEQUENTIAL
                  RECORD  KEY   IS COD-CLI
                  ALTERNATE RECORD KEY IS CNPJ
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FSTATUS-CLI.
           SELECT ARQ-SORT      ASSIGN TO "RELCLIENTE.TMP"
                  FILE STATUS   IS FS-SORT.
           SELECT REL-CLIENTE ASSIGN TO "RELCLIENTE.CSV"
           ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-CLIENTE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'CADCLI'.
       COPY "BOOKCLI.CPY".

       SD  ARQ-SORT.
       01  REG-SORT.
           03 SORT-COD-CLI              PIC  9(007).
           03 SORT-CNPJ                 PIC  9(014).
           03 SORT-RZ-SOCIAL            PIC  X(040).
           03 SORT-LATI-CLIENTE         PIC S9(003)V9(008).
           03 SORT-LONG-CLI             PIC S9(003)V9(008).

       FD  REL-CLIENTE.
       01  RELAT-REG PIC X(100).
      *-----------------------------------*
       WORKING-STORAGE SECTION.
      *-----------------------------------*
       77  FSTATUS-CLI            PIC  X(002) VALUE "00".
       77  FS-SORT                PIC  X(002) VALUE "00".
      *-----------------------------------*
       01  WS-CAB                   PIC  X(100)         VALUE
           "COD CLI;CNPJ CLI;RAZAO SOCIAL;LATITUDE;LONGITUDE".
       01  WS-DET.
           03 WS-DET-COD-CLI      PIC 9(007)       VALUE ZEROS.
           03 FILLER              PIC X            VALUE ";".
           03 WS-DET-CNPJ         PIC 9(014)       VALUE ZEROS.
           03 FILLER              PIC X            VALUE ";".
           03 WS-DET-RZ-SOCIAL    PIC X(040)       VALUE SPACES.
           03 FILLER              PIC X            VALUE ";".
           03 WS-DET-LATI         PIC -ZZ9,99999999 VALUE ZEROS.
           03 FILLER              PIC X             VALUE ";".
           03 WS-DET-LONG      PIC -ZZ9,99999999 VALUE ZEROS.
      *-----------------------------------*
       LINKAGE SECTION.
       01  PARAMETROS.
           03  ORDENACAO          PIC X      VALUE SPACES.
           03  CLASSIFICA         PIC 9      VALUE ZEROS.
           03  VENDEDOR           PIC 9(003) VALUE ZEROS.
           03  CODCLI             PIC 9(007) VALUE ZEROS.
           03  RZNOME             PIC X(040) VALUE SPACES.
           03  MSG                PIC X(040) VALUE SPACES.
      *-----------------------------------*
       PROCEDURE DIVISION USING PARAMETROS.
      *-----------------------------------*
       000-INICIO SECTION.
      *
           IF ORDENACAO EQUAL "A"
              IF CLASSIFICA EQUAL 1
                 SORT ARQ-SORT
                      ON ASCENDING KEY SORT-COD-CLI
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              ELSE
                 SORT ARQ-SORT
                      ON ASCENDING KEY SORT-RZ-SOCIAL
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              END-IF
           ELSE
              IF CLASSIFICA EQUAL 1
                 SORT ARQ-SORT
                      ON DESCENDING KEY SORT-COD-CLI
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              ELSE
                 SORT ARQ-SORT
                      ON DESCENDING KEY SORT-RZ-SOCIAL
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              END-IF
           END-IF

           MOVE "RELATORIO GERADO COM SUCESSO" TO MSG.
           GOBACK.

      *-----------------------------------*
      *
       100-SORT SECTION.
      *
           OPEN INPUT ARQ-CLIENTE
      *
           READ ARQ-CLIENTE

           PERFORM 110-GERA-ARQ
             UNTIL FSTATUS-CLI NOT EQUAL "00"

           CLOSE ARQ-CLIENTE.

      *-----------------------------------*
      *
       110-GERA-ARQ.
      *
           IF CODCLI EQUAL ZEROS
              IF RZNOME EQUAL SPACES
                 RELEASE REG-SORT FROM ARQ-CLIENTE-REG
              ELSE
                 IF RAZAO-SOCIAL EQUAL RZNOME
                    RELEASE REG-SORT FROM ARQ-CLIENTE-REG
                 END-IF
              END-IF
           ELSE
              IF COD-CLI EQUAL CODCLI
                 RELEASE REG-SORT FROM ARQ-CLIENTE-REG
              END-IF
           END-IF

           READ ARQ-CLIENTE.

      *-----------------------------------*
      *
       200-RELAT SECTION.
      *
           OPEN OUTPUT REL-CLIENTE.

           RETURN ARQ-SORT.

           WRITE RELAT-REG FROM WS-CAB
           PERFORM 210-IMP-REL
             UNTIL FS-SORT NOT EQUAL "00"

           CLOSE REL-CLIENTE.

      *-----------------------------------*
      *
       210-IMP-REL.
           MOVE SORT-COD-CLI          TO WS-DET-COD-CLI
           MOVE SORT-CNPJ             TO WS-DET-CNPJ
           MOVE SORT-RZ-SOCIAL        TO WS-DET-RZ-SOCIAL
           MOVE SORT-LATI-CLIENTE     TO WS-DET-LATI
           MOVE SORT-LONG-CLI         TO WS-DET-LONG
           WRITE RELAT-REG FROM WS-DET
           RETURN ARQ-SORT

       END PROGRAM RELCLIENTE
      *-----------------------------------*
