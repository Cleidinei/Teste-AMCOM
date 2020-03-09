       IDENTIFICATION DIVISION.
       PROGRAM-ID.   CADVEND.
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
           SELECT ARQ-VENDEDOR  ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS COD-VEND
                  ALTERNATE RECORD KEY IS CPF
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FSTATUS-VEND.
           SELECT IMPORT-VEND   ASSIGN TO WS-IMPORT-LABEL
                  ORGANIZATION  IS LINE SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL
                  FILE STATUS   IS FS-IMPORT-VEND.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-VENDEDOR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'CADVENDEDOR'.
       COPY "BOOKVEND.CPY".

       FD  IMPORT-VEND
           RECORD CONTAINS 100
           LABEL RECORD IS STANDARD.
       01  IMPORT-VEND-REG.
           03 IMPORT-COD-VEND      PIC  9(003).
           03 IMPORT-CPF           PIC  9(011).
           03 IMPORT-NOME-VEND     PIC  X(040).
           03 IMPORT-S-LAT         PIC  X.
           03 IMPORT-LAT           PIC  9(011).
           03 IMPORT-S-LON         PIC  X.
           03 IMPORT-LON           PIC  9(011).
      *-----------------------------------*
       WORKING-STORAGE SECTION.
      *-----------------------------------*
       77 FSTATUS-VEND             PIC X(002) VALUE "00".
       77 WS-SAIR                  PIC 9      VALUE ZEROS.
       77 WS-OPCAO                 PIC 9      VALUE ZEROS.
       77 WS-CONFIRMA              PIC X      VALUE SPACES.
       77 FS-IMPORT-VEND           PIC X(002) VALUE "00".
       77 WS-RETORNO               PIC 9(001) VALUE ZEROS.
       77 WS-CPF                   PIC 9(011) VALUE ZEROS.
       77 WS-IMPORT-LABEL          PIC X(020) VALUE SPACES.
       77 WS-MSG                   PIC X(040) VALUE SPACES.
      *-----------------------------------*
       01 WS-CONTADORES.
          03 WS-LIDOS              PIC 9(009)  VALUE ZEROS.
          03 WS-GRAVADOS           PIC 9(009)  VALUE ZEROS.
      *
       01  WS-ARQ-VEND-REG.
           03 WS-COD-VEN           PIC  9(003) VALUE ZEROS.
           03 WS-CPF-VEN           PIC  9(011) VALUE ZEROS.
           03 WS-NOME-VEN          PIC  X(040)  VALUE SPACES.
           03 WS-LATI-VEN          PIC S9(003)V9(008) VALUE ZEROS.
           03 WS-LONG-VEN          PIC S9(003)V9(008) VALUE ZEROS.
      *
      *-----------------------------------*
      *
       SCREEN SECTION.
       01 MENU.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "CADASTRO DE VENDEDORES".
          02 LINE 07 COL 10 VALUE "(1) INCLUIR".
          02 LINE 08 COL 10 VALUE "(2) ALTERAR".
          02 LINE 09 COL 10 VALUE "(3) EXCLUIR".
          02 LINE 10 COL 10 VALUE "(4) IMPORTAR".
          02 LINE 11 COL 10 VALUE "(9) VOLTAR AO MENU".
          02 LINE 15 COL 10 VALUE "OPCAO DESEJADA: (.)".
          02 LINE 15 COL 27 PIC 9 TO WS-OPCAO AUTO.
          02 LINE 19 COL 10, PIC X(040) FROM WS-MSG.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".
      *-----------------------------------*
      *
       01 INCLUSAO AUTO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "INCLUSAO DE VENDEDOR".
          02 LINE 07 COL 10 VALUE "CODIGO VENDEDOR:".
          02 LINE 07 COL 27, PIC ZZ9 TO WS-COD-VEN.
          02 LINE 08 COL 10 VALUE "CPF            :".
          02 LINE 08 COL 27, PIC 99999999999 TO WS-CPF-VEN.
          02 LINE 09 COL 10 VALUE "RAZAO SOCIAL   :".
          02 LINE 09 COL 27, PIC X(040) TO WS-NOME-VEN.
          02 LINE 10 COL 10 VALUE "LATITUDE       :".
          02 LINE 10 COL 27, PIC -ZZ9,99999999 TO WS-LATI-VEN.
          02 LINE 11 COL 10 VALUE "LONGITUDE      :".
          02 LINE 11 COL 27, PIC -ZZ9,99999999 TO WS-LONG-VEN.
          02 LINE 15 COL 10 VALUE "CONFIRMA A INCLUSAO? (S/N): (.)".
          02 LINE 15 COL 40, PIC X TO WS-CONFIRMA.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".
      *-----------------------------------*
      *
       01 ALTERACAO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "ALTERACAO DE VENDEDOR".
          02 LINE 07 COL 10 VALUE "CODIGO VENDEDOR:".
          02 LINE 07 COL 27, PIC ZZ9 FROM WS-COD-VEN.
          02 LINE 08 COL 10 VALUE "CPF            :".
          02 LINE 08 COL 27, PIC 99999999999 FROM WS-CPF-VEN AUTO.
          02 LINE 09 COL 10 VALUE "RAZAO SOCIAL   :".
          02 LINE 09 COL 27, PIC X(040) USING WS-NOME-VEN AUTO.
          02 LINE 10 COL 10 VALUE "LATITUDE       :".
          02 LINE 10 COL 27, PIC -ZZ9,99999999 USING WS-LATI-VEN
                                              AUTO.
          02 LINE 11 COL 10 VALUE "LONGITUDE      :".
          02 LINE 11 COL 27, PIC -ZZ9,99999999 USING WS-LONG-VEN
                                              AUTO.
          02 LINE 15 COL 10 VALUE "CONFIRMA A ALTERACAO? (S/N): (.)".
          02 LINE 15 COL 40, PIC X TO WS-CONFIRMA AUTO.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".
      *-----------------------------------*
      *
       01 EXCLUSAO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "EXCLUSAO DE VENDEDOR".
          02 LINE 07 COL 10 VALUE "CODIGO VENDEDOR:".
          02 LINE 07 COL 27, PIC ZZ9 FROM WS-COD-VEN.
          02 LINE 08 COL 10 VALUE "CPF            :".
          02 LINE 08 COL 27, PIC 99999999999 FROM WS-CPF-VEN.
          02 LINE 09 COL 10 VALUE "RAZAO SOCIAL   :".
          02 LINE 09 COL 27, PIC X(040) FROM WS-NOME-VEN.
          02 LINE 10 COL 10 VALUE "LATITUDE       :".
          02 LINE 10 COL 27, PIC -ZZ9,99999999 FROM WS-LATI-VEN.
          02 LINE 11 COL 10 VALUE "LONGITUDE      :".
          02 LINE 11 COL 27, PIC -ZZ9,99999999 FROM WS-LONG-VEN.
          02 LINE 15 COL 10 VALUE "CONFIRMA A EXCLUSAO? (S/N): (.)".
          02 LINE 15 COL 39, PIC X TO WS-CONFIRMA AUTO.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".
      *-----------------------------------*
      *
       01 BUSCAR AUTO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "CONSULTA PARA ALTERACAO/EXCLUSAO".
          02 LINE 07 COL 10 VALUE "CODIGO VENDEDOR: (...)".
          02 LINE 07 COL 28, PIC ZZ9 TO WS-COD-VEN.
          02 LINE 15 COL 10, PIC X(040) FROM WS-MSG.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".
      *-----------------------------------*
      *
       01 MENSAGEM AUTO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 10 COL 10 VALUE "MSG:".
          02 LINE 10 COL 15, PIC X(040) FROM WS-MSG.
          02 LINE 15 COL 10, VALUE "FAZER OUTRA CONSULTA? (S/N): (.)".
          02 LINE 15 COL 40, PIC X TO WS-CONFIRMA.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".
      *-----------------------------------*
      *
       01 IMPORTACAO AUTO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "IMPORTACAO DE VENDEDOR".
          02 LINE 07 COL 10 VALUE "INFORMAR ARQUIVO PARA IMPORTACAO:".
          02 LINE 07 COL 44, PIC X(020) TO WS-IMPORT-LABEL.
          02 LINE 15 COL 10 VALUE "CONFIRMA A IMPORTACAO? (S/N): (.)".
          02 LINE 15 COL 41, PIC X TO WS-CONFIRMA.
          02 LINE 19 COL 10 VALUE "MSG:".
          02 LINE 19 COL 15, PIC X(040) FROM WS-MSG.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".
      *-----------------------------------*
       PROCEDURE DIVISION.
      *-----------------------------------*
      *
       000-INICIO SECTION.
      *
           DISPLAY MENU
      *
           ACCEPT  MENU
      *
           EVALUATE WS-OPCAO
             WHEN 1
                PERFORM 100-INCLUSAO
             WHEN 2
                MOVE ZEROS TO WS-SAIR
                PERFORM 200-ALTERACAO
                  UNTIL WS-SAIR NOT EQUAL ZEROS
             WHEN 3
                MOVE ZEROS TO WS-SAIR
                PERFORM 300-EXCLUSAO
                  UNTIL WS-SAIR NOT EQUAL ZEROS
             WHEN 4
                PERFORM 400-IMPORTACAO
             WHEN 5
                GOBACK
           END-EVALUATE
           PERFORM 000-INICIO.
      *-----------------------------------*
      *
       100-INCLUSAO SECTION.
      *
           DISPLAY  INCLUSAO
      *
           ACCEPT   INCLUSAO
      *
           IF FUNCTION UPPER-CASE(WS-CONFIRMA) EQUAL "S"
              OPEN I-O ARQ-VENDEDOR
              MOVE WS-CPF-VEN TO WS-CPF
              MOVE WS-ARQ-VEND-REG TO ARQ-VENDEDOR-REG
              CLOSE ARQ-VENDEDOR
           END-IF

           PERFORM 000-INICIO.
      *-----------------------------------*
      *
       200-ALTERACAO.
      *
           OPEN I-O ARQ-VENDEDOR
           DISPLAY BUSCAR
      *
           ACCEPT  BUSCAR
      *
           MOVE    WS-COD-VEN    TO COD-VEND
           READ ARQ-VENDEDOR RECORD INTO WS-ARQ-VEND-REG
             KEY IS COD-VEND

           IF FSTATUS-VEND NOT EQUAL "00"
              MOVE "CODIGO DE VENDEDOR NAO LOCALIZADO"
                TO WS-MSG
              DISPLAY MENSAGEM
              ACCEPT  MENSAGEM
              IF FUNCTION UPPER-CASE(WS-CONFIRMA) EQUAL "N"
                 MOVE 9 TO WS-SAIR
              ELSE
                 MOVE "INFORMAR NOVO CODIGO PARA CONSULTA"
                   TO WS-MSG
              END-IF
           ELSE
              DISPLAY ALTERACAO
              ACCEPT  ALTERACAO

              IF FUNCTION UPPER-CASE(WS-CONFIRMA) EQUAL "S"
                 INITIALIZE ARQ-VENDEDOR-REG
                  MOVE WS-COD-VEN        TO COD-VEND
                  MOVE WS-CPF-VEN        TO CPF
                  MOVE WS-NOME-VEN       TO NOME-VENDEDOR
                  MOVE WS-LATI-VEN       TO LATITUDE-VENDEDOR
                  MOVE WS-LONG-VEN       TO LONGITUDE-VENDEDOR

                  REWRITE ARQ-VENDEDOR-REG
              ELSE
                 MOVE 9 TO WS-SAIR
              END-IF
           END-IF
           CLOSE ARQ-VENDEDOR.
      *-----------------------------------*
      *
       300-EXCLUSAO SECTION.
      *
           OPEN I-O ARQ-VENDEDOR
      *
           DISPLAY BUSCAR
      *
           ACCEPT  BUSCAR
           MOVE WS-COD-VEN TO COD-VEND

           READ ARQ-VENDEDOR RECORD INTO WS-ARQ-VEND-REG
                KEY IS COD-VEND

           IF FSTATUS-VEND NOT EQUAL "00"
              MOVE "CODIGO DE VENDEDOR NAO LOCALIZADO"
                TO WS-MSG
              DISPLAY MENSAGEM
              ACCEPT  MENSAGEM
              IF FUNCTION UPPER-CASE(WS-CONFIRMA) EQUAL "N"
                 MOVE 9 TO WS-SAIR
              ELSE
                 MOVE "INFORMAR NOVO CODIGO PARA CONSULTA"
                   TO WS-MSG
              END-IF
           ELSE
              DISPLAY EXCLUSAO
              ACCEPT  EXCLUSAO

              IF FUNCTION UPPER-CASE(WS-CONFIRMA) EQUAL "S"
                 DELETE ARQ-VENDEDOR RECORD
              ELSE
                 MOVE 9 TO WS-SAIR
              END-IF
           END-IF
           CLOSE ARQ-VENDEDOR.
      *-----------------------------------*
      *
       400-IMPORTACAO.
      *
           INITIALIZE WS-CONTADORES
      *
           DISPLAY IMPORTACAO
      *
           ACCEPT  IMPORTACAO

           MOVE SPACES TO WS-MSG
           IF FUNCTION UPPER-CASE(WS-CONFIRMA) EQUAL "S"
              IF WS-IMPORT-LABEL EQUAL SPACES
                 MOVE "INFORMAR NOME DO ARQUIVO" TO WS-MSG
                 PERFORM 400-IMPORTACAO
              END-IF
              OPEN INPUT IMPORT-VEND
              IF FS-IMPORT-VEND NOT EQUAL "00"
                 MOVE "ARQUIVO NAO LOCALIZADO" TO WS-MSG
              ELSE
                 OPEN I-O ARQ-VENDEDOR

                 PERFORM UNTIL FS-IMPORT-VEND NOT EQUAL "00"
                    READ IMPORT-VEND
                    IF FS-IMPORT-VEND EQUAL ZEROS
                       ADD 1 TO WS-LIDOS
                       MOVE IMPORT-CPF          TO WS-CPF
                       MOVE IMPORT-COD-VEND     TO COD-VEND
                       MOVE IMPORT-CPF          TO CPF
                       MOVE IMPORT-NOME-VEND    TO NOME-VENDEDOR
                       COMPUTE LATITUDE-VENDEDOR  =
                               IMPORT-LAT  / 100000000
                       COMPUTE LONGITUDE-VENDEDOR =
                               IMPORT-LON / 100000000
                       IF IMPORT-S-LAT = "-"
                          COMPUTE LATITUDE-VENDEDOR =
                                  LATITUDE-VENDEDOR * -1
                       END-IF
                       IF IMPORT-S-LON = "-"
                          COMPUTE LONGITUDE-VENDEDOR =
                                  LONGITUDE-VENDEDOR * -1
                       END-IF

                          ADD 1 TO WS-GRAVADOS
                       END-IF
                    END-PERFORM

                 STRING "LIDOS: "
                        WS-LIDOS
                        " / GRAVADOS: "
                        WS-GRAVADOS
                   INTO WS-MSG
                 CLOSE ARQ-VENDEDOR
                 CLOSE IMPORT-VEND
              END-IF
           END-IF.
      *
       END PROGRAM CADVEND.
      *-----------------------------------*
