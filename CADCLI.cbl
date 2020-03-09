       IDENTIFICATION DIVISION.
       PROGRAM-ID.   CADCLI.
       AUTHOR. CLEIDINEI.
       DATE-WRITTEN.  08 MARCO 2020.
      *
      *-----------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-CLIENTE   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS COD-CLI
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FSTATUS-CLI.
           SELECT IMPORT-CLI   ASSIGN TO WS-IMPORT-LABEL
                  ORGANIZATION  IS LINE SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL
                  FILE STATUS   IS FS-IMPORT-CLI.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-CLIENTE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'BOOKCLI'.
       COPY "BOOKCLI.CPY".
      *
       FD  IMPORT-CLI
           RECORD CONTAINS 100
           LABEL RECORD IS STANDARD.
       01  IMPORT-CLI-REG.
           03 IMPORT-COD-CLI      PIC  9(007).
           03 IMPORT-CNPJ         PIC  9(014).
           03 IMPORT-RZ-SOCIAL    PIC  X(040).
  *****    03 IMPORT-S-LAT        PIC  X.
           03 IMPORT-LAT          PIC  9(011).
  *****    03 IMPORT-S-LON        PIC  X.
  *****    03 IMPORT-LON          PIC  9(011).
      *-----------------------------------*
       WORKING-STORAGE SECTION.
      *-----------------------------------*
       77 FSTATUS-CLI             PIC X(002) VALUE "00".
       77 WS-SAIR                 PIC 9      VALUE ZEROS.
       77 WS-OPCAO                PIC 9      VALUE ZEROS.
       77 WS-CONFIRMA             PIC X      VALUE SPACES.
       77 FS-IMPORT-CLI           PIC X(002) VALUE "00".
       77 WS-RETORNO              PIC 9(001) VALUE ZEROS.
       77 WS-CNPJ                 PIC 9(014) VALUE ZEROS.
       77 WS-IMPORT-LABEL         PIC X(020) VALUE SPACES.
       77 WS-MSG                  PIC X(040) VALUE SPACES.
      *-----------------------------------*
       01 WS-CONTADORES.
          03 WS-LIDOS             PIC 9(009) VALUE ZEROS.
          03 WS-GRAVADOS          PIC 9(009) VALUE ZEROS.
      *
       01  WS-ARQ-CLI-REG.
           03 WS-COD-CLI          PIC  9(007) VALUE ZEROS.
           03 WS-CNPJ-CLI         PIC  9(014) VALUE ZEROS.
           03 WS-RZ-SOCIAL        PIC  X(040) VALUE SPACES.
           03 WS-LATI-CLI         PIC S9(003)V9(008) VALUE ZEROS.
           03 WS-LONG-CLI         PIC S9(003)V9(008) VALUE ZEROS.
      *
      *-----------------------------------*
      *
       SCREEN SECTION.
       01 MENU.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "CADASTRO DE CLIENTES - MENU".
          02 LINE 07 COL 10 VALUE "(1) INCLUIR".
          02 LINE 08 COL 10 VALUE "(2) ALTERAR".
          02 LINE 09 COL 10 VALUE "(3) EXCLUIR".
          02 LINE 10 COL 10 VALUE "(4) IMPORTAR".
          02 LINE 11 COL 10 VALUE "(9) VOLTAR AO MENU".
          02 LINE 15 COL 10 "OPCAO DESEJADA: (.)".
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
          02 LINE 05 COL 10 VALUE "INCLUSAO DE CLIENTE".
          02 LINE 07 COL 10 VALUE "CODIGO CLIENTE:".
          02 LINE 07 COL 26, PIC ZZZZZZ9 TO WS-COD-CLI.
          02 LINE 08 COL 10 VALUE "CNPJ          :".
          02 LINE 08 COL 26, PIC 99999999999999 TO WS-CNPJ-CLI.
          02 LINE 09 COL 10 VALUE "RAZAO SOCIAL  :".
          02 LINE 09 COL 26, PIC X(040) TO WS-RZ-SOCIAL.
          02 LINE 10 COL 10 VALUE "LATITUDE      :".
          02 LINE 10 COL 26, PIC -ZZ9,99999999 TO WS-LATI-CLI.
          02 LINE 11 COL 10 VALUE "LONGITUDE     :".
          02 LINE 11 COL 26, PIC -ZZ9,99999999 TO WS-LONG-CLI.
          02 LINE 15 COL 10 "CONFIRMA A INCLUSAO? (S/N): (.)".
          02 LINE 15 COL 40, PIC X TO WS-CONFIRMA.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".
      *-----------------------------------*
      *
       01 ALTERACAO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "ALTERACAO DE CLIENTE".
          02 LINE 07 COL 10 VALUE "CODIGO CLIENTE:".
          02 LINE 07 COL 26, PIC ZZZZZZ9 FROM WS-COD-CLI.
          02 LINE 08 COL 10 VALUE "CNPJ          :".
          02 LINE 08 COL 26, PIC 99999999999999 FROM WS-CNPJ-CLI AUTO.
          02 LINE 09 COL 10 VALUE "RAZAO SOCIAL  :".
          02 LINE 09 COL 26, PIC X(040) USING WS-RZ-SOCIAL AUTO.
          02 LINE 10 COL 10 VALUE "LATITUDE      :".
          02 LINE 10 COL 26, PIC -ZZ9,99999999 USING WS-LATI-CLI
                                              AUTO.
          02 LINE 11 COL 10 VALUE "LONGITUDE     :".
          02 LINE 11 COL 26, PIC -ZZ9,99999999 USING WS-LONG-CLI
                                              AUTO.
          02 LINE 15 COL 10 "CONFIRMA A ALTERACAO? (S/N): (.)".
          02 LINE 15 COL 40, PIC X TO WS-CONFIRMA AUTO.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".
      *-----------------------------------*
      *
       01 EXCLUSAO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "EXCLUSAO DE CLIENTE".
          02 LINE 07 COL 10 VALUE "CODIGO CLIENTE:".
          02 LINE 07 COL 26, PIC ZZZZZZ9 FROM WS-COD-CLI.
          02 LINE 08 COL 10 VALUE "CNPJ          :".
          02 LINE 08 COL 26, PIC 99999999999999 FROM WS-CNPJ-CLI.
          02 LINE 09 COL 10 VALUE "RAZAO SOCIAL  :".
          02 LINE 09 COL 26, PIC X(040) FROM WS-RZ-SOCIAL.
          02 LINE 10 COL 10 VALUE "LATITUDE      :".
          02 LINE 10 COL 26, PIC -ZZ9,99999999 FROM WS-LATI-CLI.
          02 LINE 11 COL 10 VALUE "LONGITUDE     :".
          02 LINE 11 COL 26, PIC -ZZ9,99999999 FROM WS-LONG-CLI.
          02 LINE 15 COL 10 "CONFIRMA A EXCLUSAO? (S/N): (.)".
          02 LINE 15 COL 39, PIC X TO WS-CONFIRMA AUTO.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".
      *-----------------------------------*
      *
       01 BUSCAR AUTO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "CLIENTE PARA - ALTERACAO/EXCLUSAO".
          02 LINE 07 COL 10 VALUE "CODIGO CLIENTE: (.......)".
          02 LINE 07 COL 27, PIC ZZZZZZ9 TO WS-COD-CLI.
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
          02 LINE 15 COL 10, "FAZER OUTRA CONSULTA? (S/N): (.)".
          02 LINE 15 COL 40, PIC X TO WS-CONFIRMA.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".
      *-----------------------------------*
      *
       01 IMPORTACAO AUTO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "IMPORTACAO DE CLIENTE".
          02 LINE 07 COL 10 VALUE "INFORME ARQUIVO PARA IMPORTACAO:".
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
              OPEN I-O ARQ-CLIENTE
              MOVE WS-CNPJ-CLI TO WS-CNPJ
              MOVE WS-ARQ-CLI-REG TO ARQ-CLI-REG
              WRITE ARQ-CLI-REG
              CLOSE ARQ-CLIENTE
           END-IF

           PERFORM 000-INICIO.

      *-----------------------------------*
      *
       200-ALTERACAO SECTION.
      *
           OPEN I-O ARQ-CLIENTE
           DISPLAY BUSCAR
      *
           ACCEPT  BUSCAR
      *
           MOVE    WS-COD-CLI    TO COD-CLI
           READ ARQ-CLIENTE RECORD INTO WS-ARQ-CLI-REG
             KEY IS COD-CLI

           IF FSTATUS-CLI NOT EQUAL "00"
              MOVE "CODIGO CLIENTE NAO ENCONTRADO"
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
                 INITIALIZE ARQ-CLI-REG
                  MOVE WS-COD-CLI        TO COD-CLI
                  MOVE WS-CNPJ-CLI       TO CNPJ-CLI
                  MOVE WS-RZ-SOCIAL      TO RAZAO-SOCIAL
                  MOVE WS-LATI-CLI       TO LATI-CLIENTE
                  MOVE WS-LONG-CLI       TO LONGI-CLIENTE

                  REWRITE ARQ-CLI-REG
              ELSE
                 MOVE 9 TO WS-SAIR
              END-IF
           END-IF
           CLOSE ARQ-CLIENTE.

      *-----------------------------------*
      *
       300-EXCLUSAO SECTION.
      *
           OPEN I-O ARQ-CLIENTE
      *
           DISPLAY BUSCAR
      *
           ACCEPT  BUSCAR
           MOVE WS-COD-CLI TO COD-CLI

           READ ARQ-CLIENTE RECORD INTO WS-ARQ-CLI-REG
                KEY IS COD-CLI

           IF FSTATUS-CLI NOT EQUAL "00"
              MOVE "CODIGO CLIENTE NAO ENCONTRADO"
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
                 DELETE ARQ-CLIENTE RECORD
              ELSE
                 MOVE 9 TO WS-SAIR
              END-IF
           END-IF
           CLOSE ARQ-CLIENTE.

      *-----------------------------------*
      *
       400-IMPORTACAO SECTION.
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
              OPEN INPUT IMPORT-CLI
              IF FS-IMPORT-CLI NOT EQUAL "00"
                 MOVE "ARQUIVO NAO ENCONTRADO" TO WS-MSG
              ELSE
                 OPEN I-O ARQ-CLIENTE

                 PERFORM UNTIL FS-IMPORT-CLI NOT EQUAL "00"
                    READ IMPORT-CLI
                    IF FS-IMPORT-CLI EQUAL ZEROS
                       ADD 1 TO WS-LIDOS
                       MOVE IMPORT-CNPJ      TO WS-CNPJ
                       MOVE IMPORT-COD-CLI   TO COD-CLI
                       MOVE IMPORT-CNPJ      TO CNPJ
                       MOVE IMPORT-RZ-SOCIAL TO RAZAO-SOCIAL

                       WRITE ARQ-CLI-REG
                       ADD 1 TO WS-GRAVADOS
                    END-IF
                 END-PERFORM

                 STRING "LIDOS: "
                        WS-LIDOS
                        " / GRAVADOS: "
                        WS-GRAVADOS
                   INTO WS-MSG
                 CLOSE ARQ-CLIENTE
                 CLOSE IMPORT-CLI
              END-IF
           END-IF.
      *
       END PROGRAM CADCLI.
      *-----------------------------------*
