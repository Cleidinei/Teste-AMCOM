       PROGRAM-ID.   MAINMENU.
       AUTHOR.       CLEIDINEI.
       DATE-WRITTEN.  08 MARCO 2020.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
      *
      *-----------------------------------*
       WORKING-STORAGE SECTION.
      *-----------------------------------*
       77 WS-ERRO                 PIC 9       VALUE ZERO.
       77 WS-OPCAO                PIC 9       VALUE ZERO.
       77 WS-CONFIRMA             PIC X       VALUE SPACES.
      *-----------------------------------*
       01 PARAMETROS.
          03 WS-ORDEM             PIC X       VALUE SPACES.
          03 WS-CLASSIFICA        PIC 9       VALUE ZERO.
          03 WS-CODVENDEDOR       PIC 9(003)  VALUE ZEROS.
          03 WS-CODCLIENTE        PIC 9(007)  VALUE ZEROS.
          03 WS-RZ-NOME           PIC X(040)  VALUE SPACES.
          03 WS-MSG               PIC X(040)  VALUE SPACES.
      *-----------------------------------*
      *
       SCREEN SECTION.
       01 MENU.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "CADASTROS".
          02 LINE 06 COL 15 VALUE "(1) CLIENTES".
          02 LINE 07 COL 15 VALUE "(2) VENDEDORES".
          02 LINE 09 COL 10 VALUE "RELATORIOS".
          02 LINE 10 COL 15 VALUE "(3) CLIENTES".
          02 LINE 11 COL 15 VALUE "(4) VENDEDORES".
          02 LINE 13 COL 10 VALUE "DISTRIBUICAO".
          02 LINE 14 COL 15 VALUE "(5) EXECUTAR".
          02 LINE 16 COL 10 VALUE "SAIR DO SISTEMA".
          02 LINE 17 COL 15 VALUE "(9) SAIR".
          02 LINE 19 COL 10 VALUE "OPCAO DESEJADA (.)".
          02 LINE 19 COL 26, PIC 9 TO WS-OPCAO AUTO.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".
      *-----------------------------------*
      *
       01 REL-CLIENTE.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "RELATORIO CLIENTES".
          02 LINE 07 COL 10 VALUE
             "ORDENACAO (A)SCENDENTE OU (D)ESCENDENTE.....: (.)".
          02 LINE 07 COL 55, PIC X TO WS-ORDEM AUTO.
          02 LINE 08 COL 10 VALUE
             "CLASSIFICACAO (1)-CODIGO OU (2)-RAZAO SOCIAL: (.)".
          02 LINE 08 COL 55, PIC 9 TO WS-CLASSIFICA AUTO.
          02 LINE 10 COL 10 VALUE "FILTRO CODIGO CLIENTE: (.......)".
          02 LINE 10 COL 35, PIC ZZZZZZ9 TO WS-CODCLIENTE AUTO.
          02 LINE 11 COL 10 VALUE
          "FILTRO RAZAO SOCIAL..: (....................................
      -"....)".
          02 LINE 11 COL 35, PIC X(040) TO WS-RZ-NOME AUTO.
          02 LINE 15 COL 10 VALUE
          "CONFIRMAR GERACAO RELATORIO (S/N): (.)".
          02 LINE 15 COL 46, PIC X TO WS-CONFIRMA AUTO.
          02 LINE 19 COL 10 VALUE "MSG:".
          02 LINE 19 COL 15, PIC X(040) FROM WS-MSG.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".
      *-----------------------------------*
      *
       01 REL-VENDEDOR.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "RELATORIO VENDEDORES".
          02 LINE 07 COL 10 VALUE
             "ORDENACAO (A)SCENDENTE OU (D)ESCENDENTE......: (.)".
          02 LINE 07 COL 56, PIC X TO WS-ORDEM AUTO.
          02 LINE 08 COL 10 VALUE
             "CLASSIFICACAO (1)-CODIGO OU (2)-NOME VENDEDOR: (.)".
          02 LINE 08 COL 56, PIC 9 TO WS-CLASSIFICA AUTO.
          02 LINE 10 COL 10 VALUE "FILTRO CODIGO VENDEDOR: (...)".
          02 LINE 10 COL 36, PIC ZZ9 TO WS-CODVENDEDOR AUTO.
          02 LINE 11 COL 10 VALUE
             "FILTRO NOME VENDEDOR..: (................................
      -"........)".
          02 LINE 11 COL 36, PIC X(40) TO WS-RZ-NOME AUTO.
          02 LINE 15 COL 10 VALUE
          "CONFIRMAR GERACAO RELATORIO (S/N): (.)".
          02 LINE 15 COL 46, PIC X TO WS-CONFIRMA AUTO.
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
                 CALL "CADCLI"
              WHEN 2
                 CALL "CADVEND"
              WHEN 3
                 PERFORM 100-RELAT-CLI
              WHEN 4
                 PERFORM 200-RELAT-VEND
              WHEN 5
                 CALL "FAZDISTRIB"
              WHEN 9
                 STOP RUN
           END-EVALUATE
           PERFORM 000-INICIO.


      *-----------------------------------*
      *
       100-RELAT-CLI SECTION.
      *
           DISPLAY REL-CLIENTE
      *
           ACCEPT  REL-CLIENTE
      *
           MOVE 0 TO WS-ERRO
           IF FUNCTION UPPER-CASE(WS-ORDEM) NOT EQUAL "A" AND
              FUNCTION UPPER-CASE(WS-ORDEM) NOT EQUAL "D"
              MOVE "ORDENACAO INVALIDA. INFORME A OU D" TO WS-MSG
              MOVE 9 TO WS-ERRO
           END-IF
           IF WS-CLASSIFICA NOT EQUAL 1 AND
              WS-CLASSIFICA NOT EQUAL 2
              MOVE "CLASSIFICACAO INVALIDA. INFORME 1 OU 2" TO WS-MSG
              MOVE 9 TO WS-ERRO
           END-IF
           IF FUNCTION UPPER-CASE(WS-CONFIRMA) = "S"
              IF WS-ERRO EQUAL 0
                 MOVE FUNCTION UPPER-CASE(WS-ORDEM)
                   TO WS-ORDEM
                 CALL "RELAT-CLI" USING PARAMETROS
              END-IF
              PERFORM 100-RELAT-CLI
           END-IF
           PERFORM 000-INICIO.

      *-----------------------------------*
      *
       200-RELAT-VEND SECTION.
      *
           DISPLAY REL-VENDEDOR
      *
           ACCEPT  REL-VENDEDOR
      *
           MOVE 0 TO WS-ERRO
           IF FUNCTION UPPER-CASE(WS-ORDEM) NOT EQUAL "A" AND
              FUNCTION UPPER-CASE(WS-ORDEM) NOT EQUAL "D"
              MOVE "ORDENACAO INVALIDA. INFORME A OU D" TO WS-MSG
              MOVE 9 TO WS-ERRO
           END-IF
           IF WS-CLASSIFICA NOT EQUAL 1 AND
              WS-CLASSIFICA NOT EQUAL 2
              MOVE "CLASSIFICACAO INVALIDA. INFORME 1 OU 2" TO WS-MSG
              MOVE 9 TO WS-ERRO
           END-IF
           IF FUNCTION UPPER-CASE(WS-CONFIRMA) = "S"
              IF WS-ERRO EQUAL 0
                 MOVE FUNCTION UPPER-CASE(WS-ORDEM)
                   TO WS-ORDEM
                 CALL "RELAT-VEND" USING PARAMETROS
              END-IF
              PERFORM 200-RELAT-VEND
           END-IF
           PERFORM 000-INICIO.

       END PROGRAM MAINMENU.


      *-----------------------------------*
      *
