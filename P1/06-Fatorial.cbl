      /    O programa não consegue evitar a entrada de valores maiores
      /    que 10.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  FATORIAL.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
           01 AREA-TRABALHO.
               02 WS-NOME        PIC X(30).
               02 WS-CONTADOR    PIC 9(03).
               02 WS-NUMERO      PIC 9(02).
               02 WS-NUMERO-E    PIC Z9.
               02 WS-RESULTADO   PIC 9(30).
               02 WS-RESULTADO-E PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
               02 WS-CONTINUA    PIC X(01) VALUE SPACE.
               02 WS-FL          PIC 9(01) VALUE ZEROS.
           01 MENSAGENS-DE-TELA.
      /        02 MENSA1 PIC X(30) VALUE "NUMERO DEVE SER MENOR QUE 10".
               02 MENSA2 PIC X(30) VALUE "FIM DO PROGRAMA".
               02 MENSA3 PIC X(30) VALUE SPACE.
           01 DATA-DO-SISTEMA.
               02 ANO PIC 9(02) VALUE ZEROS.
               02 MES PIC 9(02) VALUE ZEROS.
               02 DIA PIC 9(02) VALUE ZEROS.

       SCREEN SECTION.
           01 TELA01.
               02 BLANK SCREEN.
               02 LINE 02 COLUMN 05   PIC 9(02)/ USING DIA.
               02 LINE 02 COLUMN 08   PIC 9(02)/ USING MES.
               02 LINE 02 COLUMN 11   PIC 9(02)  USING ANO.
               02 LINE 02 COLUMN 28   VALUE
                   "CALCULO DE FATORIAL".
               02 LINE 08 COLUMN 21   VALUE "NOME....:".
               02 LINE 10 COLUMN 21   VALUE "NUMERO..:".
               02 LINE 14 COLUMN 21   VALUE "FATORIAL:".
               02 LINE 15 COLUMN 21   VALUE
                   "(Nota: output so mostrara 24 casas.)".
               02 LINE 18 COLUMN 30   VALUE "* CONTINUA (S/N): < > *".

       PROCEDURE DIVISION.
       INICIO.
           ACCEPT  DATA-DO-SISTEMA FROM DATE.
           PERFORM PROCESSO UNTIL WS-CONTINUA = "N".
           PERFORM SAIDA.
           STOP RUN.

       PROCESSO.
           PERFORM TELA.
           MOVE ZEROS          TO WS-FL.
           PERFORM ENTRA-DADOS UNTIL WS-FL=1.
           PERFORM CALCULA     UNTIL WS-CONTADOR > WS-NUMERO.
           PERFORM RESULTADO.
           EXIT.

       TELA.
      /    DISPLAY ERASE   AT 0101.
           DISPLAY TELA01  AT 0101.
           MOVE 2          TO WS-CONTADOR.
           MOVE ZEROS      TO WS-NUMERO.
           MOVE ZEROS      TO WS-NUMERO-E.
           MOVE 1          TO WS-RESULTADO.
           EXIT.

       ENTRA-DADOS.
           ACCEPT   WS-NOME     AT 0832 WITH PROMPT AUTO.
           ACCEPT   WS-NUMERO-E AT 1032 WITH PROMPT AUTO.
           MOVE     WS-NUMERO-E TO WS-NUMERO.
           DISPLAY  MENSA3      AT 2310.
           MOVE 1               TO WS-FL.
           EXIT.

       CALCULA.
           COMPUTE  WS-RESULTADO = WS-RESULTADO * WS-CONTADOR.
           ADD 1 TO WS-CONTADOR.
           EXIT.

       RESULTADO.
           MOVE    WS-RESULTADO    TO WS-RESULTADO-E.
           DISPLAY WS-RESULTADO-E  AT 1432.
           ACCEPT  WS-CONTINUA     AT 1849 WITH PROMPT AUTO.
           EXIT.

       SAIDA.
      /    DISPLAY ERASE AT 0101
           DISPLAY MENSA2.
           EXIT.
