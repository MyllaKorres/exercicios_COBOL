       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBONACCI.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 DADOS.
               02 NUM      PIC  9(4)   VALUE  1.
               02 PROXNUM  PIC  S9(4)  VALUE  -1.
               02 SOMA     PIC  9(5).
               02 FIB      PIC  9(04).
               02 FIB-E    PIC  ZZZ9.
               02 N        PIC  9(2).
               02 N-E      PIC  Z9.
               02 RESP     PIC  X(01)   VALUE SPACE.
               02 AUX      PIC  9(04).
           01 MENSAGENS.
               02 MENSA0   PIC  X(55) VALUE
               "INSIRA UM NUMERO VALIDO!".
               02 MENSA1   PIC  X(55) VALUE
               "INSIRA UM NUMERO MENOR OU IGUAL A 24!".
               02 MENSA2   PIC  X(55) VALUE
               "DESEJA GERAR OUTRA SEQUENCIA DE FIBONACCI (S/N) ? < >".
               02 MENSA3   PIC  X(55) VALUE
               "FIM DE PROGRAMA!".
       SCREEN SECTION.
           1 TELA01.
               02 BLANK SCREEN.
               02 LINE 03 COLUMN 23 VALUE ">>> FIBONACCI <<<".
               02 LINE 07 COLUMN 20 VALUE
               "DIGITE A QUANTIDADE DESEJADA DE TERMOS: ".
               02 LINE 11 COLUMN 20 VALUE "SEQUENCIA: ".
               02 LINE 15 COLUMN 20 VALUE "MENSAGEM: ".
       PROCEDURE DIVISION.
       INICIO.
           PERFORM PROCESSO UNTIL RESP = "N" OR "n".
           DISPLAY MENSA3 AT 1530.
           CALL "C$SLEEP" USING 3.
           STOP RUN.
       PROCESSO.
           DISPLAY TELA01.
           MOVE 1 TO NUM.
           MOVE -1 TO PROXNUM.
           MOVE 0 TO N.
           MOVE " " TO RESP.
           MOVE 1131 TO AUX.
           PERFORM RECEBE-N UNTIL N > 0 AND N < 25.
           PERFORM CALCULO N TIMES.
           PERFORM CONTINUA UNTIL RESP = "s" OR "S" OR "n" OR "N".
       RECEBE-N.
           ACCEPT N-E AT 0760.
           MOVE N-E TO N.
           IF N < 1
               DISPLAY MENSA0 AT 1530
           ELSE IF N > 24
               DISPLAY MENSA1 AT 1530.
       CALCULO.
           COMPUTE SOMA = NUM + PROXNUM.
           MOVE SOMA TO FIB-E.
           DISPLAY FIB-E AT AUX.
           MOVE FIB-E TO FIB.
           ADD 6 TO AUX.
           MOVE PROXNUM TO NUM.
           MOVE SOMA TO PROXNUM.
           IF FIB = 89
               MOVE 1231 TO AUX.
       CONTINUA.
           DISPLAY MENSA2 AT 1530.
           ACCEPT RESP AT 1581.
