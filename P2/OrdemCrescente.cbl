       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ORDEMCRESCENTE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
              DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 DADOS-DO-SISTEMA.
               02 OPCAO        PIC X(01)  VALUE SPACE.
               02 W-DADO-E     PIC ZZZ9.
               02 TEMP-DADO    PIC ZZZ9.
               02 CONTARRAY    PIC 9(01).
               02 CONTAUX      PIC 9(01).
               02 LISTA-NUMEROS.
                   03 FILLER   PIC ZZZ9.
                   03 FILLER   PIC ZZZ9.
                   03 FILLER   PIC ZZZ9.
                   03 FILLER   PIC ZZZ9.
               02 TABELA-NUMEROS REDEFINES LISTA-NUMEROS.
                   03 W-NUMERORD  PIC ZZZ9    OCCURS 4 TIMES.
           01 MENSAGENS-DE-TELA.
               02 MENSA1       PIC X(30) VALUE "MENOR VALOR 1: ".
               02 MENSA2       PIC X(30) VALUE "MENOR VALOR 2: ".
               02 MENSA3       PIC X(30) VALUE "MENOR VALOR 3: ".
               02 MENSA4       PIC X(30) VALUE "MAIOR VALOR  : ".
               02 MENSA5       PIC X(30) VALUE "CONTINUA (S/N)? < >".
               02 MENSA6       PIC X(16) VALUE "FIM DE PROGRAMA!".
               02 MENSA7       PIC X(30) VALUE SPACE.
           01 DATA-DO-SISTEMA.
               02 ANO      PIC 9(02) VALUE ZEROS.
               02 MES      PIC 9(02) VALUE ZEROS.
               02 DIA      PIC 9(02) VALUE ZEROS.

       SCREEN SECTION.
           01 TELA01.
               02 LINE 03 COLUMN 23 VALUE ">>> ORDEM CRESCENTE <<<".
               02 LINE 02 COLUMN 05 PIC 9(02)/ USING DIA.
               02 LINE 02 COLUMN 08 PIC 9(02)/ USING MES.
               02 LINE 02 COLUMN 11 PIC 9(02)  USING ANO.
               02 LINE 08 COLUMN 15 VALUE "Valor   :".

       PROCEDURE DIVISION.
       INICIO.
           PERFORM PROGRAMA UNTIL OPCAO = "N" OR "n".
           PERFORM FINALIZA.
           STOP RUN.
       PROGRAMA.
           DISPLAY SPACE UPON CRT.
           ACCEPT  DATA-DO-SISTEMA FROM DATE.
           DISPLAY TELA01.
           MOVE 1 TO CONTARRAY.
           PERFORM ENTRADA UNTIL CONTARRAY>4.
           PERFORM MOSTRAVALORES.
           PERFORM CONTINUA.
       ENTRADA.
           DISPLAY CONTARRAY       AT    0822.
           ACCEPT  W-DADO-E        AT    0825.
           MOVE 1 TO CONTAUX.
           PERFORM ARRAYLOOP UNTIL CONTAUX>CONTARRAY.
           ADD 1 TO CONTARRAY.
       ARRAYLOOP.
           IF (W-DADO-E < W-NUMERORD(CONTAUX)) OR
               (CONTAUX = CONTARRAY)
               MOVE W-NUMERORD(CONTAUX) TO TEMP-DADO
               MOVE W-DADO-E TO W-NUMERORD(CONTAUX)
               MOVE TEMP-DADO TO W-DADO-E
           END-IF.
           ADD 1 TO CONTAUX.
       MOSTRAVALORES.
           DISPLAY MENSA1          AT 1115.
           DISPLAY MENSA2          AT 1215.
           DISPLAY MENSA3          AT 1315.
           DISPLAY MENSA4          AT 1415.
           DISPLAY W-NUMERORD(1)   AT 1131.
           DISPLAY W-NUMERORD(2)   AT 1231.
           DISPLAY W-NUMERORD(3)   AT 1331.
           DISPLAY W-NUMERORD(4)   AT 1431.
       CONTINUA.
           DISPLAY MENSA5          AT  2030.
           ACCEPT  OPCAO           AT 2047 WITH PROMPT AUTO.
           IF      OPCAO <> "S" AND "s" AND "n" AND "N"
               PERFORM CONTINUA.
       FINALIZA.
           DISPLAY MENSA6          AT  2130.
           CALL "C$SLEEP" USING 5.
           STOP RUN.
