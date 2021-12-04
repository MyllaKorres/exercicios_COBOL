       IDENTIFICATION DIVISION.
       PROGRAM-ID.  EQUACAO-2o-GRAU-NAO-ESTRUTURADO.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
           01 DADOS.
               02 A            PIC S9(03).
               02 B            PIC S9(03).
               02 C            PIC S9(03).
               02 A-E          PIC -ZZ9.
               02 B-E          PIC -ZZ9.
               02 C-E          PIC -ZZ9.
               02 DELTA        PIC S9(05)V9(03).
               02 DELTA-E      PIC -ZZZZ9,999.
               02 X1           PIC S9(05)V9(03).
               02 X1-E         PIC -ZZZZ9,999.
               02 X2           PIC S9(05)V9(03).
               02 X2-E         PIC -ZZZZ9,999.
               02 RAIZ         PIC 9(05)V9(05) VALUE ZEROS.
               02 CONTINUAR    PIC X(01) VALUE SPACE.

           01 MENSAGENS-DE-TELA.
               02 MENSA0 PIC X(25) VALUE SPACES.
               02 MENSA1 PIC X(25) VALUE "DIGITE O VALOR DE A".
               02 MENSA2 PIC X(25) VALUE "DIGITE O VALOR DE B".
               02 MENSA3 PIC X(25) VALUE "DIGITE O VALOR DE C".
               02 MENSA4 PIC X(25) VALUE "DIGITE UM VALOR VALIDO!".
               02 MENSA5 PIC X(25) VALUE "DIGITE UMA OPCAO VALIDA!".
               02 MENSA6 PIC X(25) VALUE "FIM DO PROGRAMA".
               02 MENSA7 PIC X(25) VALUE "CALCULAR OUTRAS RAIZES?".

       SCREEN SECTION.
           01 TELA01.
               02 BLANK SCREEN.
               02 LINE 02 COLUMN 15   VALUE "EQUACAO DE 2o GRAU".
               02 LINE 05 COLUMN 05   VALUE "VALOR DE A: ".
               02 LINE 06 COLUMN 05   VALUE "VALOR DE B: ".
               02 LINE 07 COLUMN 05   VALUE "VALOR DE C: ".
               02 LINE 09 COLUMN 05   VALUE "DELTA: ".
               02 LINE 17 COLUMN 05   VALUE "CONTINUA (S/N) ?  < >".
               02 LINE 20 COLUMN 05   VALUE "MENSAGEM: ".

       PROCEDURE DIVISION.

       INICIO.
           DISPLAY TELA01      AT 0102.
           MOVE ZEROS          TO DADOS.
           DISPLAY MENSA1  AT 2016.
           ACCEPT  A-E         AT 0518.
           MOVE    A-E         TO A.
           IF A = 0
               DISPLAY MENSA4  AT 2016
               CALL "C$SLEEP" USING 2
               GO TO INICIO.
           DISPLAY MENSA2  AT 2016.
           ACCEPT  B-E         AT 0618.
           MOVE    B-E         TO B.
           DISPLAY MENSA3  AT 2016.
           ACCEPT  C-E         AT 0718.
           MOVE    C-E         TO C.
           COMPUTE DELTA = B * B - ( 4 * A * C ).
           COMPUTE RAIZ = FUNCTION SQRT (DELTA).
           COMPUTE X1 = (- B + RAIZ ) / ( 2 * A ).
           MOVE    X1 TO X1-E.
           COMPUTE X2 = (- B - RAIZ ) / ( 2 * A ).
           MOVE    X2 TO X2-E.
           DISPLAY MENSA7  AT 2016.
           EVALUATE DELTA
           WHEN = 0
               MOVE    DELTA   TO DELTA-E
               DISPLAY DELTA-E AT 0915
               DISPLAY "SO EXISTE UM VALOR PARA X" AT 1106
               DISPLAY "X1 = X2 = " AT 1306
               DISPLAY X1-E AT 1316
           WHEN > 0
               MOVE    DELTA   TO DELTA-E
               DISPLAY DELTA-E AT 0915
               DISPLAY "EXISTEM DOIS VALORES PARA X" AT 1106
               DISPLAY "X1 = " AT 1306
               DISPLAY "X2 = " AT 1406
               DISPLAY X1-E AT 1314
               DISPLAY X2-E AT 1414
           WHEN OTHER
               MOVE    DELTA   TO DELTA-E
               DISPLAY DELTA-E AT 0915
               DISPLAY "NAO EXISTEM RAIZES REAIS" AT 1106
           END-EVALUATE.
           ACCEPT CONTINUAR WITH PROMPT AT 1725.
           IF CONTINUAR = "S" OR "s"
               GO TO INICIO
           ELSE
               DISPLAY  MENSA6 AT 2016
               CALL "C$SLEEP" USING 3
               STOP RUN.
