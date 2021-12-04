       IDENTIFICATION DIVISION.
       PROGRAM-ID. RAIZ-QUADRADA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 DADOS.
               02 OPCAO          PIC X(01)  VALUE SPACE.
               02 AUX            PIC 9(01).
               02 AUX2           PIC 9(04).
               02 NUM            PIC 9(02).
               02 RAIZ           PIC 9(02)V9(02).
               02 RAIZ-E         PIC Z9,99.
           01 NATURAIS-IMPARES.
               02 FILLER     PIC 9(02).
           01 TABELA-IMPARES REDEFINES NATURAIS-IMPARES.
               02 IMPAR-T    PIC Z9    OCCURS 5 TIMES.
           01 NATURAIS-PARES.
               02 FILLER     PIC 9(02).
           01 TABELA-PARES   REDEFINES NATURAIS-PARES.
               02 PAR-T      PIC 9(02) OCCURS 5 TIMES.
           01 TABELA-PARES-E REDEFINES NATURAIS-PARES.
               02 PAR-T-E    PIC Z9    OCCURS 5 TIMES.
           01 MENSAGENS.
               02 MENSA1     PIC X(30) VALUE "INSIRA UM NUMERO IMPAR!".
               02 MENSA2     PIC X(30) VALUE "INSIRA UM NUMERO PAR!".
               02 MENSA3     PIC X(16) VALUE "FIM DE PROGRAMA!".
               02 MENSA4     PIC X(30) VALUE "CONTINUA (S/N)? < >".
               02 MENSA5     PIC X(30) VALUE SPACES.
       SCREEN SECTION.
           01 TELA01.
               02 BLANK SCREEN.
               02 LINE 03 COLUMN 23 VALUE ">>> RAIZ QUADRADA <<<".
               02 LINE 07 COLUMN 20 VALUE
               "DIGITE 5 NUMEROS IMPARES: __, __, __, __, __".
               02 LINE 11 COLUMN 20 VALUE
               "DIGITE 5 NUMEROS PARES: __, __, __, __, __".
               02 LINE 21 COLUMN 20 VALUE "MENSAGEM: ".
       PROCEDURE DIVISION.
           INICIO.
               INITIALIZE OPCAO.
               PERFORM CORPO UNTIL OPCAO = "N" OR "n".
               DISPLAY MENSA3 AT 2523.
               CALL "C$SLEEP" USING 2.
               STOP RUN.
           CORPO.
               PERFORM ABERTURA.
               MOVE 1 TO AUX.
               MOVE 0746 TO AUX2.
               PERFORM RECEBE-IMPAR 5 TIMES.
               MOVE 1 TO AUX.
               MOVE 1144 TO AUX2.
               PERFORM RECEBE-PAR 5 TIMES.
               PERFORM MOSTRA.
               PERFORM CONTINUA UNTIL OPCAO = "N" OR "n".
           ABERTURA.
               DISPLAY TELA01 AT 0101.
           RECEBE-IMPAR.
               ACCEPT IMPAR-T(AUX) AT AUX2 WITH PROMPT AUTO.
               IF FUNCTION MOD(IMPAR-T(AUX), 2) = 0
                   DISPLAY MENSA1 AT 2130
                   PERFORM RECEBE-IMPAR
               ELSE
                   DISPLAY MENSA5 AT 2130
                   ADD 1 TO AUX
                   ADD 4 TO AUX2.
           RECEBE-PAR.
               ACCEPT PAR-T-E(AUX) AT AUX2 WITH PROMPT AUTO.
               MOVE PAR-T-E(AUX) TO PAR-T(AUX).
               IF FUNCTION MOD(PAR-T(AUX), 2) <> 0 OR PAR-T(AUX) = 0
                   DISPLAY MENSA2 AT 2130
                   PERFORM RECEBE-PAR
               ELSE
                   DISPLAY MENSA5 AT 2130
                   ADD 1 TO AUX
                   ADD 4 TO AUX2.
           MOSTRA.
               MOVE 1 TO AUX.
               MOVE 1547 TO AUX2.
               DISPLAY "RAIZES DOS VALORES IMPARES: " AT 1520.
               PERFORM CALCULA-IMPAR 5 TIMES.
               MOVE 1 TO AUX.
               MOVE 1745 TO AUX2.
               DISPLAY "RAIZES DOS VALORES PARES: " AT 1720.
               PERFORM CALCULA-PAR 5 TIMES.
           CALCULA-IMPAR.
               MOVE IMPAR-T(AUX) TO NUM.
               COMPUTE RAIZ = FUNCTION SQRT (NUM).
               MOVE RAIZ TO RAIZ-E.
               DISPLAY RAIZ-E AT AUX2.
               ADD 1 TO AUX.
               ADD 8 TO AUX2.
           CALCULA-PAR.
               MOVE PAR-T(AUX) TO NUM.
               COMPUTE RAIZ-E = FUNCTION SQRT (NUM).
               DISPLAY RAIZ-E AT AUX2.
               ADD 1 TO AUX.
               ADD 8 TO AUX2.
           CONTINUA.
               DISPLAY MENSA4 AT 2130.
               ACCEPT  OPCAO AT 2147 WITH PROMPT AUTO.
               IF      OPCAO = "S" or "s"
                   PERFORM INICIO.
