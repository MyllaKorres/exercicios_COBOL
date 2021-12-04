       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CALCULADORA.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
           01 DADOS.
               02 NUM1     PIC S9(03).
               02 NUM2     PIC S9(03).
               02 NUM1-E   PIC -ZZ9.
               02 NUM2-E   PIC -ZZ9.
               02 MAIS-E   PIC Z.ZZ9.
               02 MENOS-E  PIC -Z.ZZ9.
               02 DIV-E    PIC ZZ9,999.
               02 MULT-E   PIC ZZZ.ZZ9.

           01 MENSAGENS-DE-TELA.
               02 MENSA1   PIC X(30) VALUE "DIGITE O 1o NUMERO".
               02 MENSA2   PIC X(30) VALUE "DIGITE O 2o NUMERO".
               02 MENSA3   PIC X(30) VALUE "FIM DO PROGRAMA".
               02 MENSA4   PIC X(30) VALUE SPACE.

           01 DATA-DO-SISTEMA.
               02 ANO      PIC 9(02) VALUE ZEROS.
               02 MES      PIC 9(02) VALUE ZEROS.
               02 DIA      PIC 9(02) VALUE ZEROS.

       SCREEN SECTION.
           01 TELA01.
               02 LINE 02 COLUMN 05 PIC 9(02)/ USING DIA.
               02 LINE 02 COLUMN 08 PIC 9(02)/ USING MES.
               02 LINE 02 COLUMN 11 PIC 9(02)  USING ANO.
               02 LINE 02 COLUMN 28 VALUE "Calculadora".
               02 LINE 08 COLUMN 15 VALUE "NUM1:".
               02 LINE 09 COLUMN 15 VALUE "NUM2:".
               02 LINE 12 COLUMN 20 VALUE "RESULTADOS".
               02 LINE 14 COLUMN 15 VALUE "SOMA: ".
               02 LINE 16 COLUMN 15 VALUE "SUBTRACAO: ".
               02 LINE 18 COLUMN 15 VALUE "DIVISAO: ".
               02 LINE 20 COLUMN 15 VALUE "MULTIPLICACAO: ".

       PROCEDURE DIVISION.
       Inicio.
           ACCEPT   DATA-DO-SISTEMA FROM DATE.
           DISPLAY  TELA01  AT  0101.
           MOVE     ZEROS   TO  DADOS.
       Entrada.
           DISPLAY  MENSA1  AT  2230.
           ACCEPT   NUM1-E  AT  0821.
           MOVE     NUM1-E  TO  NUM1.
           DISPLAY  MENSA2  AT  2230.
           ACCEPT   NUM2-E  AT  0921.
           MOVE     NUM2-E  TO  NUM2.
           DISPLAY  MENSA4  AT  2230.
       Calcula.
           ADD      NUM1        NUM2   GIVING   MAIS-E.
           SUBTRACT NUM2  FROM  NUM1   GIVING   MENOS-E.
           DIVIDE   NUM1  BY    NUM2   GIVING   DIV-E.
           MULTIPLY NUM1  BY    NUM2   GIVING   MULT-E.
       Resultado.
           DISPLAY MAIS-E   AT  1421.
           DISPLAY MENOS-E  AT  1626.
           IF NUM2 = 0
               DISPLAY "NAO EXISTE" AT 1824
           ELSE
               DISPLAY DIV-E   AT  1824
           END-IF.
           DISPLAY MULT-E   AT 2030.
       Finaliza.
           DISPLAY MENSA3   AT 2230.

           CALL "C$SLEEP" USING 6.
           STOP RUN.
