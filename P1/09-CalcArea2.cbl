       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Calculo-Area-Circunferencia2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
              DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 DADOS.
               02 W-Raio   PIC 9(03)V99.
               02 W-Area   PIC 9(05)V99.
           01 DADOS-E.
               02 W-Raio-E   PIC ZZ9,99.
               02 W-Area-E   PIC ZZ.ZZ9,99.
           01 MENSAGENS-DE-TELA.
               02 MENSA1   PIC X(30) VALUE "DIGITE O RAIO".
               02 MENSA2   PIC X(30) VALUE "FIM DO PROGRAMA".
               02 MENSA3   PIC X(30) VALUE SPACE.
           01 DATA-DO-SISTEMA.
               02 ANO      PIC 9(02) VALUE ZEROS.
               02 MES      PIC 9(02) VALUE ZEROS.
               02 DIA      PIC 9(02) VALUE ZEROS.

       SCREEN SECTION.
           01 TELA01.
               02 LINE 02 COLUMN 05 PIC 9(02)/ USING DIA.
               02 LINE 02 COLUMN 08 PIC 9(02)/ USING MES.
               02 LINE 02 COLUMN 11 PIC 9(02)  USING ANO.
               02 LINE 02 COLUMN 28 VALUE
               "Calculo da Area de um Circulo".
               02 LINE 08 COLUMN 15 VALUE "Raio:".
               02 LINE 10 COLUMN 15 VALUE "Area:".

       PROCEDURE DIVISION.
       Inicio.
           ACCEPT  DATA-DO-SISTEMA FROM DATE.
           DISPLAY TELA01      AT  0101.
           MOVE    ZEROS       TO  DADOS.
       Entrada.
           DISPLAY MENSA1      AT  2030.
           ACCEPT  W-Raio-E    AT  0821.
           MOVE    W-Raio-E    TO  W-Raio.
           DISPLAY MENSA3      AT  2030.
       Calcula.
           COMPUTE W-Area = 3,1416*(w-Raio**2).
           MOVE    W-Area      TO  W-Area-E.
           DISPLAY W-Area-E    AT  1021.
       Finaliza.
           DISPLAY MENSA2      AT  2030.
           CALL "C$SLEEP" USING 5.
           STOP RUN.
