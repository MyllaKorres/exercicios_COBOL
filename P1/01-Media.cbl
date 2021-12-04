       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Media-Aritmetica.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 DADOS.
               02 WS-NOME      PIC A(30).
               02 WS-NOTA1     PIC 9(02).
               02 WS-NOTA2     PIC 9(02).
               02 WS-NOTA3     PIC 9(02).
               02 WS-NOTA1-E   PIC Z9.
               02 WS-NOTA2-E   PIC Z9.
               02 WS-NOTA3-E   PIC Z9.
               02 WS-MEDIA     PIC 9(03)V9.
               02 WS-MEDIA-E   PIC Z9,9.
           01 MENSAGENS-DE-TELA.
               02 MENSA0   PIC X(50) VALUE "DIGITE O NOME DO ALUNO".
               02 MENSA1   PIC X(50) VALUE "DIGITE A 1a NOTA".
               02 MENSA2   PIC X(50) VALUE "DIGITE A 2a NOTA".
               02 MENSA3   PIC X(50) VALUE "DIGITE NOTA VALIDA (0-10)".
               02 MENSA4   PIC X(50) VALUE SPACE.
               02 MENSA5   PIC X(50) VALUE "FIM DO PROGRAMA".
               02 MENSA6   PIC X(50) VALUE "DIGITE A NOTA DO EXAME".
           01 DATA-DO-SISTEMA.
               02 ANO      PIC 9(02) VALUE ZEROS.
               02 MES      PIC 9(02) VALUE ZEROS.
               02 DIA      PIC 9(02) VALUE ZEROS.

       SCREEN SECTION.
           01 TELA01.
               02 BLANK SCREEN.
               02 LINE 02 COLUMN 05 PIC 9(02)/ USING DIA.
               02 LINE 02 COLUMN 08 PIC 9(02)/ USING MES.
               02 LINE 02 COLUMN 11 PIC 9(02)  USING ANO.
               02 LINE 02 COLUMN 28 VALUE "CALCULO MEDIA ARITMETICA".
               02 LINE 08 COLUMN 15 VALUE "ALUNO:".
               02 LINE 09 COLUMN 15 VALUE "NOTA 1:".
               02 LINE 10 COLUMN 15 VALUE "NOTA 2:".
               02 LINE 11 COLUMN 15 VALUE "EXAME:".
               02 LINE 15 COLUMN 20 VALUE "MEDIA:".
               02 LINE 16 COLUMN 20 VALUE "SITUACAO:".

       PROCEDURE DIVISION.
       Inicio.
           ACCEPT   DATA-DO-SISTEMA FROM DATE.
           DISPLAY  TELA01     AT  0101.
           MOVE     ZEROS      TO  DADOS.
       Nome.
           DISPLAY  MENSA0     AT  2030.
           ACCEPT   WS-NOME    AT  0822.
           IF WS-NOME = SPACES
                   PERFORM Nome.
       Nota1.
           DISPLAY  MENSA1     AT  2030.
           ACCEPT   WS-NOTA1-E AT  0923.
           IF WS-NOTA1-E > 10
               DISPLAY MENSA3  AT  2130
               PERFORM Nota1.
           MOVE    WS-NOTA1-E  TO  WS-NOTA1.
           DISPLAY MENSA4      AT  2130.
       Nota2.
           DISPLAY  MENSA2     AT  2030.
           ACCEPT   WS-NOTA2-E AT  1023.
           IF  WS-NOTA2-E > 10
                   DISPLAY MENSA3  AT  2130
                   PERFORM Nota2.
           MOVE    WS-NOTA2-E  TO  WS-NOTA2.
           DISPLAY MENSA4      AT  2130.
       Calcula.
           DISPLAY  MENSA4     AT  2030.
           COMPUTE  WS-MEDIA = (WS-NOTA1 + WS-NOTA2) / 2.
       Confirmacao.
           IF       WS-MEDIA  < 6
               DISPLAY "MEDIA PARCIAL:" AT 1720
               MOVE WS-MEDIA TO WS-MEDIA-E
               DISPLAY WS-MEDIA-E  AT  1734
               DISPLAY MENSA6      AT  2030
               ACCEPT  WS-NOTA3-E  AT  1122
               IF  WS-NOTA3-E > 10
                    DISPLAY MENSA3 AT  2130
                    PERFORM    Confirmacao
               ELSE IF  WS-NOTA2-E > WS-NOTA1-E
                   DISPLAY MENSA4      AT  2130
                   MOVE    WS-NOTA2-E  TO  WS-NOTA1-E
                   END-IF
               MOVE    WS-NOTA3-E  TO  WS-NOTA2-E
               MOVE    WS-NOTA1-E  TO  WS-NOTA1
               MOVE    WS-NOTA2-E  TO  WS-NOTA2
               PERFORM Calcula
               DISPLAY MENSA4  AT  2030
               END-IF.
       Resultado.
           IF       WS-MEDIA >= 6
               MOVE WS-MEDIA       TO  WS-MEDIA-E
               DISPLAY  WS-MEDIA-E AT  1526
               DISPLAY "APROVADO"  AT  1630
           ELSE
               MOVE WS-MEDIA       TO  WS-MEDIA-E
               DISPLAY  WS-MEDIA-E AT  1526
               DISPLAY "REPROVADO" AT  1630.
       Finaliza.
           DISPLAY MENSA4  AT  1720.
           DISPLAY MENSA5  AT  2030.
           DISPLAY MENSA4  AT  2130.
           CALL "C$SLEEP" USING 3.
           STOP RUN.
