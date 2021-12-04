      /O programa inicial não está estruturado, o que pode confundir a
      /Leitura do código para outros programadores ou até o próprio
      /criador no futuro, e o comando PERFORM possui uma lógica de
      /programação mais compreensível.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DADOS.
       AUTHOR EQUIPE_1_ADS.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
           SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
           01 AREAS-DE-TRABALHO.
               02 WS-NOME      PIC X(30) VALUE SPACES.
               02 WS-IDADE     PIC 9(02) VALUE ZEROS.
                   88 IDADE    VALUE 15 THRU 29.
               02 WS-SEXO      PIC X VALUE SPACE.
                   88 FM       VALUE "F" "M" "f" "m".
               02 WS-SALARIO   PIC 9(15) VALUE ZEROS.
               02 WS-SALARIO-E PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               02 WS-SAL-ATUAL PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               02 WS-CONT      PIC X VALUE SPACE.

           01 MENSAGENS-DE-CRITICA.
               02 MENSA1       PIC X(30) VALUE
                   "NOME INVALIDO <REDIGITE>".
               02 MENSA2       PIC X(30) VALUE
                   "IDADE INVALIDA <REDIGITE>".
               02 MENSA3       PIC X(30) VALUE
                   "SEXO INVALIDO <REDIGITE>".
               02 MENSA4       PIC X(30) VALUE
                   "SALARIO INVALIDO <REDIGITE>".
               02 MENSA5       PIC X(30) VALUE SPACES.
               02 MENSA6       PIC X(30) VALUE
                   "FIM DO PROGRAMA".
               02 MENSA7       PIC X(30) VALUE
                   "OPCAO INVALIDA <REDIGITE>".

           01 DATA-DO-SISTEMA.
                   02 ANO      PIC 9(02) VALUE ZEROS.
                   02 MES      PIC 9(02) VALUE ZEROS.
                   02 DIA      PIC 9(02) VALUE ZEROS.

       SCREEN SECTION.
           01 TELA.
               02 BLANK SCREEN.
               02 LINE 01 COLUMN 01 PIC 9(02)/ USING DIA.
               02 LINE 01 COLUMN 04 PIC 9(02)/ USING MES.
               02 LINE 01 COLUMN 07 PIC 9(02)  USING ANO.
               02 LINE 02 COLUMN 37 VALUE
                "** CONSISTENCIA DE DADOS **".
               02 LINE 04 COLUMN 25 VALUE
                   "AUTOR: FATEC RUBENS LARA - ADS NOITE - 2021-SEM2".
               02 LINE 08 COLUMN 21 VALUE "NOME: ".
               02 LINE 10 COLUMN 21 VALUE "IDADE (15-29): ".
               02 LINE 12 COLUMN 21 VALUE "SEXO (F/M): ".
               02 LINE 14 COLUMN 21 VALUE "SALARIO: ".
               02 LINE 16 COLUMN 21 VALUE "SALARIO ATUAL: ".
               02 LINE 19 COLUMN 21 VALUE "CONTINUA (S/N) < >".
               02 LINE 23 COLUMN 21 VALUE "MENSAGEM: ".

       PROCEDURE DIVISION.
       ROT-INICIO.
           ACCEPT  DATA-DO-SISTEMA FROM DATE.
           DISPLAY TELA.
           PERFORM ROT-NOME.
           PERFORM ROT-IDADE.
           PERFORM ROT-SEXO.
           PERFORM ROT-SALARIO.
           PERFORM ROT-CALCULO.
           PERFORM ROT-CONTINUA.

       ROT-NOME.
           ACCEPT  WS-NOME WITH PROMPT     AT 0839.
           DISPLAY MENSA5                  AT 2331.
           IF  WS-NOME = SPACES
               DISPLAY MENSA1              AT 2331
               PERFORM ROT-NOME.

       ROT-IDADE.
           ACCEPT WS-IDADE WITH PROMPT     AT 1039.
           DISPLAY MENSA5                  AT 2331.
           IF IDADE
               NEXT SENTENCE
           ELSE
               DISPLAY MENSA2              AT 2331
               PERFORM ROT-IDADE.

       ROT-SEXO.
           ACCEPT  WS-SEXO WITH PROMPT     AT 1239.
           DISPLAY MENSA5                  AT 2331.
           IF FM
               NEXT SENTENCE
           ELSE
               DISPLAY MENSA3              AT 2331
               PERFORM ROT-SEXO.

       ROT-SALARIO.
           ACCEPT WS-SALARIO-E             AT 1439 WITH PROMPT.
           DISPLAY MENSA5                  AT 2331.
           IF WS-SALARIO-E > 04999,00 OR < 50001,00
               NEXT SENTENCE
           ELSE
               DISPLAY MENSA4              AT 2331
               PERFORM ROT-SALARIO.

       ROT-CALCULO.
           MOVE WS-SALARIO-E TO WS-SALARIO.
           COMPUTE WS-SAL-ATUAL = WS-SALARIO * 25 / 100 + WS-SALARIO.
           DISPLAY WS-SAL-ATUAL            AT 1639.

       ROT-CONTINUA.
           ACCEPT WS-CONT WITH PROMPT      AT 1937.
           DISPLAY MENSA5                  AT 2331.
           IF WS-CONT = "S" OR "s"
               PERFORM ROT-INICIO.
           IF WS-CONT = "N" OR "n"
               DISPLAY  MENSA6 AT 2331
               CALL "C$SLEEP" USING 2
               STOP RUN
           ELSE
               DISPLAY MENSA7 AT 2331
               PERFORM ROT-CONTINUA.
