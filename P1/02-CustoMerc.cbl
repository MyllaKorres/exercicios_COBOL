       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Custo-Mercadoria.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 DADOS.
               02 MERCADORIA       PIC X(20).
               02 QTDE             PIC 9(4).
               02 PRECOUNI         PIC 9(4)V99.
               02 PRECOTOTAL       PIC 9(6)V99.
               02 PRECOVENDA       PIC 9(7)V99.
               02 CONTINUAR        PIC X(01) VALUE SPACE.
               02 QTDE-E           PIC Z.ZZ9.
               02 PRECOUNI-E       PIC Z.ZZ9,99.
               02 PRECOTOTAL-E     PIC ZZZ.ZZ9,99.
               02 PRECOVENDA-E     PIC Z.ZZZ.ZZ9,99.

           01 MENSAGENS-DE-TELA.
               02 MENSA0 PIC X(30) VALUE SPACES.
               02 MENSA1 PIC X(30) VALUE "DIGITE O NOME DA MERCADORIA".
               02 MENSA2 PIC X(30) VALUE "DIGITE A QUANTIDADE".
               02 MENSA3 PIC X(30) VALUE "DIGITE O PRECO UNITARIO".
               02 MENSA4 PIC X(30) VALUE "DIGITE UMA OPCAO VALIDA".
               02 MENSA5 PIC X(30) VALUE "FIM DO PROGRAMA".
               02 MENSA6 PIC X(30) VALUE "CONTINUA (S/N) ? < >".

       SCREEN SECTION.
           01 TELA01.
               02 BLANK SCREEN.
               02 LINE 02 COLUMN 15 VALUE
               "DISTRIBUIDORA DE PRODUTOS LTDA".
               02 LINE 05 COLUMN 05 VALUE "NOME DA MERCADORIA:".
               02 LINE 07 COLUMN 05 VALUE "QUANTIDADE:".
               02 LINE 09 COLUMN 05 VALUE "PRECO UNITARIO: $".
               02 LINE 13 COLUMN 05 VALUE "PRECO TOTAL: $".
               02 LINE 15 COLUMN 05 VALUE "PRECO DE VENDA: $".

       PROCEDURE DIVISION.
       Inicio.
           DISPLAY TELA01      AT 0101.
           MOVE ZEROS          TO DADOS.
       Nome.
           DISPLAY MENSA1      AT 2122.
           ACCEPT  MERCADORIA  AT 0525 .
           IF MERCADORIA = SPACES
               PERFORM Nome.
       Entra-Dados.
           DISPLAY MENSA2      AT 2122.
           ACCEPT  QTDE-E      AT 0717.
           MOVE    QTDE-E      TO QTDE.
           DISPLAY MENSA3      AT 2122.
           ACCEPT  PRECOUNI-E  AT 0923.
           MOVE    PRECOUNI-E  TO PRECOUNI.
       Calcula.
           DISPLAY MENSA0      AT 2122.
           COMPUTE PRECOTOTAL = QTDE * PRECOUNI.
           MOVE    PRECOTOTAL      TO PRECOTOTAL-E.
           DISPLAY PRECOTOTAL-E    AT 1318.
           COMPUTE PRECOVENDA = PRECOTOTAL * 1,3.
           MOVE    PRECOVENDA      TO PRECOVENDA-E
           DISPLAY PRECOVENDA-E    AT 1520.
       Finaliza.
           DISPLAY MENSA6  AT  1815.
           ACCEPT CONTINUAR    WITH PROMPT AT 1833.
           IF CONTINUAR = "S" OR "s"
               PERFORM Inicio THRU Finaliza
           ELSE IF CONTINUAR = "N" OR "n"
               DISPLAY  MENSA5 AT 2122
               CALL "C$SLEEP" USING 3
               STOP RUN
           ELSE
               DISPLAY MENSA4  AT 2122
               PERFORM Finaliza.
