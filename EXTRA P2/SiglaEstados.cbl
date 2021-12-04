       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIGLAS-DOS-ESTADOS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 ESTADOS.
               02 FILLER PIC X(19) VALUE "Acre".
               02 FILLER PIC X(19) VALUE "Alagoas".
               02 FILLER PIC X(19) VALUE "Amapa".
               02 FILLER PIC X(19) VALUE "Amazonas".
               02 FILLER PIC X(19) VALUE "Bahia".
               02 FILLER PIC X(19) VALUE "Ceara".
               02 FILLER PIC X(19) VALUE "Distrito Federal".
               02 FILLER PIC X(19) VALUE "Espirito Santo".
               02 FILLER PIC X(19) VALUE "Goias".
               02 FILLER PIC X(19) VALUE "Maranhao".
               02 FILLER PIC X(19) VALUE "Mato Grosso".
               02 FILLER PIC X(19) VALUE "Mato Grosso do Sul".
               02 FILLER PIC X(19) VALUE "Minas Gerais".
               02 FILLER PIC X(19) VALUE "Para".
               02 FILLER PIC X(19) VALUE "Paraiba".
               02 FILLER PIC X(19) VALUE "Parana".
               02 FILLER PIC X(19) VALUE "Pernambuco".
               02 FILLER PIC X(19) VALUE "Piaui".
               02 FILLER PIC X(19) VALUE "Rio de Janeiro".
               02 FILLER PIC X(19) VALUE "Rio Grande do Norte".
               02 FILLER PIC X(19) VALUE "Rio Grande do Sul".
               02 FILLER PIC X(19) VALUE "Rondonia".
               02 FILLER PIC X(19) VALUE "Roraima".
               02 FILLER PIC X(19) VALUE "Santa Catarina".
               02 FILLER PIC X(19) VALUE "Sao Paulo".
               02 FILLER PIC X(19) VALUE "Sergipe".
               02 FILLER PIC X(19) VALUE "Tocantins".
           01 TABELA-ESTADOS REDEFINES ESTADOS.
               02 ESTADO-T   PIC X(19) OCCURS 27 TIMES.
           01 SIGLAS.
               02 FILLER PIC X(2) VALUE "AC".
               02 FILLER PIC X(2) VALUE "AL".
               02 FILLER PIC X(2) VALUE "AP".
               02 FILLER PIC X(2) VALUE "AM".
               02 FILLER PIC X(2) VALUE "BA".
               02 FILLER PIC X(2) VALUE "CE".
               02 FILLER PIC X(2) VALUE "DF".
               02 FILLER PIC X(2) VALUE "ES".
               02 FILLER PIC X(2) VALUE "GO".
               02 FILLER PIC X(2) VALUE "MA".
               02 FILLER PIC X(2) VALUE "MT".
               02 FILLER PIC X(2) VALUE "MS".
               02 FILLER PIC X(2) VALUE "MG".
               02 FILLER PIC X(2) VALUE "PA".
               02 FILLER PIC X(2) VALUE "PB".
               02 FILLER PIC X(2) VALUE "PR".
               02 FILLER PIC X(2) VALUE "PE".
               02 FILLER PIC X(2) VALUE "PI".
               02 FILLER PIC X(2) VALUE "RJ".
               02 FILLER PIC X(2) VALUE "RN".
               02 FILLER PIC X(2) VALUE "RS".
               02 FILLER PIC X(2) VALUE "RO".
               02 FILLER PIC X(2) VALUE "RR".
               02 FILLER PIC X(2) VALUE "SC".
               02 FILLER PIC X(2) VALUE "SP".
               02 FILLER PIC X(2) VALUE "SE".
               02 FILLER PIC X(2) VALUE "TO".
           01 TABELA-SIGLAS REDEFINES SIGLAS.
               02 SIGLA-T  PIC X(2)  OCCURS 27 TIMES.
           01 DADOS-CAR.
               02 OPCAO    PIC A     VALUE SPACES.
               02 SIGLA    PIC AA    VALUE SPACES.
               02 OPCAO-E  PIC A     VALUE SPACES.
               02 SIGLA-E  PIC AA    VALUE SPACES.
               02 ESPACO   PIC X(40) VALUE SPACES.
           01 DADOS-NUM.
               02 LOOP     PIC 9(2)  VALUE ZEROS.
               02 TESTE    PIC 9     VALUE ZERO.
       SCREEN SECTION.
           01 TELA.
               02 BLANK SCREEN.
               02 LINE 03 COLUMN 25 VALUE ">>> SIGLAS DOS ESTADOS <<<".
               02 LINE 09 COLUMN 23 VALUE "DIGITE A SIGLA: ".
               02 LINE 15 COLUMN 23 VALUE "MENSAGEM: ".
       PROCEDURE DIVISION.
           INICIO.
               PERFORM CORPO UNTIL OPCAO = "N" OR "n".
               DISPLAY "FIM DO PROGRAMA" AT 2030.
               CALL "C$SLEEP" USING 2.
               STOP RUN.
           CORPO.
               PERFORM ABERTURA.
               MOVE SPACES TO DADOS-CAR.
               MOVE ZEROS  TO DADOS-NUM.
               PERFORM MOSTRAR.
               PERFORM CONTINUA UNTIL OPCAO = "S" OR "s" OR "N" OR "n".
           ABERTURA.
               DISPLAY TELA.
           MOSTRAR.
               ACCEPT SIGLA AT 0939 WITH PROMPT AUTO.
               DISPLAY ESPACO AT 1545.
               MOVE FUNCTION UPPER-CASE(SIGLA) TO SIGLA-E.
               PERFORM TEST AFTER VARYING LOOP FROM 1 BY 1
                                           UNTIL LOOP = 27
               IF SIGLA-T(LOOP) EQUALS SIGLA-E
                   DISPLAY "-"            AT 0942
                   DISPLAY ESTADO-T(LOOP) AT 0944
                   MOVE 1 TO TESTE
                   END-PERFORM.
               IF TESTE = 0
                   DISPLAY "ESTADO INEXISTENTE" AT 1533
                   PERFORM MOSTRAR.
               DISPLAY ESPACO AT 1533.
           CONTINUA.
               DISPLAY "CONTINUAR (S/N)? [ ]" AT 1533.
               ACCEPT OPCAO  AT 1551 WITH PROMPT AUTO.
               IF OPCAO = "S" OR "s" OR "N" OR "n"
                   DISPLAY ESPACO AT 1533
                   DISPLAY ESPACO AT 0942
               ELSE
                   DISPLAY ESPACO AT 1533
                   DISPLAY "OPCAO INVALIDA!" AT 1533
                   CALL "C$SLEEP" USING 1.
