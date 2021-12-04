       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABELA-MESES-DO-ANO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 OPCAO        PIC X(01)   VALUE SPACE.
           01 MESES-ANO.
               02 FILLER   PIC X(09)   VALUE "Janeiro".
               02 FILLER   PIC X(09)   VALUE "Fevereiro".
               02 FILLER   PIC X(09)   VALUE "Marco".
               02 FILLER   PIC X(09)   VALUE "Abril".
               02 FILLER   PIC X(09)   VALUE "Maio".
               02 FILLER   PIC X(09)   VALUE "Junho".
               02 FILLER   PIC X(09)   VALUE "Julho".
               02 FILLER   PIC X(09)   VALUE "Agosto".
               02 FILLER   PIC X(09)   VALUE "Setembro".
               02 FILLER   PIC X(09)   VALUE "Outubro".
               02 FILLER   PIC X(09)   VALUE "Novembro".
               02 FILLER   PIC X(09)   VALUE "Dezembro".
           01 TABELA-MESES REDEFINES   MESES-ANO.
               02 MES-T    PIC X(09)   OCCURS 12 TIMES.
           01 DATA-QUALQUER.
               02 DIA      PIC 9(02)   VALUE ZEROS.
               02 MES      PIC 9(02)   VALUE ZEROS.
               02 ANO      PIC 9(04)   VALUE ZEROS.
               02 DIA-E    PIC Z9.
               02 MES-E    PIC Z9.
               02 ANO-E    PIC ZZZ9.
               02 DATA-COMPLETA PIC X(25) VALUE SPACES.
           01 MENSAGEM.
               02 MENSA0   PIC X(30) VALUE SPACES.
               02 MENSA1   PIC X(30) VALUE "DIA INVALIDO".
               02 MENSA2   PIC X(30) VALUE "MES INVALIDO".
               02 MENSA3   PIC X(30) VALUE "ANO INVALIDO".
       SCREEN SECTION.
           01 TELA.
               02 BLANK SCREEN.
               02 LINE 03 COLUMN 23 VALUE
               ">>> UTILIZANDO A TABELA MESES DO ANO <<<".
               02 LINE 09 COLUMN 20 VALUE
               "Insira data no formato dd/mm/aaaa: ".
               02 LINE 09 COLUMN 55 VALUE "__/__/____".
               02 LINE 15 COLUMN 25 VALUE "MENSAGEM: ".
       PROCEDURE DIVISION.
           INICIO.
               INITIALIZE OPCAO.
               INITIALIZE DIA.
               INITIALIZE MES.
               INITIALIZE ANO.
               PERFORM CORPO UNTIL OPCAO = "N" OR "n".
               DISPLAY "FIM DE PROGRAMA!" AT 1833.
               CALL "C$SLEEP" USING 2.
               STOP RUN.
           CORPO.
               PERFORM ABERTURA.
               PERFORM RECEBE-DIA UNTIL DIA >= 1 AND <= 31.
               IF DIA  = 31
                   PERFORM RECEBE-MES UNTIL MES = 1 OR = 3
                   OR  = 5 OR = 7 OR = 8 OR = 10 OR = 12
               ELSE IF DIA > 29
                   PERFORM RECEBE-MES UNTIL MES >= 1 AND <= 12 AND <> 2
               ELSE
                   PERFORM RECEBE-MES UNTIL MES >= 1 AND <= 12.
               PERFORM RECEBE-ANO UNTIL ANO > 0.
               PERFORM MOSTRA.
               PERFORM CONTINUA UNTIL OPCAO = "N" OR "n".
           ABERTURA.
               DISPLAY TELA AT 0101.
           RECEBE-DIA.
               ACCEPT  DIA-E AT 0955 WITH PROMPT AUTO.
               MOVE    DIA-E TO DIA.
               IF DIA < 1 OR > 31
                   DISPLAY MENSA1 AT 1535
               ELSE
                   DISPLAY MENSA0 AT 1535.
           RECEBE-MES.
               ACCEPT  MES-E AT 0958 WITH PROMPT AUTO.
               MOVE    MES-E TO MES.
               IF MES < 1 OR > 12
                   DISPLAY MENSA2 AT 1535
               ELSE
                   DISPLAY MENSA0 AT 1535.
           RECEBE-ANO.
               ACCEPT  ANO-E AT 0961 WITH PROMPT AUTO.
               MOVE    ANO-E TO ANO.
               IF ANO <= 0
                   DISPLAY MENSA3 AT 1535
               ELSE
                   DISPLAY MENSA0 AT 1535.
           STRING
              DIA                    DELIMITED BY SPACES
              " de "                 DELIMITED BY SIZE
              MES-T(MES)             DELIMITED BY SPACES
              " de "                 DELIMITED BY SIZE
              ANO                    DELIMITED BY SPACES
              "                  "   DELIMITED BY SIZE
              INTO DATA-COMPLETA.
           MOSTRA.
               DISPLAY "Data por extenso: " AT 1220.
               DISPLAY  DATA-COMPLETA AT 1238.
           CONTINUA.
               DISPLAY "Continua (S/N)?" AT 1535.
               ACCEPT  OPCAO AT 1551 WITH PROMPT AUTO.
               IF      OPCAO = "S" or "s"
                   MOVE ZEROS TO DATA-QUALQUER
                   PERFORM INICIO.
