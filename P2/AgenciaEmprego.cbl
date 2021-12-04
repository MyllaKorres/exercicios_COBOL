       IDENTIFICATION DIVISION.
       PROGRAM-ID. AGENCIA-EMPREGO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 DADOS.
               02 PRT-SAL   PIC 9(04)V9(02).
               02 PRT-SAL-E PIC ZZZ9,99.
               02 IDADE     PIC Z9.
               02 SEXO      PIC 9(01).
               02 SEXO-E    PIC Z.
               02 AUX       PIC 9(01).
               02 AUX-E     PIC Z.
               02 AUX2      PIC 9(04).
               02 AUX3      PIC Z.
           01 DADOS-CAR.
               02 NOME      PIC X(35)     VALUE SPACES.
               02 OPCAO     PIC X(01)     VALUE SPACE.
           01 MENSAGENS.
               02 MENSA0    PIC X(45) VALUE
               "DIGITE O NOME DO CANDIDATO".
               02 MENSA1    PIC X(45) VALUE
               "DIGITE A IDADE DO CANDIDATO".
               02 MENSA2    PIC X(45) VALUE
               "INFORME O SEXO DO CANDITADO".
               02 MENSA3    PIC X(45) VALUE
               "1 - FEMININO   2 - MASCULINO   3 - OUTRO".
               02 MENSA4    PIC X(45) VALUE
               "INFORME A PROFISSAO DO CANDIDATO".
               02 MENSA5    PIC X(45) VALUE
               "O CANDIDATO DEVE TER PELO MENOS 18 ANOS!".
               02 MENSA6    PIC X(45) VALUE
               "INFORME A PRETENSAO SALARIAL DO CANDIDATO".
               02 MENSA7    PIC X(45) VALUE
               "SELECIONE UMA OPCAO < >".
               02 MENSA8    PIC X(45) VALUE
               "ENCERRANDO PROGRAMA...".
               02 MENSA9    PIC X(45) VALUE
               "PRESSIONE ENTER PARA VOLTAR AO MENU".
               02 MENSA10   PIC X(25) VALUE SPACES.
           01 CODIGO-PROFISSAO.
               02 FILLER    PIC X(20) VALUE "CARTOGRAFO".
               02 FILLER    PIC X(20) VALUE "ASSISTENTE SOCIAL".
               02 FILLER    PIC X(20) VALUE "PSICOLOGO".
               02 FILLER    PIC X(20) VALUE "ATENDENTE".
               02 FILLER    PIC X(20) VALUE "SECRETARIA BILINGUE".
               02 FILLER    PIC X(20) VALUE "GEOLOGO".
           01 TABELA-COD-PROF REDEFINES CODIGO-PROFISSAO.
               02 PROF-T    PIC X(20) OCCURS 6 TIMES.
           01 CODIGO-SEXO.
               02 FILLER    PIC X(09) VALUE "FEMININO".
               02 FILLER    PIC X(09) VALUE "MASCULINO".
               02 FILLER    PIC X(09) VALUE "OUTRO".
           01 TABELA-SEXO REDEFINES CODIGO-SEXO.
               02 SEXO-T    PIC X(09) OCCURS 3 TIMES.
           01 DATA-DO-SISTEMA.
               02 ANO       PIC 9(02) VALUE ZEROS.
               02 MES       PIC 9(02) VALUE ZEROS.
               02 DIA       PIC 9(02) VALUE ZEROS.
       SCREEN SECTION.
           01 TELA0.
               02 BLANK SCREEN.
               02 LINE 01 COLUMN 01 PIC 9(02)/ USING DIA.
               02 LINE 01 COLUMN 04 PIC 9(02)/ USING MES.
               02 LINE 01 COLUMN 07 PIC 9(02)  USING ANO.
               02 LINE 03 COLUMN 25 VALUE
               ">>> AGENCIA DE EMPREGO <<<".
               02 LINE 07 COLUMN 20 VALUE
               "1 - CADASTRAR NOVO CANDIDATO".
               02 LINE 09 COLUMN 20 VALUE
               "2 - VISUALIZAR ULTIMO CANDIDATO".
               02 LINE 11 COLUMN 20 VALUE
               "3 - SAIR".
               02 LINE 20 COLUMN 20 VALUE
               "MENSAGEM: ".
           01 TELA1.
               02 BLANK SCREEN.
               02 LINE 01 COLUMN 01 PIC 9(02)/ USING DIA.
               02 LINE 01 COLUMN 04 PIC 9(02)/ USING MES.
               02 LINE 01 COLUMN 07 PIC 9(02)  USING ANO.
               02 LINE 03 COLUMN 25 VALUE
               ">>> CADASTRO DE CANDIDATO <<<".
               02 LINE 07 COLUMN 20 VALUE "NOME: ".
               02 LINE 09 COLUMN 20 VALUE "IDADE: ".
               02 LINE 11 COLUMN 20 VALUE "SEXO: ".
               02 LINE 13 COLUMN 20 VALUE "PRETENSAO SALARIAL: ".
               02 LINE 15 COLUMN 20 VALUE "PROFISSAO: ".
               02 LINE 20 COLUMN 20 VALUE "MENSAGEM: ".
           01 TELA2.
               02 BLANK SCREEN.
               02 LINE 01 COLUMN 01 PIC 9(02)/ USING DIA.
               02 LINE 01 COLUMN 04 PIC 9(02)/ USING MES.
               02 LINE 01 COLUMN 07 PIC 9(02)  USING ANO.
               02 LINE 03 COLUMN 25 VALUE
               ">>> DADOS CANDIDATO <<<".
               02 LINE 07 COLUMN 20 VALUE "NOME: ".
               02 LINE 09 COLUMN 20 VALUE "PROFISSAO: ".
               02 LINE 20 COLUMN 20 VALUE "MENSAGEM: ".
       PROCEDURE DIVISION.
           INICIO.
               ACCEPT  DATA-DO-SISTEMA FROM DATE.
               INITIALIZE OPCAO.
               PERFORM MENU UNTIL OPCAO = "3".
               DISPLAY MENSA8 AT 2030.
               CALL "C$SLEEP" USING 2.
               STOP RUN.
           MENU.
               DISPLAY TELA0.
               MOVE 1 TO AUX3.
               DISPLAY MENSA7 AT 2030.
               ACCEPT OPCAO AT 2051.
               IF OPCAO = "1"
                   PERFORM CADASTRAR UNTIL AUX3 <> 1
               ELSE IF OPCAO = "2"
                   PERFORM VISUALIZAR UNTIL AUX3 <> 1.
           CADASTRAR.
               DISPLAY TELA1.
               PERFORM ZERA-VARIAVEIS.
               PERFORM RECEBE-NOME UNTIL NOME <> SPACES.
               PERFORM RECEBE-IDADE UNTIL IDADE > 17.
               PERFORM RECEBE-SEXO UNTIL SEXO > 0 AND < 4.
               DISPLAY SPACES AT 2230.
               PERFORM RECEBE-PRET-SAL UNTIL PRT-SAL > 0.
               PERFORM RECEBE-PROFISSAO UNTIL AUX > 0 AND <= 6.
               MOVE 0580 TO AUX2.
               PERFORM LIMPA-TAB-PROF 7 TIMES.
               DISPLAY MENSA9 AT 2030.
               ACCEPT AUX3 AT 2065.
               MOVE SPACE TO OPCAO.
           VISUALIZAR.
               DISPLAY TELA2.
               IF AUX = 0
                   DISPLAY "--------" AT 0726
                   DISPLAY "--------" AT 0931
                   DISPLAY "NAO HA CANDIDATOS CADASTRADOS!!!" AT 1430
               ELSE
               DISPLAY NOME AT 0726
               DISPLAY PROF-T(AUX) AT 0931.
               DISPLAY MENSA9 AT 2030.
               ACCEPT AUX3 AT 2065.
               MOVE SPACE TO OPCAO.
           RECEBE-NOME.
               DISPLAY MENSA0 AT 2030.
               ACCEPT NOME AT 0726.
           RECEBE-IDADE.
               DISPLAY MENSA1 AT 2030.
               ACCEPT IDADE AT 0927.
               IF IDADE < 18
                   DISPLAY MENSA5 AT 2030
                   CALL "C$SLEEP" USING 3.
           RECEBE-SEXO.
               DISPLAY MENSA2 AT 2030.
               DISPLAY MENSA3 AT 2230.
               ACCEPT SEXO-E AT 1126.
               MOVE SEXO-E TO SEXO.
               IF SEXO <= 0 OR > 3
                   PERFORM RECEBE-SEXO.
               DISPLAY "-" AT 1128.
               DISPLAY SEXO-T(SEXO) AT 1130.
           RECEBE-PRET-SAL.
               DISPLAY MENSA6 AT 2030.
               ACCEPT PRT-SAL-E AT 1340.
               MOVE PRT-SAL-E TO PRT-SAL.
           RECEBE-PROFISSAO.
               DISPLAY MENSA4 AT 2030
               MOVE 1 TO AUX.
               DISPLAY "VAGAS DISPONIVEIS" AT 0583.
               MOVE 0780 TO AUX2.
               PERFORM TABELA-PROF 6 TIMES.
               ACCEPT AUX-E AT 1531.
               MOVE AUX-E TO AUX.
               IF AUX <= 0 OR > 6
                   PERFORM RECEBE-PROFISSAO.
               DISPLAY "-"  AT 1533.
               DISPLAY PROF-T(AUX) AT 1535.
           TABELA-PROF.
               DISPLAY AUX AT AUX2.
               ADD 4 TO AUX2.
               DISPLAY PROF-T(AUX) AT AUX2.
               ADD 1 TO AUX.
               ADD 196 TO AUX2.
           LIMPA-TAB-PROF.
               DISPLAY MENSA10 AT AUX2.
               ADD 200 TO AUX2.
           ZERA-VARIAVEIS.
               MOVE SPACES TO NOME.
               MOVE 0 TO IDADE.
               MOVE 0 TO SEXO.
               MOVE 0 TO PRT-SAL.
               MOVE 0 TO AUX.
