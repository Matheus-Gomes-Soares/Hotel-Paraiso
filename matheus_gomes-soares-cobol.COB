      ******************************************************************
      * Author:Matheus Gomes Soares
      * Date:11/06/2025
      * Purpose:Matéria da faculdade
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. paraiso_tropical.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SAIDA ASSIGN TO "saida.txt"
               ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.

       FD SAIDA.
       01 LINHA-SAIDA PIC X(100).

       WORKING-STORAGE SECTION.
       01 I-NOME              PIC A(40).
       01 I-QTD               PIC 99.
       01 I-TIPO              PIC A(1).
       01 FIM                 PIC A(1) VALUE "N".
       01 INDICE              PIC 99 VALUE 1.
       01 VALOR-TOTAL         PIC 9(10)V99.
       01 VALOR-INDIVIDUAL    PIC 9(3)V99.
       01 VALOR-DIAS          PIC 9(6)V99.
       01 NUMERICO            PIC A(1) VALUE "N".
       01 VALIDO              PIC A(1) VALUE "N".
       01 GRAVAR              PIC A(1) VALUE "N".
       01 VALOR-TEXTO         PIC X(12).
       01 VALOR-FORMATADO  PIC ZZZZZZZZZZZZ9,99.
       01 DETALHES            PIC A(1) VALUE "N".
       01 REGISTRO OCCURS 100 TIMES.

           05 REG-NOME   PIC A(40).
           05 REG-QTD    PIC 99.
           05 REG-TIPO   PIC A(1).
           05 REG-VALOR  PIC 9(6)V99.

       PROCEDURE DIVISION.



       MAIN-PROCEDURE.
           DISPLAY "SEJA BEM VINDO(A)"
           DISPLAY "ESSE PROGRAMA REGISTRA OS DADOS DE CLIENTES"
           DISPLAY "ATE 99 CLIENTES SAO ADMITIDOS"
           DISPLAY "PARA FINALIZAR DIGITE <FIM> PARA O NOME DO CLIENTE"

           PERFORM CAPTURA-REGISTRO UNTIL FIM = "S".
           PERFORM MOSTRA-REGISTROS
           PERFORM MOSTRA-DETALHES
           PERFORM GRAVAR-SAIDA
           STOP RUN.



       CAPTURA-REGISTRO.
           DISPLAY "INSIRA NOME CLIENTE " INDICE
           ACCEPT I-NOME
           MOVE FUNCTION UPPER-CASE(I-NOME) TO I-NOME
           IF I-NOME = "FIM"
               MOVE "FIM" TO REG-NOME(INDICE)
               MOVE "S" TO FIM
           ELSE
                PERFORM CAPTURA-DIAS
                PERFORM CAPTURA-TIPO
                PERFORM CALCULA-PRECO
           END-IF.

       CAPTURA-TIPO.
           PERFORM UNTIL VALIDO = "S"
           DISPLAY "QUAL O TIPO DE QUARTO?"
           DISPLAY "S = SIMPLES : R$ 100,00 POR DIA"
           DISPLAY "D = DUPLO   : R$ 150,00 POR DIA-> "
           DISPLAY "L = LUXO    : R$ 250,00 POR DIA"

           ACCEPT I-TIPO
           MOVE FUNCTION UPPER-CASE(I-TIPO) TO I-TIPO
           IF I-TIPO = "S" OR I-TIPO = "D" OR I-TIPO = "L"
               MOVE "S" TO VALIDO
           END-IF
           END-PERFORM
           MOVE "N" TO VALIDO.
      *MAIOR QUE 0 É USADO TANTO PARA VERIFICAR SE É NUMERICO, TANTO SE É MAIOR QUE 0
      *JÁ QUE NÃO FAZ SENTIDO SALVAR SE FOR MENOR
       CAPTURA-DIAS.
           DISPLAY "QUANTOS DIAS DESEJA HOSPEDAR-SE?"
           PERFORM UNTIL NUMERICO = "S"
           ACCEPT I-QTD
           IF I-QTD > 0
               MOVE "S" TO NUMERICO
           ELSE
               DISPLAY "DIGITE UM VALOR NUMERICO MAIOR QUE 0"
           END-IF

           END-PERFORM
           MOVE "N" TO NUMERICO.



       CALCULA-PRECO.
            EVALUATE I-TIPO
                    WHEN "S"
                        MOVE 100 TO VALOR-INDIVIDUAL
                    WHEN "D"
                        MOVE 150 TO VALOR-INDIVIDUAL
                    WHEN "L"
                        MOVE 250 TO VALOR-INDIVIDUAL
                END-EVALUATE

               COMPUTE VALOR-DIAS = VALOR-INDIVIDUAL*I-QTD
               IF I-QTD > 7
                   COMPUTE VALOR-DIAS = VALOR-DIAS * 0.9
               END-IF
               PERFORM SALVA-REGISTRO.


       SALVA-REGISTRO.
           MOVE I-NOME TO REG-NOME(INDICE)
                MOVE I-QTD TO REG-QTD(INDICE)
                MOVE I-TIPO TO REG-TIPO(INDICE)
                MOVE VALOR-DIAS TO REG-VALOR(INDICE)
                ADD VALOR-DIAS TO VALOR-TOTAL
                ADD 1 TO INDICE
                IF INDICE > 99
                   DISPLAY "LIMITE DE REGISTROS ATINGIDO"
                   MOVE "S" TO FIM
                END-IF.
       MOSTRA-REGISTROS.
            DISPLAY "REGISTROS SALVOS:"
               MOVE 1 TO INDICE


           PERFORM UNTIL REG-NOME(INDICE) = "FIM"
           DISPLAY
           "CLIENTE: " REG-NOME(INDICE) "VALOR: " REG-VALOR(INDICE)
           ADD 1 TO INDICE
           END-PERFORM
           DISPLAY " "
           DISPLAY "VALOR TOTAL :  R$ " VALOR-TOTAL.

       MOSTRA-DETALHES.
           DISPLAY "MOSTRAR DETALHES? (S)IM"
           ACCEPT DETALHES
           MOVE FUNCTION UPPER-CASE(DETALHES) TO DETALHES
           IF DETALHES = "S"
               PERFORM MOSTRA-REGISTROS-DETALHADOS
           END-IF.


       MOSTRA-REGISTROS-DETALHADOS.
           DISPLAY "REGISTROS SALVOS:"
               MOVE 1 TO INDICE


           PERFORM UNTIL REG-NOME(INDICE) = "FIM"
           DISPLAY "CLIENTE: " REG-NOME(INDICE)
           DISPLAY "QUANTIDADE DE DIAS " REG-QTD(INDICE)
           DISPLAY "TIPO  : "  REG-TIPO(INDICE)

           DISPLAY "PRECO  : R$ " REG-VALOR(INDICE)
           DISPLAY "-------------------------------"
           ADD 1 TO INDICE
           END-PERFORM
           DISPLAY "VALOR TOTAL :  R$ " VALOR-TOTAL.


       GRAVAR-SAIDA.
           DISPLAY "DESEJA GRAVAR SAIDA? (S)IM"
           ACCEPT GRAVAR
           MOVE FUNCTION UPPER-CASE(GRAVAR) TO GRAVAR
           IF GRAVAR = "S"

               MOVE 1 TO INDICE
               OPEN OUTPUT SAIDA
               PERFORM UNTIL REG-NOME(INDICE) = "FIM"

               STRING "CLIENTE: " REG-NOME(INDICE) INTO LINHA-SAIDA
               WRITE LINHA-SAIDA
               MOVE SPACES TO LINHA-SAIDA

               STRING "QUANTIDADE DE DIAS " REG-QTD(INDICE)
               INTO LINHA-SAIDA
               WRITE LINHA-SAIDA
               MOVE SPACES TO LINHA-SAIDA

               STRING "TIPO  : "  REG-TIPO(INDICE) INTO LINHA-SAIDA
               WRITE LINHA-SAIDA
               MOVE SPACES TO LINHA-SAIDA

               MOVE SPACES TO VALOR-TEXTO
               STRING REG-VALOR(INDICE) INTO VALOR-TEXTO
               MOVE VALOR-TEXTO TO VALOR-FORMATADO
               STRING "PRECO  : R$ " VALOR-FORMATADO INTO LINHA-SAIDA
               WRITE LINHA-SAIDA
               MOVE SPACES TO LINHA-SAIDA

               STRING "-------------------------------"
               INTO LINHA-SAIDA
               WRITE LINHA-SAIDA
               MOVE SPACES TO LINHA-SAIDA

               ADD 1 TO INDICE
               END-PERFORM

               MOVE SPACES TO VALOR-TEXTO
               STRING VALOR-TOTAL INTO VALOR-TEXTO
               MOVE VALOR-TEXTO TO VALOR-FORMATADO
               STRING "VALOR TOTAL :  R$ " VALOR-FORMATADO
               INTO LINHA-SAIDA
               WRITE LINHA-SAIDA
               CLOSE SAIDA
               DISPLAY "GRAVACAO CONCLUIDA"
           END-IF.
