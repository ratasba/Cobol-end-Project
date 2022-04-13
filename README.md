# Cobol-end-Project

 IDENTIFICATION                  DIVISION.                  
 PROGRAM-ID.                     AS08P015.                  
 AUTHOR.         GABRIEL SANTOS DE CAMPOS.                  
*                                                           
*---------------------------------------------------------* 
* OBJETIVO DO PROGRAMA: SALDO FINAL EM CC DO CLIENTE        
*---------------------------------------------------------* 
*                                                           
 ENVIRONMENT                     DIVISION.                  
 CONFIGURATION                    SECTION.                  
 SPECIAL-NAMES.                                             
     DECIMAL-POINT IS COMMA.                                
*                                                           
 DATA                            DIVISION.                  
*---------------------------------------------------------* 
* SECAO DE MEMORIA DO PROGRAMA PARA CRIAR VARIAVEIS             
*---------------------------------------------------------*     
 WORKING-STORAGE                 SECTION.                       
*                                                               
 01 WRK-DADOS-ENTRADA-INICIAL.                                  
     05 WRK-I-COD-AGENCIA            PIC X(003).                
     05 WRK-I-NUM-CC                 PIC 9(006)     VALUE ZEROS.
     05 WRK-I-NOME-CLIENTE           PIC X(020).                
     05 WRK-I-SALDO-INICIAL          PIC 9(009)V99  VALUE ZEROS.
     05 WRK-I-MES-ANO-MOVIMENTO.                                
         07 WRK-I-MES                PIC 9(002)     VALUE ZEROS.
         07 WRK-I-ANO                PIC 9(004)     VALUE ZEROS.
     05 WRK-I-PERC-JUROS             PIC 9(002)V99  VALUE ZEROS.
     05 WRK-I-PERC-RENDIMENTO        PIC 9(002)V99  VALUE ZEROS.
*                                                               
 01 WRK-DADOS-ENTRADA-MOVIMENTO.                                 
     05 WRK-M-COD-AGENCIA            PIC X(003).                 
     05 WRK-M-NUM-CC                 PIC 9(006)     VALUE ZEROS. 
     05 WRK-M-VL-MOVIMENTO           PIC 9(009)V99  VALUE ZEROS. 
     05 WRK-M-TP-MOVIMENTO           PIC X(001)     VALUE ZEROS. 
     05 WRK-M-DT-MOVIMENTO.                                      
         07 WRK-M-DIA                PIC 9(002)     VALUE ZEROS. 
         07 FILLER                   PIC X(01).                  
         07 WRK-M-MES                PIC 9(002)     VALUE ZEROS. 
         07 FILLER                   PIC X(01).                  
         07 WRK-M-ANO                PIC 9(004)     VALUE ZEROS. 
     05 WRK-M-DESCRICAO-MOVIMENTO    PIC X(020).                 
*                                                                
 01  WRK-DADOS-SAIDA-CABEC.                                      
     03 FILLER                   PIC X(10) VALUE 'DATA MOVTO'.   
     03 FILLER                   PIC X(10) VALUE 'DATA MOVTO'.    
     03 FILLER                   PIC X(01) VALUE ';'.             
     03 FILLER                   PIC X(20) VALUE                  
                                       'DESCRICAO MOVIMENTO '.    
     03 FILLER                   PIC X(01) VALUE ';'.             
     03 FILLER                   PIC X(14) VALUE ' SALDO INICIAL'.
     03 FILLER                   PIC X(01) VALUE ';'.             
     03 FILLER                   PIC X(14) VALUE ' VL.MOV.DEBITO'.
     03 FILLER                   PIC X(01) VALUE ';'.             
     03 FILLER                   PIC X(15) VALUE                  
                                                ' VL.MOV.CREDITO'.
     03 FILLER                   PIC X(01) VALUE ';'.             
     03 FILLER                   PIC X(14) VALUE '   SALDO FINAL'.
     03 FILLER                   PIC X(01) VALUE ';'.             
     03 FILLER                   PIC X(16) VALUE                  
                                               ' VL.JUROS DIARIO'.
     03 FILLER                   PIC X(01) VALUE ';'.             
     03 FILLER                   PIC X(15) VALUE                  
                                                ' VL.REND DIARIO'.
     03 FILLER                   PIC X(01) VALUE ';'.             
*                                                                 
 01  WRK-DADOS-SAIDA-DETALHE.                                     
     03 WRK-S-DT-MOVTO           PIC X(10).                       
         05 WRK-S-DIA            PIC 9(002).                      
         05 FILLER               PIC X(01) VALUE '/'.             
         05 WRK-S-MES            PIC 9(002).                      
         05 FILLER               PIC X(01) VALUE '/'.             
         05 WRK-S-ANO            PIC 9(004).                      
     03 FILLER                   PIC X(01) VALUE ';'.             
     03 WRK-S-DESCRICAO-MOVTO    PIC X(20) VALUE.                 
     03 FILLER                   PIC X(01) VALUE ';'.             
     03 WRK-S-SALDO-INICIAL      PIC ZZZ.ZZZ.ZZ9,99.           
     03 FILLER                   PIC X(01) VALUE ';'.          
     03 WRK-S-VL-MOV-DEBITO      PIC ZZZ.ZZZ.ZZ9,99.           
     03 FILLER                   PIC X(01) VALUE ';'.          
     03 WRK-S-VL-MOV-CREDITO     PIC ZZZ.ZZZ.ZZ9,99.           
     03 FILLER                   PIC X(01) VALUE ';'.          
     03 WRK-S-SALDO-FINAL        PIC ZZZ.ZZZ.ZZ9,99.           
     03 FILLER                   PIC X(01) VALUE ';'.          
     03 WRK-S-VL-JUROS-DIARIO    PIC ZZZ.ZZZ.ZZ9,99.           
     03 FILLER                   PIC X(01) VALUE ';'.          
     03 WRK-S-VL-REND-DIARIO     PIC ZZZ.ZZZ.ZZ9,99.           
     03 FILLER                   PIC X(01) VALUE ';'.          
*                                                              
 01 WRK-VARIAVEIS-CALCULO.                                     
    05 WRK-SALDO-FINAL              PIC S9(09)V99  VALUE ZEROS.
    05 WRK-VL-JUROS-DIARIO          PIC S9(11)V9(05).            
    05 WRK-VL-REND-DIARIO           PIC S9(11)V9(05).            
    05 WRK-VL-LIQUIDO               PIC S9(11)V9(05).            
*                                                                
 01 WRK-MASCARAS.                                                
     03  WRK-MASC-JR-REND               PIC +++.+++.++9,99       
                                        BLANK WHEN ZERO.         
     03  WRK-MASC-NUM-CC                PIC ZZZ.ZZ9.             
     03  WRK-MASC-SALDO                 PIC +++.+++.++9,99       
                                        BLANK WHEN ZERO.         
                                                                 
*---------------------------------------------------------*      
* DIVISAO DO PROGRAMA PARA ESCREVER INSTRUCOES COBOL             
*---------------------------------------------------------*      
 PROCEDURE                       DIVISION.                       
 0000-PRINCIPAL.                                                 
     PERFORM  1000-INICIALIZAR                                   
*                                                                
     PERFORM  2000-PROCESSAR UNTIL WRK-DADOS-ENTRADA-MOVIMENTO   
                             EQUAL 'FIM'                         
*                                                                
     PERFORM  3000-FINALIZAR                                     
*                                                                
     STOP RUN                                                    
     .                                                           
 1000-INICIALIZAR.                                               
     DISPLAY 'AS08P015 - GABRIEL SANTOS DE CAMPOS'               
     DISPLAY 'PONTO DE MELHORIA: DETALHES.......................'
     DISPLAY 'ACAO  DE MELHORIA: OLHAR DETALHADAMENTE MEUS PROG.'
     DISPLAY '--------------------------------------------------'
*                                                               
     PERFORM 1100-INICIALIZAR-VARIAVEIS                         
     PERFORM 1200-ACEITAR-DADOS-ENTRADA                         
     PERFORM 1300-MOSTRAR                                       
     DISPLAY WRK-DADOS-SAIDA-CABEC.                             
     .                                                          
 1100-INICIALIZAR-VARIAVEIS.                                    
     INITIALIZE WRK-VARIAVEIS-CALCULO                           
     INITIALIZE WRK-MASCARAS                                    
     .                                                          
 1200-ACEITAR-DADOS-ENTRADA.                                    
     ACCEPT WRK-DADOS-ENTRADA-INICIAL                           
     MOVE WRK-I-SALDO-INICIAL       TO WRK-SALDO-FINAL          
     ACCEPT WRK-DADOS-ENTRADA-MOVIMENTO                         
     .                                                         
1300-MOSTRAR.                                                    
     MOVE WRK-I-NUM-CC              TO WRK-MASC-NUM-CC            
     DISPLAY 'SALDO FINAL EM CC DO CLIENTE: ' WRK-I-COD-AGENCIA   
     '/' WRK-MASC-NUM-CC '/' WRK-I-NOME-CLIENTE                   
     DISPLAY WRK-DADOS-SAIDA-CABEC                                
     .                                                            
 2000-PROCESSAR.                                                  
     IF  (WRK-I-COD-AGENCIA         EQUAL WRK-M-COD-AGENCIA) AND  
         (WRK-I-NUM-CC              EQUAL WRK-M-NUM-CC)      AND  
         (WRK-I-MES                 EQUAL WRK-M-MES)         AND  
         (WRK-I-ANO                 EQUAL WRK-M-ANO)              
         PERFORM 2100-CALCULAR-SALDO                              
         PERFORM 2200-JUROS-REND                                  
         PERFORM 2300-MOVES                                       
         DISPLAY WRK-DADOS-SAIDA-DETALHE                          
     END-IF                                                       
     ACCEPT WRK-DADOS-ENTRADA-MOVIMENTO                           
     .                                                            
 2100-CALCULAR-SALDO.                                             
     IF  WRK-M-TP-MOVIMENTO         EQUAL 'C'                     
         ADD WRK-M-VL-MOVIMENTO     TO WRK-SALDO-FINAL            
     ELSE                                                         
         IF  WRK-M-TP-MOVIMENTO     EQUAL 'D'                     
             SUBTRACT WRK-M-VL-MOVIMENTO                          
                                    FROM WRK-SALDO-FINAL          
         END-IF                                                   
     END-IF                                                       
     .                                                            
 2200-JUROS-REND.                                                 
     IF  WRK-SALDO-FINAL            IS NEGATIVE                   
         COMPUTE WRK-VL-JUROS-DIARIO                              
                                    ROUNDED =                     
                                       (WRK-SALDO-FINAL *         
                                      (WRK-I-PERC-JUROS / 30))    
                                       / 100                      
     END-IF                                                       
     IF  WRK-SALDO-FINAL            IS POSITIVE                   
         COMPUTE WRK-VL-REND-DIARIO ROUNDED = (WRK-SALDO-FINAL *  
                                    (WRK-I-PERC-RENDIMENTO / 30)) 
                                     / 100                        
     END-IF                                                       
     .                                                            
 2300-MOVES.                                                      
     MOVE WRK-M-DIA                 TO WRK-S-DIA                  
     MOVE WRK-M-MES                 TO WRK-S-MES                  
     MOVE WRK-M-ANO                 TO WRK-S-ANO                  
     MOVE WRK-M-DESCRICAO-MOVIMENTO TO WRK-S-DESCRICAO-MOVTO      
     MOVE WRK-I-SALDO-INICIAL       TO WRK-S-SALDO-INICIAL        
     MOVE                           TO WRK-S-VL-MOV-DEBITO        
     MOVE                           TO WRK-S-VL-MOV-CREDITO       
     MOVE                           TO WRK-S-SALDO-FINAL          
     MOVE                           TO WRK-S-VL-JUROS-DIARIO      
     MOVE                           TO WRK-S-VL-REND-DIARIO       
     .                                                            
 3000-FINALIZAR.                                                  
     MOVE WRK-VL-JUROS-DIARIO       TO WRK-MASC-JR-REND           
     DISPLAY 'VALOR TOTAL JUROS DIARIO........: ' WRK-MASC-JR-REND
     MOVE WRK-VL-REND-DIARIO        TO WRK-MASC-JR-REND           
     DISPLAY 'VALOR TOTAL RENDIMENTO DIARIO...: ' WRK-MASC-JR-REND
     COMPUTE WRK-VL-LIQUIDO         ROUNDED = WRK-VL-REND-DIARIO  
                                    - WRK-VL-JUROS-DIARIO         
     MOVE WRK-VL-LIQUIDO            TO WRK-MASC-JR-REND           
     DISPLAY 'VALOR LIQUIDO RENDIMENTO - JUROS: ' WRK-MASC-JR-REND
     .                                                            
