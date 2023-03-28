       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.        
        SPECIAL-NAMES.               
           DECIMAL-POINT IS COMMA. 
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-MUJER     PIC X VALUE 'M'.
       77 WS-HOMBRE    PIC X VALUE 'H'.
       77 WS-OTRO      PIC X VALUE 'O'.
       77 WS-SEXO      PIC X.
       77 WS-EMPLE     PIC 99 VALUE ZEROS.
       77 WS-SUELDO    PIC 9(9)V99 VALUE ZEROS.
       77 WS-MAS-M    PIC 9(3) VALUE ZEROS.
       77 WS-MAS-H    PIC 9(3) VALUE ZEROS.
       77 WS-MAS-O    PIC 9(3) VALUE ZEROS.
       77 WS-MEN-M    PIC 9(3) VALUE ZEROS.
       77 WS-MEN-H    PIC 9(3) VALUE ZEROS.
       77 WS-MEN-O    PIC 9(3) VALUE ZEROS.
       77 WS-SUELDO-REF PIC 9(6)V99 VALUE 100000,00.
       01 WS-SEX        PIC X.
              88 WS-SEX-OK    VALUE "S".
              88 WS-SEX-NOK   VALUE "N".
           
       PROCEDURE DIVISION. 
       COMIENZO.
           PERFORM VARYING WS-EMPLE FROM 1 BY 1 UNTIL WS-EMPLE > 5
             SET WS-SEX-NOK TO TRUE
             PERFORM 1000-INGRESAR THRU 1000-INGRESAR-FIN UNTIL 
                                                        WS-SEX = "S"
           PERFORM 2000-PROCESO  THRU 2000-PROCESO-FIN
           END-PERFORM.  
           PERFORM 5000-MOSTRAR  THRU 5000-MOSTRAR-FIN.
           ACCEPT WS-SEXO.
           
           GOBACK.
       
       
       1000-INGRESAR.
       
           DISPLAY 
             'INGRESE SEXO  (M-MUJER H-HOMBRE O-OTRO F=FIN): '.
           ACCEPT WS-SEXO. 
           IF WS-SEXO = "F"
              GOBACK
           END-IF.
           IF WS-SEXO EQUAL WS-MUJER OR WS-SEXO EQUAL WS-HOMBRE 
                                        OR WS-SEXO  EQUAL WS-OTRO
               DISPLAY 'INGRESE EL SUELDO: '
               ACCEPT WS-SUELDO
               SET WS-SEX-OK TO TRUE
           ELSE 
              DISPLAY 
                'EL SEXO INGRESADO ES ERRONEO, INGRESELO NUEVAMENTE: '
              SET WS-SEX-NOK TO TRUE
           END-IF.
           
          
       1000-INGRESAR-FIN. EXIT.
       
       2000-PROCESO.
           
            EVALUATE WS-SEXO
           
              WHEN WS-MUJER
               IF WS-SUELDO <= WS-SUELDO-REF 
                 COMPUTE WS-MEN-M = WS-MEN-M + 1
               ELSE 
                 COMPUTE WS-MAS-M = WS-MAS-M + 1
               END-IF  
               
              WHEN WS-HOMBRE
               IF WS-SUELDO <= WS-SUELDO-REF 
                 COMPUTE WS-MEN-H = WS-MEN-H + 1
               ELSE 
                 COMPUTE WS-MAS-H = WS-MAS-H + 1
               END-IF  
               
              WHEN WS-OTRO
            
                IF WS-SUELDO <= WS-SUELDO-REF 
                 COMPUTE WS-MEN-O = WS-MEN-O + 1
               ELSE 
                 COMPUTE WS-MAS-O = WS-MAS-O + 1
               END-IF  
                
              WHEN OTHER
               DISPLAY 'EL SEXO ES ERRONEO'
            
            END-EVALUATE.  
            
           
       2000-PROCESO-FIN. EXIT.
       
       5000-MOSTRAR.
           
            DISPLAY 
              'CANT. DE MUJERES CON SUELDO MENOR O IGUAL A 100.000 : '   
                                                            WS-MEN-M .
            DISPLAY ' ' 
            
            DISPLAY 
              'CANT. DE MUJERES CON SUELDO MAYOR A 100.000 : '  
                                                            WS-MAS-M .
            DISPLAY ' ' 
            
            DISPLAY 
              'CANT. DE HOMBRE CON SUELDO MENOR O IGUAL A 100.000 : '  
                                                            WS-MEN-H .
            DISPLAY ' ' 
       
            
            DISPLAY 
              'CANTIDAD DE HOMBRE CON SUELDO MAYOR A 100.000 : '   
                                              WS-MAS-H .
            DISPLAY ' ' 
            
            
            DISPLAY 
              'CANTIDAD CON OTRO SEXO CON SUELDO <= A 100.000 : '  
                                              WS-MEN-O .
            DISPLAY ' ' 
       
          
            DISPLAY 'CANTIDAD CON OTRO SEXO CON SUELDO > A 100.000 : '
                                               WS-MAS-O .
            DISPLAY ' ' .
            
       5000-MOSTRAR-FIN. EXIT.
