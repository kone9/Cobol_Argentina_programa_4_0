      *> Construir un diagrama de flujo para describir la solución y 
      *> mostrar resultado de la siguiente problemática:
      *> Deben ingresar 150 empleados a una Nueva Empresa.
      *> Se requiere saber:
      
      *> 1) Cuántos son mujeres
      *> a. Dentro de las mujeres indicar:
      *> i. cuántas ganarán más de 100.000
      *> ii. cuántas ganarán menos de 100.000 o igual a 100.000
      
      *> 2) Cuántos son varones
      *> a. Dentro de los varones indicar:
      *> i. cuántas ganarán más de 100.000
      *> ii. cuántas ganarán menos de 100.000 o igual a 100.000
      
      *> 3) Cuántos no definen su sexo
      *> a. Dentro de los no definidos indicar:
      *> i. cuántas ganarán más de 100.000
      *> ii. cuántas ganarán menos de 100.000 o igual a 100.000
      
      
      
      *> CLASE 23 Ejercicio IDENTIFICAR DATOS
      *> Ariel Gimenez
      *> 23/03/2023

      *>--------------------------------------
      *> IdentificarDatos
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJ-01ENCL23.
      *>--------------------------------------

      *>--------------------------------------
      *> es importante esto, porque sino mainframe cancela
      *> espera esta instrucción
      *> aca identifico archivos
      *> identifico el servidor
      *> el tipo de decimal 
       ENVIRONMENT DIVISION. 
      *>--------------------------------------
       
      *>--------------------------------------
      *>   es dependiendte de enviroment division sino esta 
      *>   enviroment division arriba no va a funcionar
       CONFIGURATION SECTION.
       SPECIAL-NAMES.    
           DECIMAL-POINT IS COMMA. 
      *>--------------------------------------
      
      *>--------------------------------------
       DATA DIVISION.
      *>     FILES SECTION.
      *>     INPUT OUTPUT SECTION.
      *>--------------------------------------
       

      *>----------------------------------------------------------------
      *>----------------------------WORKING-----------------------------
       WORKING-STORAGE SECTION.
       77  WS-MUJER     PIC X VALUE 'M'.
       77  WS-HOMBRE     PIC X VALUE 'H'.
       77  WS-OTRO     PIC X VALUE 'O'.
       77  WS-SEXO   PIC X.
       77  WS-EMPLE   PIC 99 VALUE ZEROS.
       77  WS-SUELDO   PIC 9(9)V99 VALUE ZEROS.
       77  WS-MAS-M   PIC 9(3) VALUE ZEROS.
       77  WS-MAS-H   PIC 9(3) VALUE ZEROS.
       77  WS-MAS-O   PIC 9(3) VALUE ZEROS.
       77  WS-MEN-M   PIC 9(3) VALUE ZEROS.
       77  WS-MEN-H   PIC 9(3) VALUE ZEROS.
       77  WS-MEN-O   PIC 9(3) VALUE ZEROS.
       

      *>CONSTANTE REF SUELDO 
       77 WS-SUELDO-REF PIC 9(6)V99 VALUE 500000,00.


      
      *> 01 WS-SEX PIC X.
      *>     88 WS-SEX-OK  PIC X VALUE "S".
      *>     88 WS-SEX-NOK PIC X VALUE "N".
      

      *> ESTRUCTURA BOOL de bandera en cobol
      *> TAMBIEN PUEDE TENER NUMEROS Y REPRESENTAR 
      *> CON CEROS Y UNOS
       01 WS-SEX PIC X.
      *>   nivel 88 siempre lleva un valor
           88 WS-SEX-OK VALUE "S".
           88 WS-SEX-NOK VALUE "N".
               

      *>----------------------------------------------------------------
      *>--------------------------PROCEDURE-----------------------------
       PROCEDURE DIVISION.
      *>   todo lo que va en la procedura va a partir de la columna 12
           DISPLAY "-------------------------------------------------".
           DISPLAY "-------------------------------------------------".
           DISPLAY "EJERCICIOS CON CANTIDAD".
           DISPLAY "-------------------------------------------------".
           
       COMIENZO.
           PERFORM VARYING WS-EMPLE FROM 1 BY 1 UNTIL WS-EMPLE > 3 
      *>   organizar el código al inico
      *>   EL true solo se puede hacer en nivel 88
               SET WS-SEX-NOK TO TRUE
               PERFORM 1000-INGRESAR THRU 1000-INGRESAR-FIN UNTIL WS-SEX 
              
               PERFORM 2000-PROCESO THRU 2000-PROCESO-FIN
          
           END-PERFORM.


           PERFORM 5000-MOSTRAR THRU 5000-MOSTRAR-FIN.
           
       
       VARIFICAR-SEXO.
           
            IF WS-SEXO EQUAL WS-MUJER OR WS-SEXO EQUAL WS-HOMBRE 
                                                OR WS-SEXO EQUAL WS-OTRO
               ACCEPT WS-SUELDO
               SET WS-SEX-OK TO TRUE
           ELSE
              DISPLAY "EL SEXO INGRESADO ES ERRONEO, INGRESALO NUEVAMENTE"
              SET WS-SEX-NOK TO TRUE

           END-IF.
       VARIFICAR-SEXO-FIN.


       1000-INGRESAR.
           DISPLAY "ingrese sexo empleado(m= mujer h=hombre o= otro)".
           ACCEPT WS-SEXO.
           PERFORM VARIFICAR-SEXO
           
           DISPLAY "ingrese sueldo"
           ACCEPT WS-SUELDO.
       1000-INGRESAR-FIN.


       2000-PROCESO.
      *>   ACA HACEMOS UN SWITCH SEGUN LA OPCION 
      *>   ELEGUIS EL TEM
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
                   COMPUTE WS-MEN-O= WS-MEN-O+ 1
               ELSE
                   COMPUTE WS-MAS-O = WS-MAS-O + 1
               END-IF   

           WHEN OTHER
               DISPLAY "el sexo es incorrecto"
               GO TO 1000-INGRESAR.
       2000-PROCESO-FIN.
    

       5000-MOSTRAR.
           DISPLAY "CANTIDAD DE MUJERES CON SULD <= 100.00: " WS-MEN-M.
           DISPLAY "CANTIDAD DE MUJERES CON SULD > 100.00: " WS-MAS-M.
           DISPLAY ""
           
           DISPLAY "CANTIDAD DE HOMBRES CON SULD <= 100.00: " WS-MEN-H.
           DISPLAY "CANTIDAD DE HOMBRES CON SULD > 100.00: " WS-MAS-H.
           DISPLAY ""

           DISPLAY "CANTIDAD DE OTROS CON SULD <= 100.00: " WS-MEN-O.
           DISPLAY "CANTIDAD DE OTROS CON SULD > 100.00: " WS-MAS-O.
           DISPLAY "".
       5000-MOSTRAR-FIN.

           STOP RUN.