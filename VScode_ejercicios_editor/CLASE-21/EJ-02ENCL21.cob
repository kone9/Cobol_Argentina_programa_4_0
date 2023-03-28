      *> CLASE 21 Ejercicio CLASE tarea 19 

      *>Construir un diagrama de flujo para describir la solución 
      *> y mostrar resultado de la siguiente problemática:
      *> Se ingresan los primeros 10 números impares (1; 3; 5; ……..19)
      *> Tomarlos de a pares; en forma consecutiva; y:
      *> 1) El primer par: sumarlos y mostrar resultado dentro 
      *> de la segunda variable
      *> 2) El segundo par: multiplicarlos y mostrar resultado 
      *> en una tercer variable
      *> 3) El tercer par: dividirlos y mostrar resultado con decimales.
      *> 4) El cuarto par: dividirlos y mostrar el 
      *> resultado sin decimales.
      *> 5) El quinto par: restar el último número del anterior y mostrar 
      *> el resultado en una tercer variable
      *> EN TODOS LOS CASOS; EL RESULTADO EN VARIABLES EDITABLES CON COMA
      *> DECIMAL Y ELIMINACIÓN DE CEROS NO SIGNIFICATIVOS. GRACIAS
      *> Ariel Gimenez
      *> 22/03/2023

      *>--------------------------------------
      *> IdentificarDatos
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJ-02ENCL21.
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
      *>    
      *>--------------------------------------
      
      *>--------------------------------------
       DATA DIVISION.
      *>     FILES SECTION.
      *>     INPUT OUTPUT SECTION.
      *>--------------------------------------
       

      *>----------------------------------------------------------------
      *>----------------------------WORKING-----------------------------
       WORKING-STORAGE SECTION.
       
       01 IMPARES.
          02 FILLER PIC 99 VALUE 01. 
          02 FILLER PIC 99 VALUE 02. 
          02 FILLER PIC 99 VALUE 03. 
          02 FILLER PIC 99 VALUE 04. 
          02 FILLER PIC 99 VALUE 05. 
          02 FILLER PIC 99 VALUE 06. 
          02 FILLER PIC 99 VALUE 07. 
          02 FILLER PIC 99 VALUE 08. 
          02 FILLER PIC 99 VALUE 09. 
          02 FILLER PIC 99 VALUE 10. 
          02 FILLER PIC 99 VALUE 11. 
          02 FILLER PIC 99 VALUE 12. 
          02 FILLER PIC 99 VALUE 13. 
          02 FILLER PIC 99 VALUE 14. 
          02 FILLER PIC 99 VALUE 15. 
          02 FILLER PIC 99 VALUE 16. 
          02 FILLER PIC 99 VALUE 17. 
          02 FILLER PIC 99 VALUE 18. 
          02 FILLER PIC 99 VALUE 19. 
       01 TABLA-IMPARES REDEFINES IMPARES.
           02 IMPAR PIC 99 OCCURS 10 TIMES.


       01 PRODUCTO PIC 9999.
       01 COCIENTE PIC 9999V999.
       01 COCIENTE-ENTERO PIC 9999.
       01 DIFERENCIA PIC S99.
      *> NO FUNCIONA EL GUION EN ESTA PLATAFORMA
      *> 01 EDITABLE PIC ZZZ9,99-.
      *> 01 EDITABLE PIC -ZZZ9,99.
      *> 01 EDITABLE PIC -ZZZ9,99CR.
       01 EDITABLE PIC ZZZ9,99.


      *>----------------------------------------------------------------
      *>--------------------------PROCEDURE-----------------------------
       PROCEDURE DIVISION.
      *>   todo lo que va en la procedura va a partir de la columna 12
           DISPLAY "-------------------------------------------------".
           DISPLAY "-------------------------------------------------".
           DISPLAY "EJERCICIOS CON NUMEROS PARES E IMPARES"
           DISPLAY "-------------------------------------------------".        


       PRIMER-PAR.
           ADD IMPAR(1) TO IMPAR(2).
       SEGUNDO-PAR.
           MULTIPLY IMPAR(3) BY IMPAR(4) GIVING PRODUCTO.
       TERCER-PAR.
           DIVIDE IMPAR(6) BY IMPAR(5) GIVING COCIENTE.
       CUARTO-PAR.
           DIVIDE IMPAR(7) BY IMPAR(8) GIVING COCIENTE-ENTERO.
       QUINTO-PAR.
           SUBTRACT IMPAR(10) FROM IMPAR(9) GIVING DIFERENCIA.


      *>   RESULTADOS
           MOVE IMPAR(2) TO EDITABLE.
           DISPLAY "IMPAR " EDITABLE.
           MOVE PRODUCTO TO EDITABLE.
           DISPLAY "PRODUCTO " EDITABLE.
           MOVE COCIENTE TO EDITABLE.
           DISPLAY "COCIENTE " EDITABLE.
           MOVE COCIENTE-ENTERO TO EDITABLE.
           DISPLAY "COCIENTE-ENTERO  " EDITABLE.
           MOVE DIFERENCIA TO EDITABLE.
           DISPLAY "DIFERENCIA  -" EDITABLE.

           STOP RUN.