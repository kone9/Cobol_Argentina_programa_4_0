      *> CLASE 21 Ejercicio CLASE PARRAFOS EN BUCLES
      *> Ariel Gimenez
      *> 22/03/2023

      *>--------------------------------------
      *> IdentificarDatos
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJ-01ENCL21.
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
       
      *>   INDICE PARA EL BUCLE
       77 INDICE PIC 99 VALUE ZERO.
       
      *>   ESTO LO USO EDGARDO EN SU CÓDIGO
       01  123-PRUEBA           PIC X.





      *>----------------------------------------------------------------
      *>--------------------------PROCEDURE-----------------------------
       PROCEDURE DIVISION.
      *>   todo lo que va en la procedura va a partir de la columna 12
           DISPLAY "-------------------------------------------------".
           DISPLAY "-------------------------------------------------".
           DISPLAY "EJERCICIOS CON PARRAFOS"
           DISPLAY "-------------------------------------------------".
           

      *>   EL PARRAFO COMIENZA EN LA LINEA 8 
      *>   EL PERFORM SE PUEDE USAR PARA SER LLAMADO DE UN PARRAFO

      *>   LLAMANDO AL PARRAFO PRUEBA CON PERFORM
      *>   LO SABE PORQUE EMPIEZA EN LA COLUMNA 8
      *>   8 "ESPACIOS DESDE EL BORDE"
      *>   VA DONDE INICIA EL PARRAFO, DONDE TERMINA EL PARRAFO
           PERFORM PARRAFO-PERFORM THRU PARRAFO-PERFORM-FIN.


      *>   DECLARACION DE PARRAFO CON PERFORM, LO SABE
      *>   PORQUE EMPIEZA EN LA COLUMNA 8
      *>   8 "ESPACIOS DESDE EL BORDE"
      *>   COMO SE VE EL PARRAFO ABRE 
      *>   Y EL PARARFO CIERRA
       PARRAFO-PERFORM.  
           PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 5
                 DISPLAY "PARRAFO EN PERFORM"
           END-PERFORM.
       PARRAFO-PERFORM-FIN.
      *>   CUANDO TERMINA EL BUCLE CON EL PERFORM VA EL EXIT
           EXIT.


           DISPLAY "----------EDGARDO PRUEBA THRU---------------------".
           DISPLAY "--------------------------------------------------".
     
      *>   CUANDO HAY UN PERFORM SIN THRU VA A EJECUTAR 
      *>   HASTA QUE SE CRUCE CON UN PARRAFO
      *>   Y LUEGO VA A VOLVER A EJECUTARSE EL PERFORM
      *>   POR ESO ES NECESARIO EL PARRAFO FINAL
      *>   EN ESTE CASO HAY UN 222 Y COMO HAY UN PARRAFO TERMINA
      *>   INVESTIGAR UN POCO MEJOR ESO


      *>   EN COBOL PARA DECIRLE QUE PARE UN PROGRAMA TENGO 
      *>   STOP RUN
      *>   GOBACK
       010-INICIO.
           PERFORM 0100-PRIMERO.
           GOBACK.

       0100-PRIMERO.  
           DISPLAY "11111111111111111111111".

       0105-XX.
           DISPLAY "22222222222222222"
           ACCEPT 123-PRUEBA.
       FIN-PRIMERO.


           STOP RUN.