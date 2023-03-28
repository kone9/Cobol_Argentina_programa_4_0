      *> CLASE 20 Ejercicio tarea CLASE 20
      *> Ariel Gimenez
      *> 21/03/2023

      *> Construir un diagrama de flujo para describir la solución y 
      *> mostrar resultado de la siguiente problemática:
      *> Obtener el promedio de las notas ingresadas al comienzo
      *>  del programa (total 15 notas ingresadas)
      *> ✔ Las primeras 5 notas corresponden al turno mañana
      *> ✔ Las siguientes 5 notas corresponden al turno tarde
      *> ✔ Las últimas 5 notas corresponden al turno noche
      *> Como resultado MOSTRAR:
      *> 1) Cada una de las notas ingresadas; 
      *> indicando el turno al cual pertenecen
      *> 2) Mostrar promedio por turno
      *> 3) Mostrar promedio general
      
      
      *> el comentario siempre empieza de la linea 7
      *> en mainframe no se puede hacer accert por consola
      *> tener en cuenta eso ya que eso.
      *> el entorno profesional es "Cobol, CICS y DB2"

      *>--------------------------------------
      *> IdentificarDatos
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJ-TR-CL20.
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
       


      *>----------------------------WORKING-----------------------------
       WORKING-STORAGE SECTION.
      
      *>  ----------------------LISTA-TURNOS----------------------------      
      *>   CREO PEQUEÑA TABLA MOSTRAR LOS TURNOS EN EL BUCLE
       01  LISTA-TURNOS.
           02 TM PIC X(10) VALUE "MANANA".
           02 TM PIC X(10) VALUE "TARDE".
           02 TM PIC X(10) VALUE "NOCHE".
       01  TABLA-LISTA-TURNOS REDEFINES LISTA-TURNOS.
           02 TURNOS PIC X(10) OCCURS 3 TIMES.

      *>   -------------------------------------------------------------  
   

      *>   ----------------LISTA-NOTAS-TURNO MANANA---------------------
       
      *>   TABLA PORCENTAJES QUE OCURRE 5 VECES CON VALOR 99 CON DECIMAL
      *>   Uso decimal point, tengo que poner coma en los puntos

       01  TABLA-NOTAS-TM.
           02 NOTAS-TM PIC 9(2)V99 OCCURS 5 TIMES.
       
      *>   ACA GUARCO LA ECUACION DE TODA LAS NOTAS
       01  PROM-TM  PIC 9(2)V99  VALUE ZERO.     
      *>   -------------------------------------------------------------    
       

      *>   -----------------LISTA-NOTAS-TURNO TARDE---------------------
       
      *>   TABLA PORCENTAJES QUE OCURRE 5 VECES CON VALOR 99 CON DECIMAL
      *>   Uso decimal point, tengo que poner coma en los puntos

       01  TABLA-NOTAS-TT.
           02 NOTAS-TT PIC 9(2)V99 OCCURS 5 TIMES.

      *>   ACA GUARCO LA ECUACION DE TODA LAS NOTAS
       01  PROM-TT  PIC 9(2)V99  VALUE ZERO. 
      *>   -------------------------------------------------------------


      *>   ---------------LISTA-NOTAS-TURNO NOCHE-----------------------
       
      *>   TABLA PORCENTAJES QUE OCURRE 5 VECES CON VALOR 99 CON DECIMAL
      *>   Uso decimal point, tengo que poner coma en los puntos

       01  TABLA-NOTAS-TM.
           02 NOTAS-TN PIC 9(2)V99 OCCURS 5 TIMES.

      *>   ACA GUARCO LA ECUACION DE TODA LAS NOTAS
       77  PROM-TN  PIC 9(2)V99  VALUE ZERO. 
      
      *>   -------------------------------------------------------------
       
      *> indice para el bucle NOTAS
       77 I-NOTAS PIC 9(2)  VALUE ZERO.
      
      *> INDICE PARA RECORRAR LOS TURNOS
       01 I_TURNO PIC 9 VALUE 0.    
      
      *> PROM-GRAL
       77 PROM-GRAL PIC 9(2)V99  VALUE ZERO.
       
      *> CONSTANTE CANTIDAD DE NOTAS PARA USAR EN LOS BUCLES
       77 CANT-NOTAS PIC 9 VALUE 5.

      *> TRES TURNOS, TURNOMAÑANA, TURNOTARDE, TURNONOCHE 
       01  C_TURNOS PIC 9  VALUE 3.
       

      *>--------------------------PROCEDURE-----------------------------
       PROCEDURE DIVISION.
      *>   todo lo que va en la procedura va a partir de la columna 12
           DISPLAY "-------------------------------------------------".
           DISPLAY "-------------------------------------------------".
           DISPLAY "AHORA VA A INGRESAR LAS NOTAS"
           DISPLAY "-------------------------------------------------".
      *>--------------------GUARDAR NOTAS-------------------------------
       
      *>   BUCLE PARA GUARDAR LAS NOTAS3
      *>   el perform seria
      *>   Ejecute "variando" VARIABLE de uno a uno hasta el valor indice
           PERFORM VARYING I_TURNO FROM 1 BY 1 UNTIL I_TURNO > C_TURNOS
                      DISPLAY "DEL TURNO " TURNOS(I_TURNO)

       *>  VARIFICO EL TURNO Y GUERDO CON BUCLE            
           IF TURNOS(I_TURNO) EQUAL "MANANA"
                PERFORM VARYING I-NOTAS FROM 1 BY 1 UNTIL I-NOTAS > 5
                      DISPLAY "ingrese la nota " I-NOTAS
                      ACCEPT NOTAS-TM(I-NOTAS)
                END-PERFORM
                DISPLAY "----------------------------------------------"
            END-IF

           IF TURNOS(I_TURNO) EQUAL "TARDE"
                PERFORM VARYING I-NOTAS FROM 1 BY 1 UNTIL I-NOTAS > 5
                      DISPLAY "ingrese la nota " I-NOTAS
                      ACCEPT NOTAS-TT(I-NOTAS)
                END-PERFORM
                DISPLAY "----------------------------------------------"
           END-IF

           IF TURNOS(I_TURNO) EQUAL "NOCHE"
                PERFORM VARYING I-NOTAS FROM 1 BY 1 UNTIL I-NOTAS > 5
                      DISPLAY "ingrese la nota " I-NOTAS
                      ACCEPT NOTAS-TN(I-NOTAS)
                END-PERFORM
                DISPLAY "----------------------------------------------"
           END-IF
                    
           END-PERFORM.
           DISPLAY "-------------------------------------------------".
           DISPLAY " ".
     
           
       
      *>--------------------MOSTRAR NOTAS-------------------------------
       
      *>   BUCLE PARA IMPRIMER LAS NOTAS TURNO MAÑANA
           PERFORM VARYING I-NOTAS FROM 1 BY 1 UNTIL I-NOTAS > 5
                      COMPUTE PROM-TM = PROM-TM + NOTAS-TM(I-NOTAS)
           END-PERFORM.
                COMPUTE PROM-TM = PROM-TM / 5
                DISPLAY "PROMEDIO TURNO MANANA " PROM-TM

      *>   BUCLE PARA IMPRIMER LAS NOTAS TURNO TARDE
           PERFORM VARYING I-NOTAS FROM 1 BY 1 UNTIL I-NOTAS > 5
                      COMPUTE PROM-TT = PROM-TT + NOTAS-TT(I-NOTAS)
           END-PERFORM.
                COMPUTE PROM-TT = PROM-TT / 5
                DISPLAY "PROMEDIO TURNO TARDE " PROM-TT            

      *>   BUCLE PARA IMPRIMER LAS NOTAS TURNO NOCHE
           PERFORM VARYING I-NOTAS FROM 1 BY 1 UNTIL I-NOTAS > 5
                      COMPUTE PROM-TN = PROM-TN + NOTAS-TN(I-NOTAS)
           END-PERFORM.
                COMPUTE PROM-TN = PROM-TN / 5
                DISPLAY "PROMEDIO TURNO NOCHE " PROM-TN               
      

           DISPLAY " ".

      *>--------------------MOSTRAR PROMEDIO GENERAL--------------------
      *>     COMPUTE PROM-GRAL = (PROM-TM + PROM-TT + PROM-TN) / 15
           COMPUTE PROM-GRAL = (PROM-TM + PROM-TT + PROM-TN) / 3
           DISPLAY "EL PROMEDIO GRAL ES " PROM-GRAL. 

           DISPLAY "-------------------------------------------------".
           DISPLAY " ".


           STOP RUN.