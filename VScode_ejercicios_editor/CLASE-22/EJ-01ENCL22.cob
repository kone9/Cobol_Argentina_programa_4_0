      *> Construir un diagrama de flujo para describir la solución y 
      *> mostrar resultado de la siguiente problemática:
      *> Al comienzo del año lectivo de UBA Ciencias Económicas; 
      *> se publicaron las materias para cursar durante 
      *> el primer cuatrimestre, a saber:
      *> 1)ANÁLISIS MATEMÁTICO I – cupo 115 estudiantes
      *> 2) DERECHO ADMINISTRATIVO I – cupo 250 estudiantes
      *> 3) IMPUESTOS I cupo 230 estudiantes
      *> Sabiendo que se presentaron 150 personas para la primera tanda 
      *> de inscripción; relevar
      *> cuántos se inscribieron en cada materia.
      *> ¿Hubo algún conflicto de cupo? Indicarlo y justificarlo desde el
      *>  diagrama de lógica
      
      
      
      *> CLASE 22 Ejercicio CLASE CANTIDAD INSCRIPTOS MATERIAS
      *> Ariel Gimenez
      *> 23/03/2023

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
      *>SPECIAL-NAMES.    
      *>     DECIMAL-POINT IS COMMA. 
      *>--------------------------------------
      
      *>--------------------------------------
       DATA DIVISION.
      *>     FILES SECTION.
      *>     INPUT OUTPUT SECTION.
      *>--------------------------------------
       

      *>----------------------------------------------------------------
      *>----------------------------WORKING-----------------------------
       WORKING-STORAGE SECTION.
       
      *>   MATERIAS CUPO
       01 CUPO-ANALISIS PIC 99 VALUE 5.
       01 CUPO-DERECHO PIC 99 VALUE 7.
       01 CUPO-IMPUESTOS PIC 99 VALUE 8.
      
      *>   MATERIAS CUPO
       01 CONT-ANALISIS PIC 99 VALUE ZERO.
       01 CONT-DERECHO PIC 99 VALUE ZERO.
       01 CONT-IMPUESTOS PIC 99 VALUE ZERO.
      
      *>   CONFLICTO 1 ES CONFLICTO 0 NO HAY CONFLICTO
       01 CONFLIC-ANALISIS PIC 9 VALUE ZERO.
       01 CONFLIC-DERECHO PIC  9 VALUE ZERO.
       01 CONFLIC-IMPUESTOS PIC 9 VALUE ZERO.
       
       01 ALUMNO PIC 999.
       
      *> MATERIA ANOTADA A,D,I
       01 MATERIA-ANOTADA PIC X. 
       
      *>  CONDICION ROMPER BUCLE 0 SEGUIR 1 NO SEGUIR
       01 TERMINAR-BUCLE PIC 9 VALUE 0.


      *>----------------------------------------------------------------
      *>--------------------------PROCEDURE-----------------------------
       PROCEDURE DIVISION.
      *>   todo lo que va en la procedura va a partir de la columna 12
           DISPLAY "-------------------------------------------------".
           DISPLAY "-------------------------------------------------".
           DISPLAY "EJERCICIOS CON CANTIDAD ALUMNOS INSCRIPTOS"
           DISPLAY "-------------------------------------------------".
           
       
           
      
      *> BUCLE QUE SE CIERRA CUANDO INGRESAS 1
      *> VERIFICAR EL CONFLICTO CON LAS MATERIAS Y AVISARLO
           PERFORM UNTIL 1 = 1

               PERFORM ALUMNO-ANOTAR-EN-MATERIA

               DISPLAY "INGRESE SI QUIERE"
               DISPLAY "TERMINAR EL PROGRAMA"
               DISPLAY "0= FALSE 1 = TRUE"
               ACCEPT TERMINAR-BUCLE
           END-PERFORM.



       ALUMNO-ANOTAR-EN-MATERIA.
           DISPLAY "INGRESE LA MATERIA= A, D ,I"
           ACCEPT MATERIA-ANOTADA.

           PERFORM ALUMNO-ANOTAR-ANALISIS.
      *>     PERFORM ALUMNO-ANOTAR-DERECHO
      *>     PERFORM ALUMNO-ANOTAR-IMPUESTOS.


      *>   ANOTAR A ANALISIS
       ALUMNO-ANOTAR-ANALISIS.
           IF MATERIA-ANOTADA EQUAL "A"
               IF CONFLIC-ANALISIS EQUAL 0
                   ADD 1 TO CONT-ANALISIS
               ELSE
                   DISPLAY " HAY UN CONFLICTO CON MATERIA"
                   DISPLAY " ANALISIS NO PODES ANOTARTE, NO HAY"
                   DISPLAY " CUPO"
               END-IF
           END-IF.
      

      *>   ANOTAR A DERECHO  
       ALUMNO-ANOTAR-DERECHO.
           IF MATERIA-ANOTADA EQUAL "D"
               IF CONFLIC-DERECHO EQUAL 0
                   ADD 1 TO CONT-DERECHO
               ELSE
                   DISPLAY " HAY UN CONFLICTO CON MATERIA"
                   DISPLAY " DERECHO NO PODES ANOTARTE, NO HAY"
                   DISPLAY " CUPO"
               END-IF
           END-IF.
       

      *>   ANOTAR A IMPUESTOS  
       ALUMNO-ANOTAR-IMPUESTOS.    
           IF MATERIA-ANOTADA EQUAL "I"
               IF CONFLIC-IMPUESTOS EQUAL 0
                   ADD 1 TO CONT-IMPUESTOS
               ELSE
                   DISPLAY " HAY UN CONFLICTO CON MATERIA"
                   DISPLAY " IMPUESTOS NO PODES ANOTARTE, NO HAY"
                   DISPLAY " CUPO"
               END-IF
           END-IF.

           STOP RUN.