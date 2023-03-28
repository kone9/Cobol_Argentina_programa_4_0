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
       PROGRAM-ID. EJ-02ENCL22.
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
       01 VARIABLES.
           03 ANA1 PIC 999.
           03 DER2 PIC 999.
           03 IMP3 PIC 999.
           03 MATERIA PIC 9.


       01 CONFLICTO1 PIC X VALUE "N".
       01 CONFLICTO2 PIC X VALUE "N".
       01 CONFLICTO3 PIC X VALUE "N".

      *> 0 TERMINAR PROGRAMA 1 CONTINUAR 
       01 TERMINAR-PROGRAMA PIC 9 VALUE 1.


      *> VARIABLE PARA MOSTRAR RESULTADOS FORMATEADOS
       01 FORMATEO-RESULT PIC ZZ9.

      *>   nota este ejercicio uso parrafos en el procedura,
      *>   dejo alguna recomendacion
      *>   de los profesores
      *>   los nombres de los parrafos 
      *>   NO PUEDEN TENER MÁS DE 30 CARACTERES
      *>----------------------------------------------------------------
      *>--------------------------PROCEDURE-----------------------------
       PROCEDURE DIVISION.
      *>   inicia las variables correctamente
           INITIALIZE VARIABLES.

       COMIENZO.
      *>   todo lo que va en la procedura va a partir de la columna 12
           DISPLAY " ".
           DISPLAY "-------------------------------------------------".
           DISPLAY "INICIO EJERCICIOS CON CANTIDAD ALUMNOS INSCRIPTOS"
           DISPLAY "-------------------------------------------------".
           DISPLAY "AGREGAR ALUMNOS INGRESAR = 1"
           DISPLAY "TERMINAR EL PROGRAMA INGRESAR = 0"
           PERFORM VERIFICAR-TERMINAR-PROGRAMA.
           DISPLAY " "

           DISPLAY "INGRESE ALUMNO A ALGUNA MATERIAS".
           DISPLAY "ANALISIS = 1 // DERECHO = 2 // IMPUESTOS = 3 ".
           ACCEPT MATERIA.
           PERFORM VERIFICAR.
           PERFORM AGREGAR.

       VERIFICAR. 
      *>VERIFICAR-SI-NUMERO-MATERIA-CORRECTO
           IF MATERIA < 1 AND MATERIA > 3
             DISPLAY "NUMERO INCORRECTO, SOLO AGREGUE MATERIAS DE 1 A 3"
             DISPLAY " "
             GO TO COMIENZO.

       AGREGAR.
      *> AGREGAR-ALUMNOS-A-MATERIAS.
           PERFORM ALUMNO-ANOTAR-ANALISIS.
           PERFORM ALUMNO-ANOTAR-DERECHO.
           PERFORM ALUMNO-ANOTAR-ANALISIS.
      
      *> ACHICAR LOS NOMBRES PARRAFOS ahora los dejo asi ya que funciona

       ALUMNO-ANOTAR-ANALISIS.
           IF MATERIA = 1
               COMPUTE ANA1 = ANA1 + 1
               IF ANA1 > 5 MOVE "S" TO CONFLICTO1
                   COMPUTE ANA1 = ANA1 - 1
               END-IF
      *>   vuelve a comienzo si pasa esto
               GO TO COMIENZO
           END-IF.

       ALUMNO-ANOTAR-DERECHO.
           IF MATERIA = 2
               COMPUTE DER2 = DER2 + 1
               IF DER2 > 7 MOVE "S" TO CONFLICTO2
                   COMPUTE DER2 = DER2 - 1
               END-IF
      *>   vuelve a comienzo si pasa esto
               GO TO COMIENZO
           END-IF.

       ALUMNO-ANOTAR-IMPUESTOS.
           IF MATERIA = 3
               COMPUTE IMP3 = IMP3 + 1
               IF IMP3 > 8 MOVE "S" TO CONFLICTO3
                   COMPUTE IMP3 = IMP3 - 1
               END-IF
      *>   vuelve a comienzo si pasa esto
               GO TO COMIENZO
           END-IF.
       
       VERIFICAR-TERMINAR-PROGRAMA.
           ACCEPT TERMINAR-PROGRAMA
           IF TERMINAR-PROGRAMA = 0
               GO TO FIN-PROGRAMA.
           
       MOSTRAR-CANTIDAD-INSCRIPTOS.
      *>    formateo resultados para que no tengan ceros
           MOVE ANA1 TO FORMATEO-RESULT
           DISPLAY "CANTIDAD INSCRIPTOS ANALISIS = " FORMATEO-RESULT
           MOVE DER2 TO FORMATEO-RESULT
           DISPLAY "CANTIDAD INSCRIPTOS DERECHO = " FORMATEO-RESULT
           MOVE IMP3 TO FORMATEO-RESULT
           DISPLAY "CANTIDAD INSCRIPTOS IMPUESTOS = " FORMATEO-RESULT.
       
       MOSTRAR-CONFLICTOS.
           IF CONFLICTO1 = "S"
               DISPLAY "SE ENCONTRO UN CONFLICTO EN ANALISIS"
           END-IF
           IF CONFLICTO2 = "S"
               DISPLAY "SE ENCONTRO UN CONFLICTO EN DERECHO"
           END-IF
           IF CONFLICTO3 = "S"
               DISPLAY "SE ENCONTRO UN CONFLICTO EN IMPUESTOS"
           END-IF.
       
       FIN-PROGRAMA.
           DISPLAY " "
           DISPLAY "---------------------------------------------------"
           DISPLAY " "
           PERFORM MOSTRAR-CANTIDAD-INSCRIPTOS
           PERFORM MOSTRAR-CONFLICTOS
           DISPLAY "TERMINO EL PROGRAMA".

           STOP RUN.