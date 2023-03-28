      
      *> el comentario siempre empieza de la linea 7
      *> en mainframe no se puede hacer accert por consola
      *> tener en cuenta eso ya que eso.
      *> el entorno profesional es "Cobol, CICS y DB2"


      *>--------------------------------------
      *> IdentificarDatos
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJ-01ENCL20.
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
       DATA DIVISION.
      *>     FILES SECTION.
      *>     INPUT OUTPUT SECTION.
      *>--------------------------------------
       

      *>--------------------------------------
       WORKING-STORAGE SECTION.
       01 TABLA-NOTAS.
           02 NOTAS PIC 9(2)V99 OCCURS 15 TIMES.
       
      *> TODAS LAS NOTAS
       77 IND PIC 9(2)  VALUE ZERO. 
       77 TOT-TM  PIC 9(2)V99  VALUE ZERO. 
       77 TOT-TT  PIC 9(2)V99  VALUE ZERO. 
       77 TOT-TN  PIC 9(2)V99  VALUE ZERO. 
       77 PROM-TM PIC 9(2)V99  VALUE ZERO.
       77 PROM-TT PIC 9(2)V99  VALUE ZERO.
       77 PROM-TN PIC 9(2)V99  VALUE ZERO.

       77 PROM-GRAL PIC 9(2)V99  VALUE ZERO.

      *> indice para el bucle
       77 INDICE PIC 9(2)  VALUE ZERO.
      *>--------------------------------------

       PROCEDURE DIVISION.
      *>   todo lo que va en la procedura va a partir de la columna 12
           DISPLAY "INGRESE NOTA".
           DISPLAY "-------------------------------------------------".
           DISPLAY "-------------------------------------------------".

      *>   MAÑANA    
           DISPLAY "NOTAS TURNO MAÑANA".
           PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 5
                 DISPLAY "INGRESE NOTA"
                 ACCEPT NOTAS(INDICE)
                 COMPUTE TOT-TM = TOT-TM + NOTAS(INDICE)
           END-PERFORM.
           COMPUTE PROM-TM = TOT-TM / 5
           DISPLAY "EL PROMEDIO DE TURNO MAÑANA ES: " PROM-TM.

           DISPLAY "-------------------------------------------------".
           DISPLAY " ".

      *>   TARDE    
           DISPLAY "NOTAS TURNO TARDE".
           PERFORM VARYING INDICE FROM 6 BY 1 UNTIL INDICE > 10
                 DISPLAY "INGRESE NOTA"
                 ACCEPT NOTAS(INDICE)
                 COMPUTE TOT-TT = TOT-TT + NOTAS(INDICE)
           END-PERFORM.
           COMPUTE PROM-TT = TOT-TT / 5
           DISPLAY "EL PROMEDIO DE TURNO TARDE ES: " PROM-TT.
           
           DISPLAY "-------------------------------------------------".
           DISPLAY " ".

      *>   NOCHE    
           DISPLAY "NOTAS TURNO NOCHE".
           PERFORM VARYING INDICE FROM 11 BY 1 UNTIL INDICE > 15
                 DISPLAY "INGRESE NOTA"
                 ACCEPT NOTAS(INDICE)
                 COMPUTE TOT-TN = TOT-TN + NOTAS(INDICE)
           END-PERFORM.
           COMPUTE PROM-TT = TOT-TT / 5
           DISPLAY "EL PROMEDIO DE TURNO NOCHE ES: " PROM-TN.
           
           DISPLAY "-------------------------------------------------".
           DISPLAY " ".

      *>   GENERAL   
           COMPUTE PROM-GRAL = (PROM-TM + PROM-TT + PROM-TN) / 15
           DISPLAY "EL PROMEDIO GRAL ES:" PROM-GRAL. 

           DISPLAY "-------------------------------------------------".
           DISPLAY " ".

           STOP RUN.