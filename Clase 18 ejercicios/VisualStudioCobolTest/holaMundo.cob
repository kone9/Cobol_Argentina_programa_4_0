       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL-18-EJ-17.

       DATA DIVISION.
           

       WORKING-STORAGE SECTION.
      *> declaro la tabla
       01 DIAS-SEMANA.
           05 DIA OCCURS 5 TIMES.
                10 NOMBRE-DIA PIC X(9).
      *> indice para recorrer bucle  DIAS    
       01 I PIC 9.

       *> creo el sector finanza   
       01 SECTOR-FINANZA.
           05 DIA-ANALISIS OCCURS 5 TIMES.
                10 ANALISIS-DIA PIC X(9).
           05 DIA-PROCEDIMIENTO OCCURS 5 TIMES.
                10 PROCEDIMIENTO-DIA PIC X(9).
       *> indice para recorrer bucle  SECTOR-FINANZA    
       01 J PIC 9.

       *> creo el sector finanza   
       01 SECTOR-CLIENTES.
           05 DIA-CLIENTES OCCURS 5 TIMES.
                10 ATENCION-DIA PIC X(9).
           05 DIA-PROCEDIMIENTO OCCURS 5 TIMES.
                10 ACTUALIZACION-DATOS-DIA PIC X(9).
       *> indice para recorrer bucle  SECTOR-CLIENTES    
       01 K PIC 9.

       PROCEDURE DIVISION.
      *>   Guardo los dias, nose hacerlo de otra manera por ahora
           MOVE "Lunes" TO NOMBRE-DIA(1).
           MOVE "Martes" TO NOMBRE-DIA(2).
           MOVE "Miércoles" TO NOMBRE-DIA(3).
           MOVE "Jueves" TO NOMBRE-DIA(4).
           MOVE "Viernes" TO NOMBRE-DIA(5).

       *>   Guardo los dias, SECTOR-FINANZA "DIA-ANALISIS"
           MOVE "Lunes" TO ANALISIS-DIA(1).
           MOVE "Miércoles" TO ANALISIS-DIA(2).
           MOVE "Viernes" TO ANALISIS-DIA(3).
       
       *>   Guardo los dias, SECTOR-FINANZA "PROCEDIMIENTO-DIA"
           MOVE "martes" TO PROCEDIMIENTO-DIA(1).
           MOVE "Jueves" TO PROCEDIMIENTO-DIA(2).

       *>   Guardo los dias, SECTOR-CLIENTES "DIA-ANALISIS"
           MOVE "Lunes" TO ATENCION-DIA(1).
           MOVE "Miércoles" TO ATENCION-DIA(2).
       
       *>   Guardo los dias, SECTOR-CLIENTES "PROCEDIMIENTO-DIA"
           MOVE "martes" TO ACTUALIZACION-DATOS-DIA(1).
           MOVE "Jueves" TO ACTUALIZACION-DATOS-DIA(2).
           MOVE "Viernes" TO ACTUALIZACION-DATOS-DIA(3).
       
       *>  recorro los dias
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
                DISPLAY "Día " I ": " NOMBRE-DIA(I)
                DISPLAY "FINANZA"
       *>  recorro RECORRO EL SECTOR-FINANZA ATENCION DIA        
                PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5
                     IF NOMBRE-DIA(I) ATENCION-DIA THEN
                          DISPLAY "  ATENCION-DIA"
                     END-IF
                END-PERFORM
       *>  recorro RECORRO EL SECTOR-FINANZA PROCEDIMIENTO-DIA  
                PERFORM VARYING K FROM 1 BY 1 UNTIL K > 5
                     IF NOMBRE-DIA(I) PROCEDIMIENTO-DIA THEN
                          DISPLAY "  PROCEDIMIENTO-DIA"
                     END-IF
                END-PERFORM
           
           END-PERFORM.


           STOP RUN.
