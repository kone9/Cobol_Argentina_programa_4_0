       
      *> CLASE 18 Ejercicio ejercicio CLASE 17
      *> Ariel Gimenez

      *>   Se sabe que hay 7 días de la semana; de los cuales habitualmente 
      *>   resultan laborables de lunes a viernes durante 8 horas cada día.
      *>   Tomando como precondición esta premisa relatada como HABITUAL;
      *>   construir un diagrama de flujo que muestre el detalle de actividades
      *>   realizadas cada día laborable porcada sector de una empresa
      *>   financiera Los sectores son:
      *>  01 finanzas; 02 clientes; 03 inversiones; 04 préstamos; 05 informática
      
      *>   Las actividades generales se dividen según código de sector
      *>   de 01 a 05, a saber: 
      *>    01) Análisis (LUN; MIE; VIE); procedimientos (MAR, JUE)
      *>    02) Atención (LUN; MIE); actualización datos (MAR; JUE; VIE)
      *>    03) Administración (MAR; JUE); asesoramiento (LUN; MIE; VIE)
      *>    04) Promoción (VIE); otorgamiento (LUN; MAR; MIE; JUE)
      *>    05) Desarrollo (LUN; MAR; MIE; JUE; VIE) Producción 
      *>   (LUN; MAR; MIE; JUE; VIE)
      *>    Se deberá generar un listado con el detalle de todas las actividades
      *>    de cada sector por cada día de la semana.
      *>    Al finalizar, MOSTRAR EL TOTAL GENERAL de actividades por 
      *>   sector de la semana completa.

       
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL-18-EJ-17.

       DATA DIVISION.
      *> ESTE PROYECTO ES EL MEJOR EJEMPLO DE CODIGO SPAGUETTI XD
      
      *> POR EJEMPLO PARA CREAR UN ARRAY SE USARON LOS CONCEPTOS
      *> VARIBLES COMPUESTAS
      *> NUMEROS DE NIVEL
      *> ESTRUCTURAS ANIDADAS
      *> VARIABLES CONSTANTES FILLER
      *> REDEFINES
      *> OCCURS
      *> DISPLAY para probar su funcionamiento    
       

      *>-----------------------------WORKING-STORAGE----------------------------
       WORKING-STORAGE SECTION.

      *>   ----CREACION DE UNA TABLA EN COBOL CON 5 DIAS DE LA SEMANA---  
      *>   USO FILLER PARA CREAR LOS VALORES CONSTANTES DE LOS DIAS LABORALES
       01 DIAS.
           02 FILLER PIC X(10) VALUE "LUNES".
           02 FILLER PIC X(10) VALUE "MARTES".
           02 FILLER PIC X(10) VALUE "MIERCOLES".
           02 FILLER PIC X(10) VALUE "JUEVES".
           02 FILLER PIC X(10) VALUE "VIERNES".
      *>   CON REDEFINE HACEMOS QUE LO VALRES SE CARGUEN EN TABLA DIAS
      *>   CON OCCURS DEFINIMOS UNA TABLA CON UNA CIERTA CANTIDAD DE VALORES
       01  TABLA-DIAS-LABORALES REDEFINES DIAS.
           02 D-LABORALES  PIC X(10) OCCURS 5 TIMES.
      
      *>   CREO UN INDICE PARA RECORRER EL BUCLE
       01  INDICE-DIAS PIC 99.
      *>   -------------------------------------------------------------
      

      *>   --------------------FINANZAS------------WORKING--------------
       01  ANALISIS.
           02 FILLER PIC X(10) VALUE "LUNES".
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE "MIERCOLES".
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE "VIERNES".
       01  TABLA-FINANZAS-ANALISIS REDEFINES ANALISIS.
           02 D-ANALISIS PIC X(10) OCCURS 5 TIMES.

       01  PROCEDIMIENTO.
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE "MARTES".
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE "JUEVES".
           02 FILLER PIC X(10) VALUE " ".
       01 TABLA-FINANZAS-PROCEDIMIENTO REDEFINES PROCEDIMIENTO.
           02 D-PROCEDIMIENTO PIC X(10) OCCURS 5 TIMES.
      *>   -------------------------------------------------------------


      *>   --------------------CLIENTES------------------WORKING--------
       01  ATENCION.
           02 FILLER PIC X(10) VALUE "LUNES".
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE "MIERCOLES".
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE " ".
       01  TABLA-CLIENTES-ATENCION REDEFINES ATENCION.
           02 D-ATENCION PIC X(10) OCCURS 5 TIMES.

       01  ACT-DATOS.
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE "MARTES".
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE "JUEVES".
           02 FILLER PIC X(10) VALUE "VIERNES".
       01 TABLA-CLIENTES-ACT-DATOS REDEFINES ACT-DATOS.
           02 D-ACT-DATOS PIC X(10) OCCURS 5 TIMES.
      *>   -------------------------------------------------------------

       
      *>   --------------------INVERSIONES--------------WORKING---------
       01  ADMINISTRACION.
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE "MARTES".
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE "JUEVES".
           02 FILLER PIC X(10) VALUE " ".
       01  TABLA-INVERC-ADMINISTRACION REDEFINES ADMINISTRACION.
           02 D-ADMINISTRACION PIC X(10) OCCURS 5 TIMES.

       01  ASESORAMIENTO.
           02 FILLER PIC X(10) VALUE "LUNES".
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE "MIERCOLES".
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE "VIERNES".
       01 TABLA-INVERC-ASESORAMIENTO REDEFINES ASESORAMIENTO.
           02 D-ASESORAMIENTO PIC X(10) OCCURS 5 TIMES.
      *>   -------------------------------------------------------------

       
      *>   --------------------PRESTAMO------------------WORKING--------
       01  PROMOSION.
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE " ".
           02 FILLER PIC X(10) VALUE "VIERNES".
       01  TABLA-PREST-PROMOSION REDEFINES PROMOSION.
           02 D-PROMOSION PIC X(10) OCCURS 5 TIMES.

       01  OTORGAMIENTO.
           02 FILLER PIC X(10) VALUE "LUNES".
           02 FILLER PIC X(10) VALUE "MARTES".
           02 FILLER PIC X(10) VALUE "MIERCOLES".
           02 FILLER PIC X(10) VALUE "JUEVES".
           02 FILLER PIC X(10) VALUE " ".
       01  TABLA-PREST-OTORGAMIENTO REDEFINES OTORGAMIENTO.
           02 D-OTORGAMIENTO PIC X(10) OCCURS 5 TIMES.
      *>   -------------------------------------------------------------


      *>   --------------------INFORMATICA---------------------WORKING--
       01  DESARROLLO.
           02 FILLER PIC X(10) VALUE "LUNES".
           02 FILLER PIC X(10) VALUE "MARTES".
           02 FILLER PIC X(10) VALUE "MIERCOLES".
           02 FILLER PIC X(10) VALUE "JUEVES".
           02 FILLER PIC X(10) VALUE "VIERNES".
       01  TABLA-INFORMT-DESARROLLO REDEFINES DESARROLLO.
           02 D-DESARROLLO PIC X(10) OCCURS 5 TIMES.

       01  PRODUCCION.
           02 FILLER PIC X(10) VALUE "LUNES".
           02 FILLER PIC X(10) VALUE "MARTES".
           02 FILLER PIC X(10) VALUE "MIERCOLES".
           02 FILLER PIC X(10) VALUE "JUEVES".
           02 FILLER PIC X(10) VALUE "VIERNES".
       01  TABLA-INFORMT-PRODUCCION REDEFINES PRODUCCION.
           02 D-PRODUCCION PIC X(10) OCCURS 5 TIMES.
      *>   -------------------------------------------------------------


      *>  CREO UN INDICE PARA RECORRER LOS BUCLES SECUNDARIOS
       01  i PIC 99.
       
      *>  USO NUMERO COMO BOOLEANO PARA SABER SI MUESTRO EL SECTOR
      *>  0 FALSO 1 VERDADERO
       01  BOOL_VIEW_SECTOR PIC 9 VALUE 0.


      *>---------------------------PROCEDURE----------------------------
       PROCEDURE DIVISION.


      *>   PARA QUE SE VEA BIEN AL INICIO
           DISPLAY "          "
           DISPLAY "---------------------------------------------------"
           DISPLAY "          "

      *> POR CADA DIA VOY A REVISAR CADA SECTOR, BUCLE PRINCIPAL  
           PERFORM VARYING INDICE-DIAS FROM 1 BY 1 UNTIL INDICE-DIAS > 5
      
      *> MUESTRO DIA         
               DISPLAY D-LABORALES(INDICE-DIAS)
      
      *>--------------------------FINANZAS--------------------PROCEDURE-     
      
      *> SINO HAY SECTOR FINANZA EN ESE DÍA NO LO MUESTRO            
               PERFORM VARYING i FROM 1 BY 1 UNTIL i > 5
                IF D-ANALISIS(i) EQUAL D-LABORALES(INDICE-DIAS)   
                    COMPUTE BOOL_VIEW_SECTOR = 1
                END-IF
                IF D-PROCEDIMIENTO(i) EQUAL D-LABORALES(INDICE-DIAS)   
                    COMPUTE BOOL_VIEW_SECTOR = 1
                END-IF
               END-PERFORM

               IF BOOL_VIEW_SECTOR EQUAL 1
                    DISPLAY "     FINANZAS"
               END-IF
      
      *> FINANZAS ANALISIS
      *> FINANZAS PROCEDIMIENTO      

               PERFORM VARYING i FROM 1 BY 1 UNTIL i > 5
                IF D-ANALISIS(i) EQUAL D-LABORALES(INDICE-DIAS)   
                    DISPLAY "         ANALISIS"
                END-IF
                IF D-PROCEDIMIENTO(i) EQUAL D-LABORALES(INDICE-DIAS)   
                    DISPLAY "         PROCEDIMIENTO"
                END-IF
                END-PERFORM

                COMPUTE BOOL_VIEW_SECTOR = 0
      *>----------------------------------------------------------------
          
      *>--------------------------CLIENTES--------------------PROCEDURE-

      *> SINO HAY SECTOR CLIENTES EN ESE DÍA NO LO MUESTRO           
               PERFORM VARYING i FROM 1 BY 1 UNTIL i > 5
                IF D-ATENCION(i) EQUAL D-LABORALES(INDICE-DIAS)   
                    COMPUTE BOOL_VIEW_SECTOR = 1
                END-IF
                IF D-ACT-DATOS(i) EQUAL D-LABORALES(INDICE-DIAS)   
                    COMPUTE BOOL_VIEW_SECTOR = 1
                END-IF
               END-PERFORM

               IF BOOL_VIEW_SECTOR EQUAL 1
                     DISPLAY "     CLIENTES"
               END-IF
       
                
      *> CLIENTES ATENCION
      *> CLIENTES ACTUALIZACION DATOS      

                PERFORM VARYING i FROM 1 BY 1 UNTIL i > 5
                    IF D-ATENCION(i) EQUAL D-LABORALES(INDICE-DIAS)   
                        DISPLAY "         ATENCION"
                    END-IF
                    IF D-ACT-DATOS(i) EQUAL D-LABORALES(INDICE-DIAS)   
                        DISPLAY "         ACTUALIZACION DATOS"
                    END-IF
                END-PERFORM

                COMPUTE BOOL_VIEW_SECTOR = 0
      *>----------------------------------------------------------------

      *>--------------------------INVERSION-------------------PROCEDURE-

      *> SINO HAY SECTOR INVERSION EN ESE DÍA NO LO MUESTRO             
               PERFORM VARYING i FROM 1 BY 1 UNTIL i > 5
                IF D-ADMINISTRACION(i) EQUAL D-LABORALES(INDICE-DIAS)   
                    COMPUTE BOOL_VIEW_SECTOR = 1
                END-IF
                IF D-ASESORAMIENTO(i) EQUAL D-LABORALES(INDICE-DIAS)   
                    COMPUTE BOOL_VIEW_SECTOR = 1
                END-IF
               END-PERFORM

               IF BOOL_VIEW_SECTOR EQUAL 1
                     DISPLAY "     INVERSION" 
                END-IF
                 

                
      *> INVERSION ADMINISTRACION
      *> INVERSION ASESORAMIENTO 

                PERFORM VARYING i FROM 1 BY 1 UNTIL i > 5
                   IF D-ADMINISTRACION(i) EQUAL D-LABORALES(INDICE-DIAS)   
                        DISPLAY "         ADMINISTRACION"
                    END-IF
                    IF D-ASESORAMIENTO(i) EQUAL D-LABORALES(INDICE-DIAS)   
                        DISPLAY "         ASESORAMIENTO"
                    END-IF
                END-PERFORM

               COMPUTE BOOL_VIEW_SECTOR = 0
      *>----------------------------------------------------------------
      
       
      *>--------------------------PRESTAMO--------------------PROCEDURE-

      *> SINO HAY SECTOR PRESTAMO EN ESE DÍA NO LO MUESTRO          
               PERFORM VARYING i FROM 1 BY 1 UNTIL i > 5
                IF D-PROMOSION(i) EQUAL D-LABORALES(INDICE-DIAS)   
                    COMPUTE BOOL_VIEW_SECTOR = 1
                END-IF
                IF D-OTORGAMIENTO(i) EQUAL D-LABORALES(INDICE-DIAS)   
                    COMPUTE BOOL_VIEW_SECTOR = 1
                END-IF
               END-PERFORM

               IF BOOL_VIEW_SECTOR EQUAL 1
                     DISPLAY "     PRESTAMO"      
               END-IF
                 

                
      *> PRESTAMO PROMOSION
      *> PRESTAMO OTORGAMIENTO  

                PERFORM VARYING i FROM 1 BY 1 UNTIL i > 5
                   IF D-PROMOSION(i) EQUAL D-LABORALES(INDICE-DIAS)   
                        DISPLAY "         PROMOSION"
                   END-IF
                   IF D-OTORGAMIENTO(i) EQUAL D-LABORALES(INDICE-DIAS)   
                        DISPLAY "         OTORGAMIENTO"
                   END-IF
                END-PERFORM
               
                COMPUTE BOOL_VIEW_SECTOR = 0
      *>----------------------------------------------------------------


      *>--------------------------INFORMATICA-----------------PROCEDURE-
       
      *> SINO HAY SECTOR INFORMATICA EN ESE DÍA NO LO MUESTRO           
               PERFORM VARYING i FROM 1 BY 1 UNTIL i > 5
                IF D-DESARROLLO(i) EQUAL D-LABORALES(INDICE-DIAS)   
                    COMPUTE BOOL_VIEW_SECTOR = 1
                END-IF
                IF D-PRODUCCION(i) EQUAL D-LABORALES(INDICE-DIAS)   
                    COMPUTE BOOL_VIEW_SECTOR = 1
                END-IF
               END-PERFORM

               IF BOOL_VIEW_SECTOR EQUAL 1
                     DISPLAY "     INFORMATICA"        
               END-IF
                     
       
                
      *> INFORMATICA DESARROLLO
      *> INFORMATICA PRODUCCION

                PERFORM VARYING i FROM 1 BY 1 UNTIL i > 5
                   IF D-DESARROLLO(i) EQUAL D-LABORALES(INDICE-DIAS)   
                        DISPLAY "         DESARROLLO"
                   END-IF
                   IF D-PRODUCCION(i) EQUAL D-LABORALES(INDICE-DIAS)   
                        DISPLAY "         PRODUCCION"
                   END-IF
                END-PERFORM

               COMPUTE BOOL_VIEW_SECTOR = 0
      *>----------------------------------------------------------------



      *> CADA VES QUE TERMINA UN DIA AGREGO UN ESPACIO
           DISPLAY "          "
           DISPLAY "---------------------------------------------------"
           DISPLAY "          "


      *>   TERMINA POR CADA DIA BUCLE PRINCIPAL 
           END-PERFORM.

           DISPLAY "FIN DE PROGRAMA ARIEL GIMENEZ 16/03/2023"
           DISPLAY " "
           DISPLAY " "

       *>  TERMINA PROGRAMA
           STOP RUN.
       

