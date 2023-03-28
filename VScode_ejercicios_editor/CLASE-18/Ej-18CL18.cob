
      *> CLASE 18 Ejercicio ejercicio resuelto CLASE 20
      *> Ariel Gimenez

      *>  Construir un diagrama de flujo para describir la solución y mostrar 
      *> resultado de la siguiente problemática:
      *> - Como INPUT se recibe un monto determinado en millones 
      *> de pesos sin decimales.
      *> Distribuir dicho monto entre las siguientes variables; según el 
      *>  porcentaje de participaciónde cada una de ellas:
      *>     ▪ PARTICIPANTE 1 = 15,5%
      *>     ▪ PARTICIPANTE 2 = 10,5%
      *>     ▪ PARTICIPANTE 3 = 50%
      *>     ▪ PARTICIPANTE 4 = 14%
      *>     ▪ PARTICIPANTE 5 = 10%
      *> Al finalizar mostrar el importe de participación en $ de cada 
      *> participante; teniendo en cuenta los decimales.
      *> Cuando el importe a mostrar tenga “0” no significativos;
      *> reemplazarlos por “espacios”.
      *> Colocar el punto indicando los miles y la coma para 
      *> indicar los decimales.


      *> NOTA: NO ENCONTRE LA FORMA DE PONER LOS PUNTOS Y LOS DECIMALES

      *> CLASE 11 Y 12 HAY INFORMACION DE COMO PONER PUNTOS Y COMAS

       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL-18-EJ-18.

      *>EJEMPLO 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
      *>   importante si uso decimal point, tengo que poner coma en los puntos
           DECIMAL-POINT IS COMMA.


       DATA DIVISION.


      *>-----------------------------WORKING-STORAGE----------------------------
       WORKING-STORAGE SECTION.
       01 MILLONES PIC 9(9)V9 VALUE 1000000.
       
      *> TABLA PORCENTAJES QUE OCURRE 5 VECES CON VALOR 99V9
      *>01  TABLA-PORCENTAJES.
      *>     02 PORCENTAJES PIC 99V9 OCCURS 5 TIMES.

      *> ACA ABAJO REDEFINO LA TABLA CON CADA VALOR
      *> 01  MONTOS-PORCENTAJES REDEFINES TABLA-PORCENTAJES.
      *>     02 A PIC 99V9 VALUE 15,5.
      *>     02 B PIC 99V9 VALUE 10,5.
      *>     02 C PIC 99V9 VALUE 50,0.
      *>     02 D PIC 99V9 VALUE 14,0.
      *>     02 E PIC 99V9 VALUE 10,0.
       
      *>   -------------------------------------------------------------
       
      *>   TABLA PORCENTAJES QUE OCURRE 5 VECES CON VALOR 99V9
      *>   Uso decimal point, tengo que poner coma en los puntos
       01  MONTOS-PORCENTAJES.
           02 A PIC 99V9 VALUE 15,5.
           02 B PIC 99V9 VALUE 10,5.
           02 C PIC 99V9 VALUE 50,0.
           02 D PIC 99V9 VALUE 14,0.
           02 E PIC 99V9 VALUE 10,0.
       
       01  TABLA-PORCENTAJES REDEFINES MONTOS-PORCENTAJES.
           02 PORCENTAJES PIC 99V9 OCCURS 5 TIMES.
      *>   -------------------------------------------------------------

      *> TABLA CON EL VALOR DE CADA UNO TOMANDO EN CUENTO EL MONTO MILLONES
      *> VARIABLE DE EDICION
       01  MONTOS-FINALES.
           02 FILLER PIC 9(9)V99.
           02 FILLER PIC 9(9)V99.
           02 FILLER PIC 9(9)V99.
           02 FILLER PIC 9(9)V99.
           02 FILLER PIC 9(9)V99.
       01  TABLA-MONTOS-FINALES REDEFINES MONTOS-FINALES.
           02 MONTOS PIC 9(9)V99 OCCURS 5 TIMES.
      *>   -------------------------------------------------------------

      *>  CREO UN INDICE PARA RECORRER LOS BUCLES 
       01 INDICE PIC 99.
      *>  VARIABLE TEMPORAL PARA HACER EL CALCULO
       01 RESUL PIC 9(9)V99.
      *>  PESO ADELANTE PARA QUE MUESTRE EL PESO CON REFORMATEO punto y coma
       01 EDICION PIC $ZZZ.ZZZ.ZZ9,99.
       
      *>---------------------------PROCEDURE----------------------------
       PROCEDURE DIVISION.
           DISPLAY MONTOS-PORCENTAJES.
           DISPLAY MONTOS-PORCENTAJES.

      *>   DETALLE ESTETICO INICIAL
           DISPLAY "---------------------------------------------------"
           DISPLAY "---------------------------------------------------"
           DISPLAY "          "
           
           DISPLAY "INGRESE EL MONTO"
           ACCEPT MILLONES
           DISPLAY "          "

      *>   RECORRE 5 VECES PARA REPRESENTAR LOS 5 PORCENTAJES
           PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 5
      
      *>   GUARDO EL MONTO SEGUN EL PORCENTAJE EN CADA RESULTADO 
                  COMPUTE RESUL = MILLONES * PORCENTAJES(INDICE) / 100 
                  MOVE RESUL TO MONTOS(INDICE)

                  MOVE MONTOS(INDICE) TO EDICION
      *>   MUESTRO RESULTADO, ME FALTO EL TEMA DE LOS PUNTOS 
      *>   Y LAS COMAS, ESPERO RESPUESTA DE ESO EN CLASE

                  DISPLAY  MONTOS(INDICE)
                  DISPLAY  PORCENTAJES(INDICE)
                  DISPLAY  EDICION
          
      *>   FIN BUCLE
           END-PERFORM

           
      *>   DETALLE ESTETICO FINAL
           DISPLAY "          "
           DISPLAY "---------------------------------------------------"

           DISPLAY "FIN DE PROGRAMA ARIEL GIMENEZ 20/03/2023"
           DISPLAY " "
           DISPLAY " "

       *>  TERMINA PROGRAMA
           STOP RUN.
       

