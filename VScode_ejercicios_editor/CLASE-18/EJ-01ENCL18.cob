      
      *> el comentario siempre empieza de la linea 7
      *>--------------------------------------
      *> IdentificarDatos
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJ-01ENCL18.
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
      *> AHORA CREAMOS UNA ESTRUCTURA, ANIDADA
      *> EL CONCEPTO SE LLAMA ESTRUCTURA O VARIBLES COMPUESTAS
      *>   se denomina variable dependiente
      *>   considera a nivel 01 con la longitud de todo lo que tiene adentro
      *>   conclusion es una estructura que contiene en ella a los niveles 03 
      *>   de 10 caracteres
       01 WS-VARIABLE PIC X(10) VALUE "2023/02/20".
      *>   03 WS-ANIO PIC X(4).
       01  WS-FECHA.
           03 WS-ANIO.
               05 WS-SIGLO PIC X(2).
               05 WS-DECADA PIC X(2).
      *>   Filler para separar
           03 FILLER PIC X(4) VALUE ".".
           03 WS-AND PIC X(2).
      *>   Filler para separar
           03 FILLER PIC X(4) VALUE ".".
           03 WS-AND PIC X(2).
       *> CUANDO APARECE UN NUMERO DE MAYOR NIVEL ASUME QUE TOMA UN NUEVO
       *> VALOR DE DATOS

       01 WS-FECHA-COBRO PIC X(10).


      *> CUANTAS PERSONAS ENTRAN A UN MUSEO



      *>--------------------------------------

       PROCEDURE DIVISION.
      *>   todo lo que va en la procedura va a partir de la columna 12
           DISPLAY "usar columna 12".
      *>   ACCEPT WS-FECHA-COBRO FROM SYSTEM.
           ACCEPT WS-FECHA-COBRO.
      *>     MOVE WS-FECHA-COBRO TO WS-FECHA.
       
      *>   NOSE COMO SE LLAMA EL CONCEPTO, PERO ASI SE FORMATEO SEGUN
      *>   LA PARTE DEL TEXTO
           MOVE WS-FECHA-COBRO(1:4) TO WS-ANIO.  
           MOVE WS-FECHA-COBRO(6:2) TO WS-ANIO.   
           MOVE WS-FECHA-COBRO(9:2) TO WS-ANIO.     

           DISPLAY WS-FECHA.

           STOP RUN.