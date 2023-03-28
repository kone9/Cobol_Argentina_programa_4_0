      *> el comentario siempre empieza de la linea 7

      *>--------------------------------------
      *> IdentificarDatos
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJ-03ENCL18.
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
       

      *>-----------------------WORKING-STORAGE-------------------------
       WORKING-STORAGE SECTION.
      *> DECLARANDO DIAS DE FORMA NORMAL
       
      *>----------------------------------------------------------------
      *> DECLARANDO ARRAY SIN DEFINIR
      *> ES UNA ESTRUCTURA QUE DENTRO TIENE EL OCCURS
       *> COMO USAR REDEFINES DECLARAR DATOS DENTRO DE ARRAY
       01  DIAS-SEMANA.
           02 FILLER PIC X(10) VALUE "LUNES ".
           02 FILLER PIC X(10) VALUE "MARTES ".
           02 FILLER PIC X(10) VALUE "MIERCOLES ".
           02 FILLER PIC X(10) VALUE "JUEVES ".
           02 FILLER PIC X(10) VALUE "VIERNES ".
           02 FILLER PIC X(10) VALUE "SABADO ".
           02 FILLER PIC X(10) VALUE "DOMINGO ".
      *>     02 ELDIA REDEFINES DIASSEMANA PIC X(10) OCCURS 7 TIMES. 
       01 TABLA-DE-DIAS REDEFINES DIAS-SEMANA.
           02 LOS-DIAS PIC X(10) OCCURS 7 TIMES.

      *>----------------------------------------------------------------
       01  CANT-VISITANTES.
           02 FILLER PIC 9999 VALUE 200.
           02 FILLER PIC 9999 VALUE 430.
           02 FILLER PIC 9999 VALUE 136.
           02 FILLER PIC 9999 VALUE 525.
           02 FILLER PIC 9999 VALUE 380.
           02 FILLER PIC 9999 VALUE 1910.
           02 FILLER PIC 9999 VALUE 2300.
       01  TABLA-CANT-VISITANTES REDEFINES CANT-VISITANTES.
           02 VISITAS PIC 9999 OCCURS 5 TIMES.
      *>--------------------------------------
       
       *> INDICE TABLA
       77 DIA PIC 9(2).

      *>-------------------PROCEDURE-------------------
       PROCEDURE DIVISION.

      *>   RECORRER EL BUCLE
           PERFORM VARYING DIA FROM 1 BY 1 UNTIL DIA > 7


           END-PERFORM
      
           STOP RUN.