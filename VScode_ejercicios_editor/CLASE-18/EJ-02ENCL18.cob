             
      *> el comentario siempre empieza de la linea 7
      *>--------------------------------------
      *> IdentificarDatos
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJ-02ENCL18.
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
      *> DECLARANDO DIAS DE FORMA NORMAL

       01 LUNES PIC 9(4).
       01 MARTES PIC 9(4). 
       01 MIERCOLES PIC 9(4).
       01 JUEVES PIC 9(4).
       01 VIERNES PIC 9(4).
       01 SABADO PIC 9(4).
       01 DOMINGO PIC 9(4).

      *> DECLARANDO ARRAY SIN DEFINIR
      *> ES UNA ESTRUCTURA QUE DENTRO TIENE EL OCCURS
       01 DIASSEMANA.
           02 VISITAS PIC 9(4) OCCURS 7 TIMES. 


      *>--------------------------------------

       PROCEDURE DIVISION.

           MOVE 200 TO LUNES.
           MOVE 430 TO MARTES.
           MOVE 136 TO MIERCOLES.
           MOVE 525 TO JUEVES.
           MOVE 380 TO VIERNES.
           MOVE 1910 TO SABADO.
           MOVE 2300 TO DOMINGO.

           MOVE 200 TO VISITAS(1).
           MOVE 430 TO VISITAS(2).
           MOVE 136 TO VISITAS(3).
           MOVE 525 TO VISITAS(4).
           MOVE 380 TO VISITAS(5).
           MOVE 1910 TO VISITAS(6).
           MOVE 2300 TO VISITAS(7).

           DISPLAY DIASSEMANA.
           DISPLAY VISITAS(1).

           STOP RUN.