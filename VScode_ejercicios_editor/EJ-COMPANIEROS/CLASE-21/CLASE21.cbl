      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.


       01 PRODUCTO PIC 9999.
       01 COCIENTE PIC 9999V999.
       01 COCIENTE-ENTERO PIC 9999.
       01 DIFERENCIA PIC S99.
       01 EDITABLE PIC ZZZ9,99CR.
       01 IMPARES.
              02 FILLER PIC 99 VALUE 01.
              02 FILLER PIC 99 VALUE 03.
              02 FILLER PIC 99 VALUE 05.
              02 FILLER PIC 99 VALUE 07.
              02 FILLER PIC 99 VALUE 09.
              02 FILLER PIC 99 VALUE 11.
              02 FILLER PIC 99 VALUE 13.
              02 FILLER PIC 99 VALUE 15.
              02 FILLER PIC 99 VALUE 17.
              02 FILLER PIC 99 VALUE 19.
       01 TABLA-IMPARES REDEFINES IMPARES.
              02 IMPAR PIC 99 OCCURS 10 TIMES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

       PRIMER-PAR.
                    ADD IMPAR(1) TO IMPAR(2).
       SEGUNDO-PAR.
                    MULTIPLY IMPAR(3) BY IMPAR(4) GIVING PRODUCTO.
       TERCER-PAR.
                    DIVIDE IMPAR(6) BY IMPAR(5) GIVING COCIENTE.
       CUARTO-PAR.
                    DIVIDE IMPAR(7) BY IMPAR(8) GIVING COCIENTE-ENTERO.
       QUINTO-PAR.
                    SUBTRACT IMPAR(10) FROM IMPAR(9) GIVING DIFERENCIA.

      *RESULTADOS
                    MOVE IMPAR(2) TO EDITABLE.
                    DISPLAY "SUMA " EDITABLE.
                    MOVE PRODUCTO TO EDITABLE.
                    DISPLAY "PRODUCTO " EDITABLE.
                    MOVE COCIENTE TO EDITABLE.
                    DISPLAY "COCIENTE " EDITABLE.
                    MOVE COCIENTE-ENTERO TO EDITABLE.
                    DISPLAY "COCIENTE ENTERO " EDITABLE.
                    MOVE DIFERENCIA TO EDITABLE.
                    DISPLAY "DIFERENCIA " EDITABLE.
                    ACCEPT PRODUCTO.

       STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
