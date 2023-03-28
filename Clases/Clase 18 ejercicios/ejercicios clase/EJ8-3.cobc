IDENTIFICATION DIVISION.
       PROGRAM-ID. EJ8-3.
       AUTHOR. VICTOR.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  CADENA.
           02 PIC X(10) VALUE "ENERO     ".
           02 PIC X(10) VALUE "FEBRERO   ".
           02 PIC X(10) VALUE "MARZO     ".
           02 PIC X(10) VALUE "ABRIL     ".
           02 PIC X(10) VALUE "MAYO      ".
           02 PIC X(10) VALUE "JUNIO     ".
           02 PIC X(10) VALUE "JULIO     ".
            02 PIC X(10) VALUE "AGOSTO    ".
           02 PIC X(10) VALUE "SEPTIEMBRE".
           02 PIC X(10) VALUE "OCTUBRE   ".
           02 PIC X(10) VALUE "NOVIEMBRE ".
           02 PIC X(10) VALUE "DICIEMBRE ".
       01  CADENA2 REDEFINES CADENA.
           02 CADE PIC X(10) OCCURS 12 TIMES.

       77  MES  PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       COMIENZO.
            DISPLAY "INGRESE UN NUMERO DE MES:".
            ACCEPT MES.
            IF MES NOT < 1 AND MES NOT > 12 THEN
                  DISPLAY CADE(MES)
               ELSE
                  DISPLAY "NUMERO DE MES INCORRECTO"
            END-IF.
            GOBACK.
