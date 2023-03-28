       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.



       DATA DIVISION.


       WORKING-STORAGE SECTION.
       01 num   PIC 9.




       PROCEDURE DIVISION.
      *>      DISPLAY "HOLA MUNDO"
           CALL "DATE-AND-TIME" USING FUNCTION RETURN-CODE,
                                BY REFERENCE FUNCTION DATE-AND-TIME-VALUE
           COMPUTE seed = FUNCTION MOD (FUNCTION INTEGER-OF-DATE (DATE-AND-TIME-VALUE),
                                 FUNCTION INTEGER-OF-TIME (DATE-AND-TIME-VALUE))
           CALL "SRAND" USING seed
           COMPUTE num = FUNCTION RANDOM (3)
           ADD 1 TO num
           DISPLAY "Número aleatorio: " num

           STOP RUN.
