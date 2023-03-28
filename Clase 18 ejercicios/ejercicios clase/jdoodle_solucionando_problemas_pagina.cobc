

IDENTIFICATION DIVISION.
AUTHOR. ARIEL. GIMENEZ.
SECURITY. PRECIO-LIBRO.
PROGRAM-ID. CL16-EJERCICIO-CLASE-8-4. 


DATA DIVISION.
    WORKING-STORAGE SECTION.
        
        01 ValorbasicoLibro PIC 999 VALUE 500.             
        01 ValorPorPagina PIC 99V99 VALUE 20.20.           
        01 ValorTapaTela PIC 999 VALUE 200.                  
        01 ValorProcedimientoEspecial PIC 999 VALUE 336.     
        01 valorLibro PIC 99999.                            
        01 cantidadDePáginas PIC 99999 VALUE 700.                     
        
   
PROCEDURE DIVISION.
        DISPLAY "Ingrese cantidad de páginas del libro para calcular su valor"
        ACCEPT cantidadDePáginas
        
        IF cantidadDePáginas < 1 THEN
            DISPLAY "se va a cerrar el programa tienes que ingresar más de una página"
        ELSE
            IF cantidadDePáginas >= 300 THEN    
                COMPUTE valorLibro = ValorbasicoLibro +  ValorTapaTela
                
                IF cantidadDePáginas >= 600 THEN    
                    COMPUTE valorLibro = ValorbasicoLibro +  ValorTapaTela
                    DISPLAY "El precio básico del libro incluyendo tapa y procedimiento especial porque tiene"
                ELSE
                    DISPLAY "El precio básico del libro incluyendo tapa porque tiene "
                    DISPLAY ValorbasicoLibro
           
            ELSE
                DISPLAY "El precio básico del libro porque tiene tapa y procedimiento especial"
                DISPLAY ValorbasicoLibro
            
       
        END-IF.

    
STOP RUN.













