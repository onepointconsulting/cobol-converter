      ******************************************************************
      * Author: Gil Fernandes
      * Date:  2023-11-13
      * Purpose: See if Cobol works
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 MY_INPUT PIC X(255) VALUE 'HI '.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world, I love you!"
            DISPLAY "This is my first cobol programme"
            DISPLAY "Cobol is really cool"
            DISPLAY "Now say something ..."
            ACCEPT MY_INPUT
            DISPLAY "You have said: " MY_INPUT
            STOP RUN.
       END PROGRAM HELLO.
