      ******************************************************************
      * Author: Gil Fernandes
      * Date: 2023-11-22
      * Purpose: Write student data to file conditionally depending on command line input.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRITE-STUDENT-FILE.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT STUDENT ASSIGN TO 'students.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.
               
       DATA DIVISION.
           FILE SECTION.
           FD STUDENT.
           01 STUDENT-FILE.
               05 STUDENT-ID PIC 9(5).
               05 NAME PIC A(25).
               05 CLASS-NAME PIC X(3).

           WORKING-STORAGE SECTION.
               01 CMDLINE pic x(100).

       PROCEDURE DIVISION.
           ACCEPT CMDLINE FROM COMMAND-LINE
   
           OPEN OUTPUT STUDENT.
               
               IF CMDLINE NOT = SPACE AND LOW-VALUE THEN
                   MOVE 1006 TO STUDENT-ID
                   MOVE CMDLINE TO NAME
                   MOVE '11' TO CLASS-NAME
                   WRITE STUDENT-FILE
               ELSE
                   MOVE 1000 TO STUDENT-ID
                   MOVE 'Tim' TO NAME
                   MOVE '10' TO CLASS-NAME
                   WRITE STUDENT-FILE
                   
                   MOVE 1001 TO STUDENT-ID
                   MOVE 'John Doe' TO NAME
                   MOVE '10' TO CLASS-NAME
                   WRITE STUDENT-FILE   
                   
                   MOVE 1002 TO STUDENT-ID
                   MOVE 'Jane Doe' TO NAME
                   MOVE '11' TO CLASS-NAME
                   WRITE STUDENT-FILE   
                                                             
           END-WRITE.
           CLOSE STUDENT.
       STOP RUN.
       END PROGRAM WRITE-STUDENT-FILE.
