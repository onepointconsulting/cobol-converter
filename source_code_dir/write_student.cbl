      ******************************************************************
      * Author: Gil Fernandes
      * Date: 2023-11-22
      * Purpose: Write a student to a file.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRITE-STUDENT-FILE.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT STUDENT ASSIGN TO 'output.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
               
       DATA DIVISION.
           FILE SECTION.
           FD STUDENT.
           01 STUDENT-FILE.
               05 STUDENT-ID PIC 9(5).
               05 NAME PIC A(25).
               05 CLASS-NAME PIC X(3).

           WORKING-STORAGE SECTION.
           01 WS-STUDENT.
               05 WS-STUDENT-ID PIC 9(5).
               05 WS-NAME PIC A(25).
               05 WS-CLASS-NAME PIC X(3).

       PROCEDURE DIVISION.
           OPEN OUTPUT STUDENT.
               
               MOVE 1000 TO STUDENT-ID.
               MOVE 'Tim' TO NAME.
               MOVE '10' TO CLASS-NAME.
               WRITE STUDENT-FILE
               
               MOVE 1001 TO STUDENT-ID.
               MOVE 'Gil Fernandes' TO NAME.
               MOVE '10' TO CLASS-NAME.
               WRITE STUDENT-FILE
               
               MOVE 1002 TO STUDENT-ID.
               MOVE 'Sasha Polev' TO NAME.
               MOVE '10' TO CLASS-NAME.
               WRITE STUDENT-FILE
               
               MOVE 1003 TO STUDENT-ID.
               MOVE 'Shashin Shah' TO NAME.
               MOVE '10' TO CLASS-NAME.
               WRITE STUDENT-FILE
               
               MOVE 1004 TO STUDENT-ID.
               MOVE 'Allan Schweitz' TO NAME.
               MOVE '10' TO CLASS-NAME.
               WRITE STUDENT-FILE
               
           END-WRITE.
           CLOSE STUDENT.
       STOP RUN.
       END PROGRAM WRITE-STUDENT-FILE.
