      * Division -> Section -> Variables/Paragraphs/Statements
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGIN.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
      * Variables go inside this section
       WORKING-STORAGE SECTION.
       01 USER-NAME PIC X(20).

      * This is where you do the actual logic stuff 
       PROCEDURE DIVISION.
           MOVE "Bob" TO USER-NAME.
           DISPLAY "Name: " USER-NAME.
           MOVE "Jack" TO USER-NAME.
           DISPLAY "Name: " USER-NAME.
       STOP RUN.
