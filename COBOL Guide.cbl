      * Hierarchy order:
      * Division -> Section -> Variables/Paragraphs/Statements
      * The four divisions below are the only ones.

      * What is the program called?
       IDENTIFICATION DIVISION.
      * PROGRAM-ID is required to run a program. It needs a name.
      * Note: There is a period after PROGRAM-ID *and* the program name.
      * Note: The program name must be one word. Use dashes to break it up.
       PROGRAM-ID. COBOL-Guide.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
      * Variables go inside this section
       WORKING-STORAGE SECTION.
      * Defining variables:
      * "01" - Level 1 variable. Higher numbers are like having children in a class, where the smaller number is the parent.
      * "USER-NAME" - The variable name. Whatever you want.
      * "PIC" - Stands for picture. It's just required. Include it.
      * "X(20)" - X means characters in a string. 20 is the max # of characters allowed.
      * "99" - Used for integers. Holds any number up to 99.
      * Variables only use the max amount when defining, like for a range of an array.
       01 USER-NAME PIC X(20).
       02 AGE PIC 99.
      * This is where you do the actual logic stuff 
       PROCEDURE DIVISION.
      * Note: Things are indented in this division.
      * "MOVE "[text]" TO [string variable]": The equivalent of "="
           MOVE "Bob" TO USER-NAME.
      * "DISPLAY": The equivalent of "print".
      * You can just display a variable or you can add text in quotes around variables.
      * No extra syntax is needed (no , or +, just "").
           DISPLAY "Name: " USER-NAME.
      * Moving something else into a variable will overwrite what was previously in it.
           MOVE "Jack" TO USER-NAME.
           DISPLAY "Name: " USER-NAME.

      * Terminates the program. Add a newline to remove a warning.
       STOP RUN.
