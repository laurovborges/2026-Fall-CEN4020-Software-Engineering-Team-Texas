      *     Note: COBOL is not case-sensitive. I've chosen to capitalize
      *           divisions, "stop run", and paragraphs (functions).
      *           I capitalize the first letter of each sentence (line).
      *           For section names, I capitalize the first letter of
      *           each word.

      *     Note: The order of divisions and sections is specific. The
      *           order they are in is required.

      *     Note: COBOL doesn't let you use code beyond this bar ------>
      *           Create paragraphs to make the code fit in this area.

      * Has details about the program. Only the ID is required.
       IDENTIFICATION DIVISION.
       Program-ID. InCollege.

      * Tells the program what files to use and how to read them.
       ENVIRONMENT DIVISION.
       Input-Output Section.
       File-Control.
           Select input-file
               Assign to "InCollege-Input.txt"
      *        Reads the file line by line
               Organization is line sequential.

      * Used for initializing variables.
       DATA DIVISION.
      * Note: File section must go before working-storage section.
       File Section.
      * Creates a variable from the file control
       FD input-file.
      * Stores info from current line of file
       01 Input-Record pic X(1000).
       Working-Storage Section.
       01 username pic X(100).
       01 password pic X(12).
      * Boolean to determine the end of file
       01 end-of-file pic X value "N".
      * This is where you do the actual logic stuff.
       PROCEDURE DIVISION.
           Display "Welcome to InCollege!".
           Display "Log In".
           Display "Create New Account".
           Display "Enter your choice:".
      *     Open the file and do things
           Open input input-file
      *     Note: Read must happen once for each line in the file
      *     Note: This read is the main function
           Read input-file
               Not at end
      *            User creates a new account
                   If Input-Record = "Create New Account"
                       Display "Please create a username:"
                       Perform CREATE-USERNAME
                       Display "Please create a password:"
                       Perform CREATE-PASSWORD
                       Display "Your account has been created."
                   End-if

      *            User logs in to existing account
                   If Input-Record = "Log In"
                       Display "Please enter your username:"
                       Perform ENTER-USERNAME
                       Display "Please enter your password:"
                       Perform ENTER-PASSWORD
                       Display "You have successfully logged in."
                   End-if
               End-read.
      *        IGNORE THESE COMMENTS
      *         perform until eof
      *         read
      *         if user = input
      *                end the file
      *         endread
      *         endperform

           Close input-file
      *    Note: Stop run goes before the functions ("paragraphs")
           STOP RUN.

           CREATE-USERNAME.
               Read input-file
                   Not at end
      *                Make sure username is unique
                       Move Input-Record to username
      *                Add username to user file
               End-read.

           CREATE-PASSWORD.
               Read input-file
                   Not at end
      *                Note: Evaluate/when is like a switch/case
      *                Note: Only way to do if-elif statements
                       Evaluate TRUE
      *                    Note: (8:1) means check the 8th character.
                           When Input-Record(8:1) = Space
      *                        If pw is invalid, run this code again
                               Perform CREATE-PASSWORD
                           When Input-Record(13:1) NOT = Space
                               Perform CREATE-PASSWORD
      *                    When password doesn't contain:
      *                    1 special char, 1 digit, or 1 capital letter
      *                        Perform CREATE-PASSWORD
      *                    Only true when no other conditions are met
                           When OTHER
                               Move Input-Record to password
      *                        Add password to pw file
                       End-evaluate
               End-read.

           ENTER-USERNAME.
               Read input-file
                   Not at end
      *                If username exists in the system
                       Move Input-Record to username
               End-read.

           ENTER-PASSWORD.
               Read input-file
                   Not at end
      *                If password exists in the system
                       Move Input-Record to password
               End-read.
