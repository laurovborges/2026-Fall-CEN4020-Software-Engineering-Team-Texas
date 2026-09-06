       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGIN.

       ENVIRONMENT DIVISION.
       Input-Output Section.
       File-Control.
           Select input-file
               Assign to "InCollege-Input.txt"
      *        Reads the file line by line
               Organization is line sequential.
       DATA DIVISION.
      * Variables go inside this section
       File Section.
       FD input-file.
       01 Input-Record Pic X(1000).
       Working-Storage Section.
      *01 line-count Pic 9(5).
       01 username Pic X(100). 
       01 password Pic X(12).
       01 end-of-file Pic X Value "N".
      * This is where you do the actual logic stuff 
       PROCEDURE DIVISION.
           Display "Welcome to InCollege!".
           Display "Log In".
           Display "Create New Account".
           Display "Enter your choice:".
      *     Read the file and do things
           Open input input-file
           Read input-file
               Not at end
                   If Input-Record = "Create New Account"
                       Display "Please create a username:"
                       Perform CREATE-USERNAME
                       Display "Please create a password:"
                       Perform CREATE-PASSWORD
                       Display "Your account has been created."
                   End-if

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
           STOP RUN.
                   
           CREATE-USERNAME.
               Read input-file
                   Not at end
                       Move Input-Record to username
               End-read.

           CREATE-PASSWORD.
               Read input-file
                   Not at end
                       Evaluate TRUE
                           When Input-Record(8:1) = Space
                               Perform CREATE-PASSWORD
                           When Input-Record(13:1) NOT = Space
                               Perform CREATE-PASSWORD
      *                    When password doesn't contain:
      *                    1 special char, 1 digit, or 1 capital letter
      *                        Perform CREATE-PASSWORD
                           When OTHER
                               Move Input-Record to password
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

           