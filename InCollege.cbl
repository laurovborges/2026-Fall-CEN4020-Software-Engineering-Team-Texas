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
           select output-file
               Assign to "InCollege-Output.txt"
               Organization is line sequential.
           Select user-file
               Assign to "InCollege-Usernames.txt"
               Organization is line sequential.
           Select check-user-file
               Assign to "InCollege-Usernames.txt"
               Organization is line sequential.
           Select pw-file
               Assign to "InCollege-Passwords.txt"
               Organization is line sequential.  
      * Used for initializing variables.
       DATA DIVISION.
      * Note: File section must go before working-storage section.
       File Section.
      * Creates a variable from the file control
       FD input-file.
      * Stores info from current line of file
       01 Input-Record pic X(1000).
       
       FD output-file.
       01 Output-Record pic X(1000).

       FD user-file.
       01 User-Record pic X(1000).

       FD check-user-file.
       01 Check-User-Record pic X(1000).

       FD pw-file.
       01 PW-Record pic X(1000).
      * Variables unrelated to files
       Working-Storage Section.
       01 username pic X(100).
       01 password pic X(12).

       01 output-message pic X(1000).
       01 input-char pic 9(5).
       01 user-num pic 9(5).
       01 user-length pic 9(5).
       01 pw-length pic 9(5).

       01 pw-line pic 9(5).
       01 curr-line pic 9(5).
      * The 3 character requirements for the password
       01 cap-flag pic X value "N".
       01 num-flag pic X value "N".
       01 spec-flag pic X value "N".

      * Check for spaces in password
       01 space-flag pic X value "N".

      * Is username unique or not
       01 unique-flag pic X value "Y".
      * Boolean to determine the end of file
       01 end-of-file pic X value "N".

       01 temp-username pic X(100).

      * This is where you do the actual logic stuff.
       PROCEDURE DIVISION.
           Open input input-file
           open output output-file

           Move "Welcome to InCollege!" to output-message
           Perform WRITE-OUTPUT

           Move "Log In" to output-message
           Perform WRITE-OUTPUT

           Move "Create New Account" to output-message
           Perform WRITE-OUTPUT

           Move "Enter your choice:" to output-message
           Perform WRITE-OUTPUT

      *     Open the files so they can do things

      *     Note: Read must happen once for each line in the file
      *     Note: This read is the main function
           Read input-file
               At end
                   Close input-file
                   Close output-file
                   STOP RUN
               Not at end
      *            User creates a new account
                   If Input-Record = "Create New Account"
                        Perform COUNT-ACCOUNTS
                     If user-num >=5
                         Move "All permitted accounts have been created,
      -                       " please come back later." 
                         to output-message
                         Perform WRITE-OUTPUT
                     else
                       Move "[Create New Account]" to output-message
                       Perform WRITE-OUTPUT
                       Perform CREATE-USERNAME
      *                Add username to user file
                       Open extend user-file
                       Move username to User-Record
                       Write User-Record
                       Close user-file
                       Perform CREATE-PASSWORD
      *                Add password to password file
                       Open extend pw-file
                       Move password to PW-Record
                       Write PW-Record
                       Close pw-file
                       Move "Your account has been created." 
                       to output-message
                       Perform WRITE-OUTPUT
                     End-if
                  End-if

      *            User logs in to existing account
                   If Input-Record = "Log In"
                       Move "[Log In]" to output-message
                       Perform WRITE-OUTPUT
                       Perform ENTER-USERNAME
                       Move 0 to curr-line
                       Perform ENTER-PASSWORD
                   End-if
               End-read.

           Close input-file
           Close output-file
      *    Note: Stop run goes before the functions ("paragraphs")
           STOP RUN.

           CREATE-USERNAME.
               Move "N" to unique-flag
               Perform until unique-flag = "Y"
                   Read input-file
                   At end
                       Close input-file
                       Close output-file
                       STOP RUN
                   Not at end
                       Move "Please create a username:"to output-message
                       Perform WRITE-OUTPUT
                       Move Input-Record to temp-username
                       Move "Y" to unique-flag
      *                Make sure username is unique
                       Open input check-user-file
                       Move "N" to end-of-file
                       Perform until end-of-file = "Y"
                           Read check-user-file
                           At end
                               Move "Y" to end-of-file
                           Not at end
                               If temp-username = Check-User-Record
                               Move "N" to unique-flag
                           End-if
                           End-read
                            End-perform
                           Close check-user-file
                           
      *        Display username even if not valid
               Move temp-username to output-message
               Perform WRITE-OUTPUT

               If unique-flag = "N"
                  Move "That username is already taken." 
                  to output-message
                  Perform WRITE-OUTPUT
               End-if
               
               If unique-flag = "Y"
                   If temp-username = Space
                       Move "Your username must not be blank."
                       to output-message
                       Perform WRITE-OUTPUT
                       Move "N" to unique-flag
                   End-if
      *            Make sure username doesn't contain spaces
                     Compute user-length = Function Length(Function Trim
      -                                    (temp-username))
                   Perform varying input-char from 1 by 1
                   Until input-char > user-length
                       If temp-username(input-char:1) = Space
                           Move "Your username must not contain spaces."
                           to output-message
                           Perform WRITE-OUTPUT
                           Move "N" to unique-flag
                       End-if
                   End-perform
               If unique-flag = "Y"
                  Move temp-username to username
               End-if
               End-read
               End-perform.

           CREATE-PASSWORD.
               Read input-file
                   At end
                       Close input-file
                       Close output-file
                       STOP RUN
                   Not at end
                       Move "Please create a password:" 
                       to output-message
                       Perform WRITE-OUTPUT
					   Move Input-Record(1:100)
					   to output-message
					   Perform WRITE-OUTPUT
      *                Reset the flags for character requirements
                       Move "N" to cap-flag
                       Move "N" to num-flag
                       Move "N" to spec-flag
      *                Note: Evaluate/when is like a switch/case.
      *                Note: Only way to do if-elif statements.
                       Evaluate TRUE
      *                    Note: (8:1) refers to the 8th character.
                           When Input-Record(8:1) = Space
                               Move "Your password must contain at least
      -                             " 8 characters."
                                      to output-message
                                      Perform WRITE-OUTPUT
      *                        If pw is invalid, run this code again
                               Perform CREATE-PASSWORD
                           When Input-Record(13:1) NOT = Space
                               Move "Your password must contain no more
      -                             "than 12 characters."
                                       to output-message
                                       Perform WRITE-OUTPUT
                               Perform CREATE-PASSWORD
      *                    Check the char reqs if the length is right
                           When OTHER
      *                        Password is created in this paragraph
                               Perform PASSWORD-CHECKS                        
                       End-evaluate
               End-read.

           ENTER-USERNAME.
               Read input-file
                   At end
                       Close input-file
                       Close output-file
                       STOP RUN
                   Not at end
                       Move "Please enter your username:" 
                       to output-message
                       Perform WRITE-OUTPUT
                       Move Input-Record to username
      *                If username exists in the system
                       Move "N" to end-of-file
                       Move 0 to pw-line
                       Open input user-file
                       Perform until end-of-file = "Y"
                           Read user-file
                               At end
                                   Move "Y" to end-of-file
                               Not at end
                                   Add 1 to pw-line
                                   If username = User-Record
                                       Move username to output-message
                                       Perform WRITE-OUTPUT
                                       Exit perform
                                   End-if
                            End-read
                        End-perform
                        Close user-file
                        If end-of-file = "Y"
                           Move username to output-message
                           Perform WRITE-OUTPUT
                           Move "Incorrect username. Please try again."
      -                         to output-message
                           Perform WRITE-OUTPUT
                           Perform ENTER-USERNAME
                        End-if
               End-read.

           ENTER-PASSWORD.
               Read input-file
                   At end
                       Close input-file
                       Close output-file
                       STOP RUN
                   Not at end
                       Move "Please enter your password:" 
                       to output-message
                       Perform WRITE-OUTPUT
                       Move Input-Record to password
					   Move password
					   to output-message
					   Perform WRITE-OUTPUT

      *                If password exists in the system
                       Move "N" to end-of-file
                       Open input pw-file
                       Perform until curr-line = pw-line
                           Read pw-file
                               Not at end
                                   Add 1 to curr-line
                           End-read
                       End-perform
                       
                       If password = PW-Record
                           Close pw-file
      *                 Note: This is where the user can navigate to different parts of the program                    
                           Move "You have successfully logged in."
                           to output-message
                           Perform WRITE-OUTPUT
                           Initialize output-message
                           String
                           "Welcome " Delimited by Size
                           Function Trim(username) Delimited by size
                           "!" Delimited by Size
                           Into output-message
                           END-STRING
                           Perform WRITE-OUTPUT
                           Perform NAVIGATION        
                       Else
                           Move "Incorrect password. Please try again."
                               to output-message
                           Perform WRITE-OUTPUT
                           Close pw-file
                           Perform ENTER-PASSWORD
                       End-if
               End-read.

           PASSWORD-CHECKS.
      *        Note: "Perform varying" is like a for-loop
      *        Check for a capital letter
               Move "N" to space-flag
               Compute pw-length = Function Length(Function Trim
      -                                           (Input-Record))
               Perform varying input-char from 1 by 1
                   Until input-char > pw-length
                   If Input-Record(input-char:1) = Space
                       Move "Y" to space-flag
                       Exit perform
                   End-if
               End-perform

               If space-flag = "Y"
                   Move "Your password must not contain spaces"
      -            to output-message
                   Perform WRITE-OUTPUT
                   Perform CREATE-PASSWORD
               Else
               
               Perform varying input-char from 1 by 1
                   Until Input-Record(input-char:1) = Space
                      OR cap-flag = "Y"
                         If Input-Record(input-char:1) >= "A"
                         AND Input-Record(input-char:1) <= "Z"
                               Move "Y" to cap-flag
                           End-if
               End-perform
               If cap-flag = "N"
                    Move "Your password must contain at least 1 capital
      -                  "letter." 
                           to output-message
                   Perform WRITE-OUTPUT
               Perform CREATE-PASSWORD
               End-if

      *        Check for a number
               Perform varying input-char from 1 by 1
                   Until Input-Record(input-char:1) = Space
                      OR num-flag = "Y"
                           If Input-Record(input-char:1) IS numeric
                               Move "Y" to num-flag
                           End-if
               End-perform
               If num-flag = "N"
                   Move "Your password must contain at least 1 digit."
                   to output-message
                   Perform WRITE-OUTPUT
                   Perform CREATE-PASSWORD
               End-if

      *        Check for a special character
               Perform varying input-char from 1 by 1
                   Until Input-Record(input-char:1) = Space
                      OR spec-flag = "Y"
                         If Input-Record(input-char:1) IS NOT alphabetic
                         AND Input-Record(input-char:1) IS NOT numeric
                               Move "Y" to spec-flag
                         End-if
               End-perform
               If spec-flag = "N"
                    Move "Your password must contain at least 1 special
      -                  "character."                   
                       to output-message
                   Perform WRITE-OUTPUT
                   Perform CREATE-PASSWORD
               End-if

      *        Flag that the requirements are all met
               If cap-flag = "Y" AND num-flag = "Y"
                  AND spec-flag = "Y"
                   Move Input-Record to password
      *        Reset the flags and try again    
               Else            
                   Move "N" to cap-flag
                   Move "N" to num-flag
                   Move "N" to spec-flag
                   Perform CREATE-PASSWORD
               End-if.

           COUNT-ACCOUNTS.
      *        Count how many account exist in the system
      *        Note: This is for limiting accounts
                Move 0 to user-num
                open input user-file
                Move "N" to end-of-file
                Perform until end-of-file = "Y"
                     Read user-file
                          At end
                            Move "Y" to end-of-file
                          Not at end
                            Add 1 to user-num
                     End-read
                End-perform
                Close user-file.
               
      *        Note: This is for the navigation menu 
           NAVIGATION.
               Move "1. Search for a job" to output-message
                Perform WRITE-OUTPUT
               Move "2. Find someone you know" to output-message
                Perform WRITE-OUTPUT
               Move "3. Learn a new skill" to output-message
                Perform WRITE-OUTPUT
               Move "4. Log Out" to output-message
                Perform WRITE-OUTPUT
               Move "Enter your choice:" to output-message
                Perform WRITE-OUTPUT

               Read input-file
                   At end
                       Close input-file
                       Close output-file
                       STOP RUN
                   Not at end
                       If Input-Record = "1"
                           Move "Job search/internship is under construc
      -                         "tion." 
                                 to output-message
                               Perform WRITE-OUTPUT
                       Else
                           If Input-Record = "2"
                               Move "Find someone you know is under cons
      -                             "truction."
                                    to output-message
                               Perform WRITE-OUTPUT
                           Else
                               If Input-Record = "3"
                                   perform LEARN-SKILL
                               Else
                                   If Input-Record = "4"
                                       Close input-file
                                       Close output-file
                                       STOP RUN

                                   End-if
                               End-if
                           End-if
                       End-if
               End-read.
           LEARN-SKILL.
                   Move "Learn a New Skill:" to output-message
                   Perform WRITE-OUTPUT
                   Move "Skill 1" to output-message
                   Perform WRITE-OUTPUT
                   Move "Skill 2" to output-message
                   Perform WRITE-OUTPUT
                   Move "Skill 3" to output-message
                   Perform WRITE-OUTPUT
                   Move "Skill 4" to output-message
                   Perform WRITE-OUTPUT
                   Move "Skill 5" to output-message
                   Perform WRITE-OUTPUT
                   Move "Go Back" to output-message
                   Perform WRITE-OUTPUT
                   Move "Enter your choice:" to output-message
                   Perform WRITE-OUTPUT
                   Read input-file
                       At end
                           Close input-file
                           Close output-file
                           STOP RUN
                          Not at end
                            If Input-Record = "Skill 1"
                                Move "Skill 1 is under construction." 
                                to output-message
                                Perform WRITE-OUTPUT
                            Else
                                If Input-Record = "Skill 2"
                                   Move "Skill 2 is under construction."
                                    to output-message
                                    Perform WRITE-OUTPUT
                                Else
                                    If Input-Record = "Skill 3"
                                        Move "Skill 3 is under construct
      -                                      "ion."
                                        to output-message
                                        Perform WRITE-OUTPUT
                                    Else
                                        If Input-Record = "Skill 4"
                                            Move "Skill 4 is under const
      -                                          "ruction."
                                            to output-message
                                            Perform WRITE-OUTPUT
                                        Else
                                            If Input-Record = "Skill 5"
                                                Move "Skill 5 is under c
      -                                              "onstruction."
                                                to output-message
                                                Perform WRITE-OUTPUT
                                            Else
                                               If Input-Record="Go Back"
                                                    Perform NAVIGATION
                                               End-if
                                            End-if
                                        End-if
                                    End-if
                                End-if
                            End-if
                   End-Read.

           WRITE-OUTPUT.
               Display function Trim(output-message)
               Move function Trim(output-message) to Output-Record
               Write Output-Record.

