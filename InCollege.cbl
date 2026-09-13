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
           Select account-file
               Assign to "InCollege-Accounts.txt"
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

       FD account-file.
       01 Account-Record pic X(1000).
      * Variables unrelated to files
       Working-Storage Section.
       01 username pic X(100).
       01 password pic X(12).
             01 Account-Username pic X(100).
             01 Account-Password pic X(12).
         01 stored-password pic X(12).

       01 output-message pic X(1000).
       01 input-char pic 9(5).
       01 user-num pic 9(5).
       01 user-length pic 9(5).
       01 pw-length pic 9(5).

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
         01 username-found pic X value "N".
         01 password-valid pic X value "N".
         01 login-successful pic X value "N".

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
                       Perform CREATE-PASSWORD
                     Open extend account-file
                     Initialize Account-Record
                     String
                         Function Trim(username) Delimited by Size
                         "|" Delimited by Size
                         Function Trim(password) Delimited by Size
                         Into Account-Record
                     End-String
                     Write Account-Record
                     Close account-file
                       Move "Your account has been created." 
                       to output-message
                       Perform WRITE-OUTPUT
                     End-if
                  End-if

      *            User logs in to existing account
                   If Input-Record = "Log In"
                       Move "[Log In]" to output-message
                       Perform WRITE-OUTPUT
                       Perform LOGIN
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
                           Move "Please create a username:"
                               to output-message
                           Perform WRITE-OUTPUT
                           Move Input-Record(1:100) to temp-username
                           Move "Y" to unique-flag
                           Open input account-file
                           Move "N" to end-of-file
                           Perform until end-of-file = "Y"
                               Read account-file
                                   At end
                                       Move "Y" to end-of-file
                                   Not at end
                                       Unstring Account-Record
                                           Delimited by "|"
                                           Into Account-Username
                                                Account-Password
                                       End-Unstring
                                       If temp-username
                                           = Account-Username
                                           Move "N" to unique-flag
                                       End-if
                               End-read
                           End-perform
                           Close account-file

                           Move temp-username to output-message
                           Perform WRITE-OUTPUT
                           If unique-flag = "N"
                               Move "That username is already taken."
                                   to output-message
                               Perform WRITE-OUTPUT
                           Else
                               If temp-username = Space
                                   Move "Your username must not be
      -                                "blank."
                                       to output-message
                                   Perform WRITE-OUTPUT
                                   Move "N" to unique-flag
                               Else
                                   Compute user-length = Function
                                       Length
                                       (Function Trim(temp-username))
                                 Perform varying input-char from 1 by 1
                                       Until input-char > user-length
                                       If temp-username(input-char:1)
                                           = Space
                                           Move "Your username must not
      -                                        "contain spaces."
                                               to output-message
                                           Perform WRITE-OUTPUT
                                           Move "N" to unique-flag
                                       End-if
                                   End-perform
                               End-if
                           End-if
                           If unique-flag = "Y"
                               Move temp-username to username
                           End-if
                   End-read
               End-perform.

           CREATE-PASSWORD.
               Move "N" to password-valid
               Perform until password-valid = "Y"
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
                           Compute pw-length = Function Length
                               (Function Trim(Input-Record))
                           If pw-length < 8
                               Move "Your password must contain at least
      -                            " 8 characters."
                                   to output-message
                               Perform WRITE-OUTPUT
                           Else
                               If pw-length > 12
                                   Move "Your password must contain no more
      -                                "than 12 characters."
                                       to output-message
                                   Perform WRITE-OUTPUT
                               Else
                                   Perform PASSWORD-CHECKS
                               End-if
                           End-if
                   End-read
               End-perform.

           ENTER-USERNAME.
               Move "N" to username-found
               Perform until username-found = "Y"
                   Read input-file
                       At end
                           Close input-file
                           Close output-file
                           STOP RUN
                       Not at end
                           Move "Please enter your username:"
                               to output-message
                           Perform WRITE-OUTPUT
                           Move Input-Record(1:100) to username
                           Move "N" to end-of-file
                           Open input account-file
                           Perform until end-of-file = "Y"
                               Read account-file
                                   At end
                                       Move "Y" to end-of-file
                                   Not at end
                                       Unstring Account-Record
                                           Delimited by "|"
                                           Into Account-Username
                                                Account-Password
                                       End-Unstring
                                       If username = Account-Username
                                           Move Account-Password
                                               to stored-password
                                           Move "Y" to username-found
                                           Exit perform
                                       End-if
                               End-read
                           End-perform
                           Close account-file
                           If username-found = "N"
                               Move "Incorrect username. Please
      -                            "try again."
                                   to output-message
                               Perform WRITE-OUTPUT
                           End-if
                   End-read
               End-perform.

           ENTER-PASSWORD.
               Move "N" to login-successful
               Perform until login-successful = "Y"
                   Read input-file
                       At end
                           Close input-file
                           Close output-file
                           STOP RUN
                       Not at end
                           Move "Please enter your password:"
                               to output-message
                           Perform WRITE-OUTPUT
                           Move Input-Record(1:12) to password
                           If password = stored-password
                               Move "Y" to login-successful
                               Move "You have successfully logged in."
                                   to output-message
                               Perform WRITE-OUTPUT
                               Initialize output-message
                               String
                                   "Welcome " Delimited by Size
                                   Function Trim(username)
                                       Delimited by size
                                   "!" Delimited by Size
                                   Into output-message
                               END-STRING
                               Perform WRITE-OUTPUT
                           Else
                               Move "Incorrect password. Please
      -                            "try again."
                                   to output-message
                               Perform WRITE-OUTPUT
                           End-if
                   End-read
               End-perform.

           LOGIN.
               Move "N" to login-successful
               Perform until login-successful = "Y"
                   Perform ENTER-USERNAME
                   If username-found = "Y"
                       Perform ENTER-PASSWORD
                   End-if
               End-perform
               Perform NAVIGATION.

           PASSWORD-CHECKS.
               Move "N" to password-valid
               Move "N" to cap-flag
               Move "N" to num-flag
               Move "N" to spec-flag
               Move "N" to space-flag
               Perform varying input-char from 1 by 1
                   Until input-char > pw-length
                   If Input-Record(input-char:1) = Space
                       Move "Y" to space-flag
                       Exit perform
                   End-if
               End-perform

               If space-flag = "Y"
                   Move "Your password must not contain spaces."
                       to output-message
                   Perform WRITE-OUTPUT
               Else
                   Perform varying input-char from 1 by 1
                       Until input-char > pw-length
                       If Input-Record(input-char:1) >= "A"
                       AND Input-Record(input-char:1) <= "Z"
                           Move "Y" to cap-flag
                       End-if
                       If Input-Record(input-char:1) IS numeric
                           Move "Y" to num-flag
                       End-if
                       If Input-Record(input-char:1) IS NOT alphabetic
                       AND Input-Record(input-char:1) IS NOT numeric
                           Move "Y" to spec-flag
                       End-if
                   End-perform
                   If cap-flag = "N"
                       Move "Your password must contain at least 1
      -                    "capital letter."
                           to output-message
                       Perform WRITE-OUTPUT
                   End-if
                   If num-flag = "N"
                       Move "Your password must contain at least 1
      -                    "digit."
                           to output-message
                       Perform WRITE-OUTPUT
                   End-if
                   If spec-flag = "N"
                       Move "Your password must contain at least 1
      -                    "special character."
                           to output-message
                       Perform WRITE-OUTPUT
                   End-if
                   If cap-flag = "Y" AND num-flag = "Y"
                   AND spec-flag = "Y"
                       Move Input-Record(1:12) to password
                       Move "Y" to password-valid
                   End-if
               End-if.

           COUNT-ACCOUNTS.
      *        Count how many account exist in the system
      *        Note: This is for limiting accounts
                Move 0 to user-num
                open input account-file
                Move "N" to end-of-file
                Perform until end-of-file = "Y"
                     Read account-file
                          At end
                            Move "Y" to end-of-file
                          Not at end
                            Add 1 to user-num
                     End-read
                End-perform
                Close account-file.
               
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
                       Evaluate Input-Record
                           When "1"
                               Move "Job search/internship is under
      -                            "construction."
                                   to output-message
                               Perform WRITE-OUTPUT
                           When "2"
                               Move "Find someone you know is under
      -                            "construction."
                                   to output-message
                               Perform WRITE-OUTPUT
                           When "3"
                               Perform LEARN-SKILL
                           When "4"
                               Close input-file
                               Close output-file
                               STOP RUN
                           When Other
                               Move "Invalid choice." to output-message
                               Perform WRITE-OUTPUT
                       End-evaluate
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
                            Evaluate Input-Record
                                When "Skill 1"
                                    Move "Skill 1 is under
      -                                "construction."
                                        to output-message
                                    Perform WRITE-OUTPUT
                                When "Skill 2"
                                    Move "Skill 2 is under
      -                                "construction."
                                        to output-message
                                    Perform WRITE-OUTPUT
                                When "Skill 3"
                                    Move "Skill 3 is under
      -                                "construction."
                                        to output-message
                                    Perform WRITE-OUTPUT
                                When "Skill 4"
                                    Move "Skill 4 is under
      -                                "construction."
                                        to output-message
                                    Perform WRITE-OUTPUT
                                When "Skill 5"
                                    Move "Skill 5 is under
      -                                "construction."
                                        to output-message
                                    Perform WRITE-OUTPUT
                                When "Go Back"
                                    Perform NAVIGATION
                                When Other
                                    Move "Invalid skill choice."
                                        to output-message
                                    Perform WRITE-OUTPUT
                            End-evaluate
                   End-Read.

           WRITE-OUTPUT.
               Display function Trim(output-message)
               Move function Trim(output-message) to Output-Record
               Write Output-Record.

