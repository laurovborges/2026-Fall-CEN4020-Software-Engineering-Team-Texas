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
       01 input-char pic 9(5).
       01 user-num pic 9(5).

       01 pw-line pic 9(5).
       01 curr-line pic 9(5).
      * The 3 character requirements for the password
       01 cap-flag pic X value "N".
       01 num-flag pic X value "N".
       01 spec-flag pic X value "N".

      * Is username unique or not
       01 unique-flag pic X value "Y".
      * Boolean to determine the end of file
       01 end-of-file pic X value "N".

       01 temp-username pic X(100).

      * This is where you do the actual logic stuff.
       PROCEDURE DIVISION.
           Display "Welcome to InCollege!".
           Display "Log In".
           Display "Create New Account".
           Display "Enter your choice:".
      *     Open the files so they can do things
           Open input input-file

      *     Note: Read must happen once for each line in the file
      *     Note: This read is the main function
           Read input-file
               Not at end
      *            User creates a new account
                   If Input-Record = "Create New Account"
                        Perform COUNT-ACCOUNTS
                     If user-num >=5
                         Display "All permitted accounts have been" 
                         "created, please come back later."
                     else
                       Display "[Create New Account]"
                       Perform CREATE-USERNAME
                       Display username
      *                Add username to user file
                       Open extend user-file
                       Move username to User-Record
                       Write User-Record
                       Close user-file
                       Perform CREATE-PASSWORD
                       Display password
      *                Add password to password file
                       Open extend pw-file
                       Move password to PW-Record
                       Write PW-Record
                       Close pw-file
                       Display "Your account has been created."
                     End-if
                  End-if

      *            User logs in to existing account
                   If Input-Record = "Log In"
                       Display "[Log In]"
                       Perform ENTER-USERNAME
                       Perform ENTER-PASSWORD
                   End-if
               End-read.

           Close input-file
      *    Note: Stop run goes before the functions ("paragraphs")
           STOP RUN.

           CREATE-USERNAME.
               Move "N" to unique-flag
               Perform until unique-flag = "Y"
                   Read input-file
                   At end
                       Move "Y" to unique-flag
                   Not at end
                       Display "Please create a username:"
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
                      

               If unique-flag = "Y"
                  Move temp-username to username
               Else
                   Display "That username is already taken."
                   Move "N" to unique-flag
               End-if
               End-read
               End-perform.

           CREATE-PASSWORD.
               Read input-file
                   Not at end
                       Display "Please create a password:"
      *                Reset the flags for character requirements
                       Move "N" to cap-flag
                       Move "N" to num-flag
                       Move "N" to spec-flag
      *                Note: Evaluate/when is like a switch/case.
      *                Note: Only way to do if-elif statements.
                       Evaluate TRUE
      *                    Note: (8:1) refers to the 8th character.
                           When Input-Record(8:1) = Space
                               Display Input-Record(1:100)
                               Display "Your password must contain "
                                       "at least 8 characters."
      *                        If pw is invalid, run this code again
                               Perform CREATE-PASSWORD
                           When Input-Record(13:1) NOT = Space
                               Display Input-Record(1:100)
                               Display "Your password must contain "
                                       "no more than 12 characters."
                               Perform CREATE-PASSWORD
      *                    Check the char reqs if the length is right
                           When OTHER
      *                        Password is created in this paragraph
                               Perform PASSWORD-CHECKS                        
                       End-evaluate
               End-read.

           ENTER-USERNAME.
               Read input-file
                   Not at end
                       Display "Please enter your username:"
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
                                       Display username
                                       Exit perform
                                   End-if
                            End-read
                        End-perform
                        Close user-file
                        If end-of-file = "Y"
                           Display username
                           Display "Incorrect username. "
                                   "Please try again."
                           Perform ENTER-USERNAME
                        End-if
               End-read.

           ENTER-PASSWORD.
               Read input-file
                   Not at end
                       Display "Please enter your password:"
                       Move Input-Record to password

      *                If password exists in the system
                       Move "N" to end-of-file
                       Move 0 to curr-line
                       Open input pw-file
                       Perform until curr-line = pw-line
                           Read pw-file
                               Not at end
                                   Add 1 to curr-line
                           End-read
                       End-perform
                       Display PW-Record
                       If password = PW-Record
                           Display password
                           Close pw-file
      *                 Note: This is where the user can navigate to different parts of the program                    
                           Display "You have successfully logged in."
                           Perform NAVIGATION        
                       Else
                           Display "Incorrect password. "
                                   "Please try again."
                           Close pw-file
                           Perform ENTER-PASSWORD
                       End-if
               End-read.

           PASSWORD-CHECKS.
      *        Note: "Perform varying" is like a for-loop
      *        Check for a capital letter
               Perform varying input-char from 1 by 1
                   Until Input-Record(input-char:1) = Space
                      OR cap-flag = "Y"
                         If Input-Record(input-char:1) >= "A"
                         AND Input-Record(input-char:1) <= "Z"
                               Move "Y" to cap-flag
                           End-if
               End-perform
               If cap-flag = "N"
                   Display Input-Record(1:100)
                   Display "Your password must contain "
                           "at least 1 capital letter." 
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
                   Display Input-Record(1:100)
                   Display "Your password must contain "
                           "at least 1 digit."
                   Perform CREATE-PASSWORD
               End-if.

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
                   Display Input-Record(1:100)
                   Display "Your password must contain "
                           "at least 1 special character."
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
               Display "1. Search for a job"
               Display "2. Find someone you know"
               Display "3. Learn a new skill"
               Display "4. Log Out"
               Display "Enter your choice:"

               Read input-file
                   Not at end
                       If Input-Record = "1"
                           Display "Job search/internship" 
                                   "is under construction."
                       Else
                           If Input-Record = "2"
                               Display "Find someone you know is" 
                                       "under construction."
                           Else
                               If Input-Record = "3"
                                   perform LEARN-SKILL
                               Else
                                   If Input-Record = "4"
                                       Stop run
                                   End-if
                               End-if
                           End-if
                       End-if
               End-read.
           LEARN-SKILL.
                   Display "Learn a New Skill:"
                   Display "Skill 1"
                   Display "Skill 2"
                   Display "Skill 3"
                   Display "Skill 4"
                   Display "Skill 5"
                   Display "Go Back"
                   Display "Enter your choice:"
                   Read input-file
                          Not at end
                            If Input-Record = "Skill 1"
                                Display "Skill 1 is under construction."
                            Else
                                If Input-Record = "Skill 2"
                                    Display "Skill 2 is under" 
                                            "construction."
                                Else
                                    If Input-Record = "Skill 3"
                                        Display "Skill 3 is under" 
                                                "construction."
                                    Else
                                        If Input-Record = "Skill 4"
                                            Display "Skill 4 is under" 
                                                    "construction."
                                        Else
                                            If Input-Record = "Skill 5"
                                                Display "Skill 5 is" 
                                                "underconstruction."
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

                         