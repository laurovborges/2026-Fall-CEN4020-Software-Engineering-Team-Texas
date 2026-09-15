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
           Select profile-file
               Assign to "InCollege-Profiles.txt"
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

         FD profile-file.
         01 Profile-Record pic X(1000).
      * Variables unrelated to files
       Working-Storage Section.
       01 username pic X(100).
       01 password pic X(12).
       01 Account-Username pic X(100).
       01 Account-Password pic X(12).
       01 First-Name pic A(50).
       01 Last-Name pic A(50).
       01 College-Name pic A(100).
       01 College-Major pic A(50).
       01 Graduation-Year pic 9(4).
       01 About-Me pic X(1000).
       01 stored-password pic X(12).
         01 Profile-Username pic X(100).
         01 Profile-Year-Input pic X(100).


         01 Profile-Found pic X value "N".
         01 Profile-Input-Length pic 9(5).
         01 Profile-Year-Length pic 9(5).
         01 Profile-Saved pic X value "N".
        01 New-Profile-Record pic X(5000).

         01 Experience-count pic 9 value 0.

         01 Education-count pic 9 value 0.

         01 Entry-Index pic 9 value 0.

         01 Experience-table.
            05 Experience-Entry occurs 3 times.
               10 Experience-Company pic X(100).
               10 Experience-Title pic X(100).
               10 Experience-Dates pic X(50).
               10 Experience-Description pic X(100).
         01 Education-table.
            05 Education-Entry occurs 3 times.
               10 Education-University pic X(100).
               10 Education-Degree pic X(100).
               10 Education-Years pic X(50).
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
                   Perform ECHO-INPUT
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
                       Perform NAVIGATION
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
                                   Move "Your password must contain no m
      -                                "ore than 12 characters."
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
                           Perform ECHO-INPUT
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
               Move "1. Create/Edit My Profile" to output-message
                Perform WRITE-OUTPUT
               Move "2. View My Profile" to output-message
                Perform WRITE-OUTPUT
               Move "3. Search for a job" to output-message
                Perform WRITE-OUTPUT
               Move "4. Find someone you know" to output-message
                Perform WRITE-OUTPUT
               Move "5. Learn a new skill" to output-message
                Perform WRITE-OUTPUT
               Move "6. Log Out" to output-message
                Perform WRITE-OUTPUT
               Move "Enter your choice:" to output-message
                Perform WRITE-OUTPUT

               Read input-file
                   At end
                       Close input-file
                       Close output-file
                       STOP RUN
                   Not at end
                       Perform ECHO-INPUT
                       Evaluate Input-Record
                           When "1"
                               Move "--- Create/Edit Profile ---"
                                   to output-message
                               Perform WRITE-OUTPUT
                               Perform CREATE-PROFILE
                           When "2"
                               Move "--- Your Profile ---"
                                   to output-message
                               Perform WRITE-OUTPUT
                               Perform VIEW-PROFILE
                           When "3"
                               Move "Job search/internship is under
      -                            "construction."
                                   to output-message
                               Perform WRITE-OUTPUT
                           When "4"
                               Move "Find someone you know is under
      -                            "construction."
                                   to output-message
                               Perform WRITE-OUTPUT
                           When "5"
                               Perform LEARN-SKILL
                           When "6"
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
                                     Perform ECHO-INPUT
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

           CREATE-PROFILE.
               Move Space to First-Name
               Perform until First-Name NOT = Space
                   Move "Enter First Name:" to output-message
                   Perform WRITE-OUTPUT
                   Read input-file
                       At end
                           Close input-file
                           Close output-file
                           STOP RUN
                       Not at end
                           Perform ECHO-INPUT
                           Move Input-Record(1:50) to First-Name
                   End-read
                   If First-Name = Space
                       Move "First Name is required." to output-message
                       Perform WRITE-OUTPUT
                   End-if
               End-perform

               Move Space to Last-Name
               Perform until Last-Name NOT = Space
                   Move "Enter Last Name:" to output-message
                   Perform WRITE-OUTPUT
                   Read input-file
                       At end
                           Close input-file
                           Close output-file
                           STOP RUN
                       Not at end
                           Perform ECHO-INPUT
                           Move Input-Record(1:50) to Last-Name
                   End-read
                   If Last-Name = Space
                       Move "Last Name is required." to output-message
                       Perform WRITE-OUTPUT
                   End-if
               End-perform

               Move Space to College-Name
               Perform until College-Name NOT = Space
                   Move "Enter University/College Attended:"
                       to output-message
                   Perform WRITE-OUTPUT
                   Read input-file
                       At end
                           Close input-file
                           Close output-file
                           STOP RUN
                       Not at end
                           Perform ECHO-INPUT
                           Move Input-Record(1:100) to College-Name
                   End-read
                   If College-Name = Space
                       Move "University/College is required."
                           to output-message
                       Perform WRITE-OUTPUT
                   End-if
               End-perform

               Move Space to College-Major
               Perform until College-Major NOT = Space
                   Move "Enter Major:" to output-message
                   Perform WRITE-OUTPUT
                   Read input-file
                       At end
                           Close input-file
                           Close output-file
                           STOP RUN
                       Not at end
                           Perform ECHO-INPUT
                           Move Input-Record(1:50) to College-Major
                   End-read
                   If College-Major = Space
                       Move "Major is required." to output-message
                       Perform WRITE-OUTPUT
                   End-if
               End-perform

               Move "N" to Profile-Saved
               Perform until Profile-Saved = "Y"
                   Move "Enter Graduation Year (YYYY):"
                       to output-message
                   Perform WRITE-OUTPUT
                   Read input-file
                       At end
                           Close input-file
                           Close output-file
                           STOP RUN
                       Not at end
                           Perform ECHO-INPUT
                           Move Input-Record(1:100)
                               to Profile-Year-Input
                           Compute Profile-Year-Length = Function Length
                               (Function Trim(Profile-Year-Input))
                           If Profile-Year-Length = 4
                           And Profile-Year-Input(1:4) IS NUMERIC
                               Move Profile-Year-Input(1:4)
                                   to Graduation-Year
                               If Graduation-Year > 2025
                               And Graduation-Year < 2034
                                   Move "Y" to Profile-Saved
                               Else
                                   Move "Graduation Year must be between
      -                                "2026 and 2033."
                                       to output-message
                                   Perform WRITE-OUTPUT
                               End-if
                           Else
                               Move "Graduation Year must be a valid
      -                            "4-digit year."
                                   to output-message
                               Perform WRITE-OUTPUT
                           End-if
                   End-read
               End-perform

               Move 201 to Profile-Input-Length
               Perform until Profile-Input-Length <= 200
                   Move "Enter About Me (optional; blank skips):"
                       to output-message
                   Perform WRITE-OUTPUT
                   Read input-file
                       At end
                           Close input-file
                           Close output-file
                           STOP RUN
                       Not at end
                           Perform ECHO-INPUT
                           Compute Profile-Input-Length = Function
                               Length (Function Trim(Input-Record))
                           If Profile-Input-Length > 200
                               Move "About Me must be 200 characters or
      -                            "less."
                                   to output-message
                               Perform WRITE-OUTPUT
                           Else
                               Move Input-Record(1:200) to About-Me
                           End-if
                   End-read
               End-perform

               Move 9 to Experience-count
               Perform until Experience-count <= 3
                   Move "How many experience entries? (0-3):"
                       to output-message
                   Perform WRITE-OUTPUT
                   read input-file
                       At end
                           Close input-file
                           Close output-file
                           STOP RUN
                       Not at end
                          Perform ECHO-INPUT

                          Compute Profile-Input-Length = Function Length
                               (Function Trim(Input-Record))

                          If Profile-Input-Length = 1
                          And Input-Record(1:1) IS NUMERIC
                              Move Input-Record(1:1)
                                  to Experience-count

                              If Experience-count > 3
                                  Move "Experience entries must be 0-3."
                                       to output-message
                                  Perform WRITE-OUTPUT
                              End-if
                          Else
                              Move 9 to Experience-count
                              Move "Experience entries must be 0-3."
                                  to output-message
                              Perform WRITE-OUTPUT
                          End-if
                     End-read
                End-perform
               
               Perform varying Entry-Index from 1 by 1
                   Until Entry-Index > Experience-count

                   Move "Enter Experience Title:" to output-message
                   Perform WRITE-OUTPUT
                   Read input-file
                   Perform ECHO-INPUT
                   Move Input-Record to Experience-Title(Entry-Index)

                   Move "Enter Experience Company:" to output-message
                   Perform WRITE-OUTPUT
                   Read input-file
                   Perform ECHO-INPUT
                   Move Input-Record to Experience-Company(Entry-Index)

                   Move "Enter Experience Dates:" to output-message
                   Perform WRITE-OUTPUT
                   Read input-file
                   Perform ECHO-INPUT
                   Move Input-Record to Experience-Dates(Entry-Index)

                   Move "Enter Experience Description:" 
                       to output-message
                   Perform WRITE-OUTPUT
                   Read input-file
                   Perform ECHO-INPUT
                   Move Input-Record 
                       to Experience-Description(Entry-Index)
                End-perform

                 Move 9 to Education-count
               Perform until Education-count <= 3
                   Move "How many education entries? (0-3):"
                       to output-message
                   Perform WRITE-OUTPUT
                   read input-file
                       At end
                           Close input-file
                           Close output-file
                           STOP RUN
                       Not at end
                           Perform ECHO-INPUT

                          Compute Profile-Input-Length = Function Length
                              (Function Trim(Input-Record))

                          If Profile-Input-Length = 1
                          And Input-Record(1:1) IS NUMERIC
                              Move Input-Record(1:1)
                                  to Education-count

                              If Education-count > 3
                                  Move "Education entries must be 0-3."
                                      to output-message
                                  Perform WRITE-OUTPUT
                              End-if
                           Else
                              Move 9 to Education-count
                              Move "Education entries must be 0-3."
                                  to output-message
                              Perform WRITE-OUTPUT
                          End-if
                     End-read
                End-perform

                Perform varying Entry-Index from 1 by 1
                   Until Entry-Index > Education-count

                   Move "Enter Education University:" to output-message
                   Perform WRITE-OUTPUT
                   Read input-file
                   Perform ECHO-INPUT
                   Move Input-Record 
                   to Education-University(Entry-Index)

                   Move "Enter Education Degree:" to output-message
                   Perform WRITE-OUTPUT
                   Read input-file
                   Perform ECHO-INPUT
                   Move Input-Record 
                   to Education-Degree(Entry-Index)

                   Move "Enter Education Years:" to output-message
                   Perform WRITE-OUTPUT
                   Read input-file
                   Perform ECHO-INPUT
                   Move Input-Record 
                   to Education-Years(Entry-Index)
                End-perform

               Initialize New-Profile-Record
               String
                   Function Trim(username) Delimited by Size
                   "|" Delimited by Size
                   Function Trim(First-Name) Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Last-Name) Delimited by Size
                   "|" Delimited by Size
                   Function Trim(College-Name) Delimited by Size
                   "|" Delimited by Size
                   Function Trim(College-Major) Delimited by Size
                   "|" Delimited by Size
                   Graduation-Year Delimited by Size
                   "|" Delimited by Size
                   Function Trim(About-Me) Delimited by Size
                   "|" Delimited by Size

                   Experience-count Delimited by Size
                   "|" Delimited by Size

                   Function Trim(Experience-Title(1)) Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Experience-Company(1)) 
                   Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Experience-Dates(1)) Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Experience-Description(1)) 
                   Delimited by Size
                   "|" Delimited by Size

                   Function Trim(Experience-Title(2)) Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Experience-Company(2)) 
                   Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Experience-Dates(2)) Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Experience-Description(2)) 
                   Delimited by Size
                   "|" Delimited by Size

                   Function Trim(Experience-Title(3)) Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Experience-Company(3)) 
                   Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Experience-Dates(3)) Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Experience-Description(3)) 
                   Delimited by Size
                   "|" Delimited by Size

                   Education-count Delimited by Size
                   "|" Delimited by Size

                   Function Trim(Education-University(1)) 
                   Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Education-Degree(1)) 
                   Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Education-Years(1)) 
                   Delimited by Size
                   "|" Delimited by Size

                   Function Trim(Education-University(2)) 
                   Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Education-Degree(2)) 
                   Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Education-Years(2)) 
                   Delimited by Size
                   "|" Delimited by Size

                   Function Trim(Education-University(3)) 
                   Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Education-Degree(3)) 
                   Delimited by Size
                   "|" Delimited by Size
                   Function Trim(Education-Years(3)) 
                   Delimited by Size
                   "|" Delimited by Size
                  
                   
                   Into New-Profile-Record
               End-String

               Move "N" to Profile-Found
               Move "N" to end-of-file
               Open input profile-file
               Perform until end-of-file = "Y"
                   Read profile-file
                       At end
                           Move "Y" to end-of-file
                       Not at end
                           Unstring Profile-Record
                               Delimited by "|"
                               Into Profile-Username
                                    First-Name
                                    Last-Name
                                    College-Name
                                    College-Major
                                    Graduation-Year
                                    About-Me
                           End-Unstring
                           If Profile-Username = username
                               Move "Y" to Profile-Found
                           End-if
                   End-read
               End-perform
               Close profile-file
               Open extend profile-file
               Move New-Profile-Record to Profile-Record
               Write Profile-Record
               Close profile-file

               Move "Profile saved successfully!" to output-message
               Perform WRITE-OUTPUT
               Perform RETURN-TO-MENU.

           VIEW-PROFILE.
               Move "N" to Profile-Found
               Move "N" to end-of-file
               Open input profile-file
               Perform until end-of-file = "Y"
                   Read profile-file
                       At end
                           Move "Y" to end-of-file
                       Not at end
                           Unstring Profile-Record
                               Delimited by "|"
                               Into Profile-Username
                                    First-Name
                                    Last-Name
                                    College-Name
                                    College-Major
                                    Graduation-Year
                                    About-Me
                                    Experience-count
                                    Experience-Title(1)
                                    Experience-Company(1)
                                    Experience-Dates(1)
                                    Experience-Description(1)
                                    Experience-Title(2)
                                    Experience-Company(2)
                                    Experience-Dates(2)
                                    Experience-Description(2)
                                    Experience-Title(3)
                                    Experience-Company(3)
                                    Experience-Dates(3)
                                    Experience-Description(3)
                                    Education-count
                                    Education-University(1)
                                    Education-Degree(1)
                                    Education-Years(1)
                                    Education-University(2)
                                    Education-Degree(2)
                                    Education-Years(2)
                                    Education-University(3)
                                    Education-Degree(3)
                                    Education-Years(3)
                           End-Unstring
                           If Profile-Username = username
                               Move "Y" to Profile-Found
                           End-if
                   End-read
               End-perform
               Close profile-file
               If Profile-Found = "N"
                   Move "Profile not created." to output-message
                   Perform WRITE-OUTPUT
               Else
                   Initialize output-message
                   String
                       "Name: " Delimited by Size
                       Function Trim(First-Name) Delimited by Size
                       Space Delimited by Size
                       Function Trim(Last-Name) Delimited by Size
                       Into output-message
                   End-String
                   Perform WRITE-OUTPUT
                   Initialize output-message
                   String
                       "University: " Delimited by Size
                       Function Trim(College-Name) Delimited by Size
                       Into output-message
                   End-String
                   Perform WRITE-OUTPUT
                   Initialize output-message
                   String
                       "Major: " Delimited by Size
                       Function Trim(College-Major) Delimited by Size
                       Into output-message
                   End-String
                   Perform WRITE-OUTPUT
                   Initialize output-message
                   String
                       "Graduation Year: " Delimited by Size
                       Graduation-Year Delimited by Size
                       Into output-message
                   End-String
                   Perform WRITE-OUTPUT
                   Initialize output-message
                   String
                       "About Me: " Delimited by Size
                       Function Trim(About-Me) Delimited by Size
                       Into output-message
                   End-String
                   Perform WRITE-OUTPUT

                   If Experience-count > 0
                   Move "Experience:" to output-message
                   Perform WRITE-OUTPUT
                     Perform varying Entry-Index from 1 by 1
                          Until Entry-Index > Experience-count
                          
                          Initialize output-message
                          String
                            "Title: " Delimited by Size
                            Function Trim(
                                Experience-Title(Entry-Index))
                                 Delimited by Size
                            Into output-message
                          End-String
                          Perform WRITE-OUTPUT
                          Initialize output-message
                          String
                            "Company: " Delimited by Size
                            Function Trim(
                                Experience-Company(Entry-Index))
                                 Delimited by Size
                            Into output-message
                          End-String
                          Perform WRITE-OUTPUT
                          Initialize output-message
                          String
                            "Dates: " Delimited by Size
                            Function Trim(
                                Experience-Dates(Entry-Index))
                                 Delimited by Size
                            Into output-message
                          End-String
                          Perform WRITE-OUTPUT
                          Initialize output-message
                          String
                            "Description: " Delimited by Size
                            Function Trim(
                                Experience-Description(Entry-Index))
                                 Delimited by Size
                            Into output-message
                          End-String
                          Perform WRITE-OUTPUT
                     End-perform
               End-if

                If Education-count > 0
                     Move "Education:" to output-message
                     Perform WRITE-OUTPUT
                        Perform varying Entry-Index from 1 by 1
                              Until Entry-Index > Education-count
                              
                              Initialize output-message
                              String
                             "University: " Delimited by Size
                             Function Trim(
                                  Education-University(Entry-Index))
                                    Delimited by Size
                             Into output-message
                              End-String
                              Perform WRITE-OUTPUT
                              Initialize output-message
                              String
                             "Degree: " Delimited by Size
                             Function Trim(
                                  Education-Degree(Entry-Index))
                                    Delimited by Size
                             Into output-message
                              End-String
                              Perform WRITE-OUTPUT
                              Initialize output-message
                              String
                             "Years: " Delimited by Size
                             Function Trim(
                                  Education-Years(Entry-Index))
                                    Delimited by Size
                             Into output-message
                              End-String
                              Perform WRITE-OUTPUT
                        End-perform
                End-if
               Perform RETURN-TO-MENU.

           RETURN-TO-MENU.
               Move "Return to main menu? (Y/N)" to output-message
               Perform WRITE-OUTPUT
               Read input-file
                   At end
                       Close input-file
                       Close output-file
                       STOP RUN
                   Not at end
                       Perform ECHO-INPUT
                       If Input-Record = "Y" or Input-Record = "y"
                           Perform NAVIGATION
                       End-if
               End-read.

           ECHO-INPUT.
               Move Input-Record to output-message
               Perform WRITE-OUTPUT.

           WRITE-OUTPUT.
               Display function Trim(output-message)
               Move function Trim(output-message) to Output-Record
               Write Output-Record.

