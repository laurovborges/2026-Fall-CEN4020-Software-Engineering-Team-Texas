# 2026-Fall-CEN4020-Software-Engineering-Team-Texas repo

Open the repo in WSL, then open the dev container.

## How to compile and run:
```sh
cobc -x -o InCollege InCollege.cbl
./InCollege
```

The input file is called "InCollege-Input.txt".
Each input should be on a new line.
If a choice is offered, the exact prompt option must be written to mark it as chosen.
For example, line 1 must contain either "Create New Account" or "Log In".
## Sample input:
```sh
Create New Account
MyUsername
MyPassword1!
```

The output file is called "InCollege-Output.txt".
The output file will contain predefined text prompts as well as the inputs from the input file.
## Sample output:
```sh
Welcome to InCollege!
Log In
Create New Account
Enter your choice:
[Create New Account]
Please create a username:
MyUsername
Please create a password:
MyPassword1!
```
