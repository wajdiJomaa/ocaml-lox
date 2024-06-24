# OLOX

olox is an interpreter for the lox programming language provided in the book 
[crafting interpreters](https://craftinginterpreters.com/)

the interepreter provided in the book is written in java, in this project i'm rewriting the
code in a functional style using ocaml

# OPERATORS

- Data Types: number of int, string (more to be added)

- Operators: +, -, *, / 

# Notes

- adding two strings will concatenate them `"abc" + "def" = "abcdef"`

- multiplying a string with number n repeat the string n times `"abc" * 2 = "abcabc"`

# VARIABLES

- to define a variable use : `var x 1`

- a variable is reachable only in its scope for sub scope for example 
`{var x = 1; {print x; //prints 1}} print x; //error `


# CONTROL FLOW

- to add a control flow statement use `if condition then doSomemthing else doSomethingElse`

- the else branch is optional

- for condition a number is true except 0, a string is true except the empty string

# run the code 

1. install [ocaml](https://ocaml.org/)
2. build the project `dune build`
3. run the code `dune exec olox`

