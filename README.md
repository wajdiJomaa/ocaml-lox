# OLOX

olox is an interpreter for the lox programming language provided in the book 
[crafting interpreters](https://craftinginterpreters.com/)

the interepreter provided in the book is written in java, in this project i'm rewriting the
code in a functional style using ocaml

# Features

- Data Types: number of int, string (more to be added)

- Operators: +, -, *, / 

# Notes

- adding two strings will concatenate them `"abc" + "def" = "abcdef"`

- multiplying a string with number n repeat the string n times `"abc" * 2 = "abcabc"`

# run the code 

1. install [ocaml](https://ocaml.org/)
2. build the project `dune build`
3. run the code `dune exec olox`

