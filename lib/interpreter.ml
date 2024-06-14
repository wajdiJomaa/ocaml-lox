open Parser;;
type result = NUMBER of int | STRING of string | BOOL of bool

let rec repeat_string s n =
  if n <= 0 then "" else s ^ repeat_string s (n-1)  

let rec evaluate parser= 
  match parser with 
  | Parser.NUMBER x -> NUMBER x
  | STRING s -> STRING s
  | BINOP {left = x; right = y; operator = op} -> 
    let left = evaluate x in let right = evaluate y in  
      match op, left, right with
        | "+", NUMBER n1, NUMBER n2 -> NUMBER (n1 + n2)
        | "+", STRING s1, STRING s2 -> STRING (String.concat "" [s1;s2])
        | "-", NUMBER n1, NUMBER n2 -> NUMBER (n1 - n2)
        | "*", NUMBER n1, NUMBER n2 -> NUMBER (n1 * n2)
        | "*", STRING s1, NUMBER n2 -> STRING (repeat_string s1 n2) 
        | "*", NUMBER n1, STRING s2 -> STRING (repeat_string s2 n1) 
        | "/", NUMBER n1, NUMBER n2 -> NUMBER (n1 / n2)
        | ">", NUMBER n1, NUMBER n2 -> BOOL (n1 > n2)
        | ">=", NUMBER n1, NUMBER n2 -> BOOL (n1 >= n2)
        | "<", NUMBER n1, NUMBER n2 ->  BOOL (n1 < n2)
        | "<=", NUMBER n1, NUMBER n2 -> BOOL (n1 <= n2)
        | "==", NUMBER n1, NUMBER n2 -> BOOL (n1 = n2)
        | _ -> raise (Failure "error")

let print_result res = 
  (match res with 
  | NUMBER num -> print_int num
  | STRING s -> print_string s
  | BOOL b -> if b then print_string "true" else print_string "false")
  ;print_newline ()