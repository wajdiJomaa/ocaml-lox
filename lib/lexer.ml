type token_type = PLUS | MINUS | MULT | DIV | NUMBER of int | STRING of string | NONE
                | BIGGER | BIGGEREQ | LESS | LESSEQ | EQ | NEQ| BOOL of bool | IDENTIFIER of string
                | VAR | PRINT | SEMICOLON | LEFTB | RIGHTB | IF | ELSE

type token = { t_type : token_type ; length: int; lexeme: string; }

let get_first s =  String.get s 0

let remove_n s n = String.sub s n (String.length s - n)

let is_digit digit =
  match digit with
   '0' .. '9' -> true
  | _ -> false

let is_alpha_numerique ch = match ch with  'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false

let is_lowercase ch = match ch with 'a' .. 'z' -> true | _ -> false 

let rec number s = 
  match s with 
  | "" -> []
  | s -> let fst = get_first s in 
    if is_digit fst then fst :: number (remove_n s 1) else []

let get_number s =
  let num = number s in
    let num_str = String.concat "" (List.map (String.make 1) num) in 
    { t_type = NUMBER (int_of_string num_str); length = String.length num_str; lexeme= num_str }

let rec string s = 
  match s with
  | "" -> raise (Failure "unterminated string") 
  | x -> let fst = get_first x in
      match fst with 
      | '"' -> []
      | c -> c :: string (remove_n x 1) 

let get_string s = 
  let res = string s in 
    let result = String.concat "" (List.map (String.make 1) res) in 
      { t_type = STRING result ; length = (String.length result) + 2; lexeme = result }

let rec keyword s = 
  match s with 
  | "" -> []
  | s -> if is_alpha_numerique (get_first s) 
          then get_first s :: keyword (remove_n s 1)
          else []

let get_keyword s = 
  let res = keyword s in 
  let result = String.concat "" (List.map (String.make 1) res) in 
  match result with
  | "true" -> { t_type = BOOL true; length = 4; lexeme = "true"}
  | "false" -> { t_type = BOOL false; length = 5; lexeme = "false"}
  | "print" -> { t_type = PRINT; length = 5; lexeme = "print"}
  | "var" -> { t_type = VAR; length = 3; lexeme = "var"}
  | "if" -> {t_type = IF; length = 2; lexeme = "if"}
  | "else" -> {t_type = ELSE; length = 4; lexeme = "else"}
  | _ -> {t_type = IDENTIFIER result; length = (String.length result); lexeme = result}

let get_token token s =
  match token with
    | '+' ->  { t_type = PLUS ; length = 1; lexeme = "+" }
    | '-' ->  { t_type = MINUS ; length = 1; lexeme = "-" }          
    | ' ' | '\n' ->  { t_type = NONE ; length = 1; lexeme = "" }
    | '*' -> { t_type = MULT; length = 1 ; lexeme = "*" }
    | '/' -> { t_type = DIV; length = 1 ; lexeme = "/" }
    | '>' ->  if get_first (remove_n s 1) = '=' 
        then { t_type = BIGGEREQ; length = 2; lexeme = ">="}
        else { t_type = BIGGER; length = 1; lexeme = ">"}
    | '<' -> if get_first (remove_n s 1) = '=' 
      then { t_type = LESSEQ; length = 2; lexeme = "<="}
      else { t_type = LESS; length = 1; lexeme = "<"}      
    | '=' -> if get_first (remove_n s 1) = '=' 
      then {t_type = EQ ; length = 2; lexeme="=="}
      else raise (Failure "= not supported")
    | '!' -> if get_first (remove_n s 1) = '=' 
      then {t_type = NEQ; length = 2; lexeme = "!="}
      else raise (Failure "unknown token")

    | '"' -> get_string (remove_n  s 1) 
    | ';' -> {t_type = SEMICOLON; length = 1; lexeme = ";"}
    | '}' -> {t_type = RIGHTB; length = 1; lexeme= "}"}
    | '{' -> {t_type = LEFTB; length = 1; lexeme= "{"}
    | c -> if is_digit c then get_number s 
            else  
              if is_lowercase c then get_keyword s
              else raise (Failure "unknown token")

let rec scan source =
  match source with
  | "" -> []
  | s ->  match get_token (get_first s) s with
          | { t_type = NONE; _ } -> scan (remove_n s 1)
          | token ->  token :: scan (remove_n s token.length) 