type expression = BINOP of {left : expression; right: expression ; operator: string} 
                | NUMBER of int | STRING of string;;
type result = {value : expression; ls: Lexer.token list}

open Lexer;;

let parse_number tokens = 
  match (List.hd tokens).t_type with
  | NUMBER num  -> { value = NUMBER num ; ls = List.tl tokens } 
  | STRING s -> { value = STRING s; ls = List.tl tokens }
  | _ -> raise (Failure "expected number token") 

let rec binary_helper tokens prev exp check = 
    match tokens with
    | x :: xs -> 
          if check x.t_type then 
            let value = exp xs in 
            let operator =  BINOP {left = prev ; right = value.value; operator= x.lexeme} 
              in binary_helper value.ls operator exp check
          else  { value = prev; ls = tokens }
    | [] -> {value = prev; ls = []} 
  
let parse_mult tokens = let num = parse_number tokens in 
      let check x = match x with | MULT | DIV -> true | _ -> false in 
      binary_helper num.ls num.value parse_number check 

let parse_sum tokens = 
    let mult = parse_mult tokens in
    let check x = match x with | PLUS | MINUS -> true | _ -> false in 
    binary_helper mult.ls mult.value parse_mult check 

let parse tokens = let res = parse_sum tokens in res.value 
 