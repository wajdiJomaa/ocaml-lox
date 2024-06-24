type expression = BINOP of {left : expression; right: expression ; operator: string} 
                | NUMBER of int | STRING of string | UNARY of {operator: string; right: expression}
                | BOOL of bool | IDENTIFIER of string

type 'a result = {value : 'a ; ls: Lexer.token list}

type statement = SPRINT of expression | SNONE | EXP of expression 
                | SVAR of {iden: string ; exp: expression} | BLOCK of statement list
                | CONDITIONAL of {condition: expression ;success: statement; fail: statement}

open Lexer;;

let parse_number tokens : expression result= 
  match (List.hd tokens).t_type with
  | Lexer.NUMBER num  -> { value = NUMBER num ; ls = List.tl tokens } 
  | STRING s -> { value = STRING s; ls = List.tl tokens }
  | BOOL b -> { value = BOOL b; ls = List.tl tokens}
  | IDENTIFIER id -> {value = IDENTIFIER id; ls = List.tl tokens}
  | _ -> raise (Failure "expected number token") 

let rec parse_unary tokens = 
  match tokens with 
  x::xs -> 
    (match x.t_type with
            | MINUS -> let next = parse_unary xs in 
              {value = UNARY {operator = "-" ; right = next.value } ; ls = next.ls }
            | _ -> parse_number tokens)
  |[] -> parse_number []

let rec binary_helper tokens prev exp check = 
    match tokens with
    | x :: xs -> 
          if check x.t_type then 
            let value = exp xs in 
            let operator =  BINOP {left = prev ; right = value.value; operator= x.lexeme} 
              in binary_helper value.ls operator exp check
          else  { value = prev; ls = tokens }
    | [] -> {value = prev; ls = []} 
  
let parse_mult tokens = let u = parse_unary tokens in 
      let check x = match x with | MULT | DIV -> true | _ -> false in 
      binary_helper u.ls u.value parse_unary check 

let parse_sum tokens = 
    let mult = parse_mult tokens in
    let check x = match x with | PLUS | MINUS -> true | _ -> false in 
    binary_helper mult.ls mult.value parse_mult check 

let parse_comparasion tokens = 
    let sum = parse_sum tokens in
    let check x = match x with | LESS | LESSEQ | BIGGER | BIGGEREQ -> true | _ -> false in 
    binary_helper sum.ls sum.value parse_sum check 

let parse_equality tokens = 
  let comparasion = parse_comparasion tokens in 
    let check x = match x with | EQ | NEQ -> true | _ -> false in 
    binary_helper comparasion.ls comparasion.value parse_comparasion check

let match_semicolon x =
  match x with
  | SEMICOLON -> true
  | _ -> false 

let match_identifier x = 
  match x with
  | IDENTIFIER _ -> true
  | _ -> false

let consume_token ls match_t =
  match ls with 
  | [] -> raise (Failure "expected semicolon")
  | x::_ -> if (match_t x.t_type)  then x else raise (Failure "expected semicolon")
  
let parse_block ls parse_statement = 
  let rec helper ls =  
    match ls with
    | [] -> raise ( Failure "unfinished block" )
    | x :: xs -> match x .t_type with RIGHTB -> [] 
            | _ -> let stmt = parse_statement (x::xs) in stmt :: helper stmt.ls
  in let res = helper ls in if List.is_empty res then {value = BLOCK []; ls = List.tl ls}
      else
      { value = BLOCK (List.map (fun (x) -> x.value) res) ; ls = List.tl (List.nth res ((List.length res) -1)).ls}

let rec parse_statement tokens = 
  match tokens with
  |  [] -> raise (Failure "impossible case") 
  | x::xs -> match x.t_type with 
          | PRINT  -> let exp = parse_equality xs in let _ = consume_token exp.ls match_semicolon in 
            {value = SPRINT exp.value ; ls = List.tl exp.ls} 
          
          | VAR -> let id = consume_token xs match_identifier in 
                  let exp = parse_equality (List.tl xs) in 
                  let _ = consume_token exp.ls match_semicolon in
                  {value = SVAR {exp = exp.value ; iden = id.lexeme}; ls = List.tl exp.ls}

          | LEFTB -> parse_block xs parse_statement
          
          | IF -> let condition = parse_equality xs in 
                let success = parse_statement condition.ls in 
                if (not (List.is_empty success.ls) && (List.hd success.ls).t_type = ELSE) 
                  then let fail = parse_statement (List.tl success.ls)
                    in { value = CONDITIONAL {success = success.value; fail = fail.value; condition = condition.value}
                        ; ls = fail.ls }
                  else { value = CONDITIONAL {success = success.value; fail = SNONE; condition = condition.value} ; 
                        ls = success.ls }


          | _ -> let exp = parse_equality tokens in let _ = consume_token exp.ls match_semicolon in 
                {value = EXP exp.value; ls = List.tl exp.ls} 

let rec parse_statements tokens = 
  match tokens with 
  | [] -> []
  | ls -> let stmt = parse_statement ls in stmt.value :: parse_statements stmt.ls   

let parse tokens =  if List.is_empty tokens 
                    then [SNONE] 
                    else parse_statements tokens 