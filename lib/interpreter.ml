open Parser;;
type result = NUMBER of int | STRING of string | BOOL of bool |  VOID

module StringMap = Map.Make(String)

let env : result StringMap.t = StringMap.empty;;

let rec repeat_string s n =
  if n <= 0 then "" else s ^ repeat_string s (n-1)  

let rec evaluate_expression parser env = 
  match parser with 
  | Parser.NUMBER x -> NUMBER x
  | STRING s -> STRING s
  | BOOL b -> BOOL b
  | UNARY {operator = op; right = x} -> 
    (let right = evaluate_expression x env in 
      match op, right with 
      | "-", NUMBER n -> NUMBER (-1 * n)
      | _ -> raise (Failure "unsupported op for -"))
  
  | IDENTIFIER iden -> (match StringMap.find_opt iden env with 
    | None -> raise (Failure "unbound var")
    | Some x -> x )  
  | BINOP {left = x; right = y; operator = op} -> 
    let left = evaluate_expression x env in let right = evaluate_expression y env in  
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
        | "==", BOOL b1, BOOL b2 -> BOOL (b1 = b2)
        | "!=", BOOL b1, BOOL b2 -> BOOL (b1 <> b2)
        | "!=", NUMBER b1, NUMBER b2 -> BOOL (b1 <> b2)
        | _ -> raise (Failure "error")

let print_result res = 
  (match res with 
  | NUMBER num -> print_int num
  | STRING s -> print_string s
  | BOOL b -> if b then print_string "true" else print_string "false"
  | VOID -> print_string "" )
  ;print_newline ()

let isTruthy result =
  match result with 
  | NUMBER x -> if x = 0 then false else true
  | STRING s -> if (String.trim s = "") then false else true
  | BOOL b -> b
  | _ -> false

let rec evaluate parser env = 
  match parser with
  | [] -> [VOID]
  | x::xs -> match x with
            | SNONE -> VOID :: evaluate xs env
            | EXP exp -> let _ = evaluate_expression exp env in VOID :: evaluate xs env
            | SPRINT exp -> let _ = let e = evaluate_expression exp env in print_result e in VOID 
                        :: evaluate xs env
            | BLOCK  stmts -> let _ = evaluate stmts env in VOID :: evaluate xs env
            | SVAR {iden = id; exp = e} -> let new_env = let ex = evaluate_expression e env
                in StringMap.add id ex env in VOID :: evaluate xs new_env
            
            | CONDITIONAL {success = success; fail = fail; condition = condition} ->
              let cond = evaluate_expression condition env in 
              let _ = 
                if (isTruthy cond) 
                then evaluate [success] env
                else if (fail <> SNONE) then evaluate [fail] env 
                else []
                
                in VOID :: evaluate xs env