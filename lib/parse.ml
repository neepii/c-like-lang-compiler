(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)
type token_type = IntLiteral
                | FirstOperator
                | SecondOperator
                | LeftParenthesis
                | RightParenthesis
                | Keyword
                | NoneToken

type num = int

type first_op = Add
              | Sub

type second_op = Mul
               | Div

type prod = Second_binary_op of prod * second_op * prod
          | Constant of num

type expr = First_binary_op of prod * first_op * expr
          | Prod of prod

let token_type_key_value = [
    "[0-9]+", IntLiteral;
    "[+-]", FirstOperator;
    "[*/]", SecondOperator
]

let regexp_string_match str pattern =
  Str.string_match (Str.regexp pattern) str 0

let get_token_type str =
  let regexp_match x = regexp_string_match str (fst x) in
  let ttype = snd (List.hd (List.filter regexp_match token_type_key_value)) in
  (str, ttype)

let tokenize_text text_string = 
  let lexeme_list = Str.split (Str.regexp "[ \n\r\x0c\t]+") text_string in
  List.map get_token_type lexeme_list 

let parse_const token = 
  if snd token == IntLiteral then Constant (int_of_string (fst token))
  else raise (Failure "Can't parse")

let get_first_sign str = 
  match str with
  | "+" -> Add
  | "-" -> Sub
  | _ -> raise (Failure "Can't parse operator")

let get_second_sign str = 
  match str with
  | "*" -> Mul
  | "/" -> Div
  | _ -> raise (Failure "Can't parse operator")
  
let rec parse_prod token_list = 
  match token_list with
    | [] -> raise (Failure "Illegal state in parse_prod")
    | x :: [] -> (parse_const x, [])
    | h :: t -> 
       let constant = parse_const h in
       let operator = List.hd t in
       if snd operator = SecondOperator then (
         let loc_product, rem = parse_prod (List.tl t) in
         let loc_operator = get_second_sign (fst operator) in
         (Second_binary_op (constant, loc_operator, loc_product)) , rem)
       else (constant, t)

let rec first_occurance_of (ttype: token_type) token_list =
  match token_list with
    | [] -> []
    | x :: [] -> if snd x = ttype then [x] else []
    | h :: t -> if snd h = ttype then h :: t else first_occurance_of ttype t

(* let rec parenthesis_head token_list (cur_head: (string * token_type) list) count = *)
(*   match token_list with *)
(*     | [] -> ([], cur_head) *)
(*     | x :: [] ->  *)
(*        if snd x = RightParenthesis && count = 1 then ([], cur_head) else raise (Failure "parenthesis_head list") *)
(*     | h :: t ->  *)
(*        if snd h = LeftParenthesis then (parenthesis_head t (count + 1), cur_head :: h) *)
(*        else if snd h = RightParenthesis then (parenthesis_head t (count - 1), cur_head :: h) *)
(*        else (parenthesis_head t count, cur_head :: h) *)

let rec parse_expr token_list =
  match token_list with 
    | [] -> raise (Failure "Illegal state in parse_expr")
    | x :: [] -> (Prod (fst (parse_prod [x])), [])
    | h :: t -> 
       (* match snd h with *)
       (* | LeftParenthesis ->  *)
       (* | RightParenthesis -> raise (Failure "Syntax error: closed parenthesis") *)
       (* | IntLiteral ->  *)
          let product, rem = parse_prod (h :: t) in
          if rem = [] then (Prod product, rem)
          else 
            let operator = get_first_sign (fst (List.hd rem)) in
            let expression, rem_expr = parse_expr (List.tl rem) in
            (First_binary_op (product, operator, expression), rem_expr)

let parse token_list =
  fst (parse_expr token_list)
       
let rec print_prod ast =
  match ast with
  | Second_binary_op (x, y, z) -> 
     print_string "(";
     print_prod x;
     let sign = if y == Mul then "*" else "/" in
     print_string sign;
     print_prod z;
     print_string ")"
  | Constant x -> print_string ("(" ^ string_of_int x ^ ")")

let rec print_expr ast =
  match ast with
  | First_binary_op (x, y, z) -> 
     print_string "(";
     print_prod x;
     let sign = if y == Add then "+" else "-" in
     print_string sign;
     print_expr z;
     print_string ")"
  | Prod x ->
     print_prod x

let print_ast ast =
  print_expr ast;
  print_endline ""
