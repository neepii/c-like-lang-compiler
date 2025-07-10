(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)
type token_type = IntLiteral
                | FirstOperator
                | SecondOperator
                | LeftParenthesis
                | RightParenthesis
                | Keyword
                | NoneToken

type literal = int

type op = Add
        | Sub
        | Mul
        | Div

type expr = Constant of literal
          | Bop of expr * op * expr

let token_type_key_value = [
    "[0-9]+", IntLiteral;
    "[+-]", FirstOperator;
    "[*/]", SecondOperator;
    "(", LeftParenthesis;
    ")", RightParenthesis;
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

let parse_match (ttype: token_type) token_list =
  match token_list with
    | [] -> raise (Failure "Can't match anything with empty list")
    | h :: t -> if snd h = ttype then t else raise (Failure "Can't match")

(*this is quite strange, only one of two must exist*)
let get_sign str = 
  match str with
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | _ -> raise (Failure "Can't parse operator")

let get_string_sign oper = 
  match oper with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

let rec parse_expr token_list =
  match token_list with 
    | [] -> raise (Failure "Illegal state in parse_expr")
    | x :: [] -> (fst (parse_prod [x])), []
    | x -> 
          let product, rem = parse_prod x in
          if rem = [] then (product, rem)
          else if snd (List.hd rem) = FirstOperator then  (
            let operator = get_sign (fst (List.hd rem)) in
            let expression, rem_expr = parse_expr (List.tl rem) in
            (Bop (product, operator, expression), rem_expr))
          else
            (product, rem)

and parse_prod token_list = 
  match token_list with
    | [] -> raise (Failure "Illegal state in parse_prod")
    | x :: [] -> 
       (parse_const x, [])
    | h :: t -> 
       match snd h with
         (* refactor me *)
       | LeftParenthesis ->
          let expression, rem = parse_expr t in
          let rem_without_rightparenthesis = parse_match RightParenthesis rem in
          if rem_without_rightparenthesis = [] then (expression, rem_without_rightparenthesis)
          else if snd (List.hd rem_without_rightparenthesis) = SecondOperator then 
            let operator = get_sign (fst (List.hd rem_without_rightparenthesis)) in
            let product, rem = parse_prod (List.tl rem_without_rightparenthesis) in
            (Bop (expression, operator, product), rem)
          else
            (expression, rem_without_rightparenthesis)
       | IntLiteral ->
          let constant = parse_const h in
          let operator = List.hd t in
          if snd operator = SecondOperator then (
            let loc_product, rem = parse_prod (List.tl t) in
            let loc_operator = get_sign (fst operator) in
            (Bop (constant, loc_operator, loc_product)) , rem)
          else 
            (constant, t)
       | _ -> 
          failwith "unimpl"

let parse token_list =
  fst (parse_expr token_list)
       
let rec print_expr ast =
  match ast with
  | Bop (x, y, z) -> 
     print_string "(";
     print_expr x;
     let sign = get_string_sign y in
     print_string sign;
     print_expr z;
     print_string ")"
  | Constant x -> print_string ("(" ^ string_of_int x ^ ")")

let print_ast ast =
  print_expr ast;
  print_endline ""
