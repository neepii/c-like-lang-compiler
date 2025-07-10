(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)
type token_type = IntLiteral
                | FirstOperator
                | SecondOperator
                | LeftParenthesis
                | RightParenthesis
                | LeftCurly
                | RightCurly
                | Keyword
                | IfKeyword
                | ElseKeyword
                | WhileKeyword
                | AssignmentSymbol
                | Identifier
                | Punctuator
                | Epsilon

type literal = int

type ident = string

type op = Add
        | Sub
        | Mul
        | Div

type expr = Constant of literal
          | Bop of expr * op * expr

type stmt = Assignment of ident * expr
          | IfStatement of expr * stmt list * stmt list
          | WhileStatement of expr * stmt list

type program = stmt list

let token_type_key_value = [
    "[0-9]+", IntLiteral;
    ";", Punctuator;
    "if", IfKeyword;
    "while", WhileKeyword;
    "else", ElseKeyword;
    "=", AssignmentSymbol;
    "[a-zA-Z][a-zA-z0-9]*", Identifier;
    "[+-]", FirstOperator;
    "[*/]", SecondOperator;
    "(", LeftParenthesis;
    ")", RightParenthesis;
    "{", LeftCurly;
    "}", RightCurly;
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


let rec parse_stmt (token_list: (string * token_type) list) = 
  let stmt, tail =
    match token_list with
    | [] -> raise (Failure "No statements ahead")
    | h :: tail ->
       match snd h with
       | Identifier -> 
          let tail = parse_match AssignmentSymbol tail in
          let expression, tail = parse_expr tail in
          let tail = parse_match Punctuator tail in
          (Assignment ((fst h), expression), tail)
       | WhileKeyword ->
          let tail = parse_match LeftParenthesis tail in
          let expression, tail = parse_expr tail in
          let tail = parse_match RightParenthesis tail in
          let tail = parse_match LeftCurly tail in
          let stmts, tail = parse_stmt tail in
          let tail = parse_match RightCurly tail in
          (WhileStatement (expression, stmts), tail)
       | IfKeyword ->
          let tail = parse_match LeftParenthesis tail in
          let expression, tail = parse_expr tail in
          let tail = parse_match RightParenthesis tail in
          let tail = parse_match LeftCurly tail in
          let first_stmts, tail = parse_stmt tail in
          let tail = parse_match RightCurly tail in
          if tail = [] || snd (List.hd tail) != ElseKeyword then 
            (IfStatement (expression, first_stmts, []), tail)
          else
            let tail = parse_match ElseKeyword tail in
            let tail = parse_match LeftCurly tail in
            let second_stmts, tail = parse_stmt tail in
            let tail = parse_match RightCurly tail in
            (IfStatement (expression, first_stmts, second_stmts), tail)
       | _ -> failwith "unimplemented case in parse_stmt" 
  in
  if tail = [] || snd (List.hd tail) = RightCurly then
    ([stmt], tail)
  else 
    let stmts, tail = parse_stmt tail in
    (stmt :: stmts, tail)


let parse (token_list: (string * token_type) list) =
  let ast, tail = parse_stmt token_list in
  if tail = [] then ast else raise (Failure "Parser didn't reached the end")

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

let rec print_stmt ast_list =
  List.iter (fun ast ->
      match ast with 
      | Assignment (x, y) ->
         print_string ("(" ^ x ^ ") = ");
         print_expr y;
         print_string " ; ";
      | WhileStatement (x, y) ->
         print_string "while (";
         print_expr x;
         print_string ") { ";
         print_stmt y;
         print_string "}";
      | IfStatement (x, y, z) ->
         print_string "if (";
         print_expr x;
         print_string ") { ";
         print_stmt y;
         print_string "}";
         if z != [] then (
           print_string " else {";
           print_stmt z;
           print_string "}";
         )
    ) ast_list

let print_ast ast_list =
  print_stmt ast_list;
  print_endline ""
