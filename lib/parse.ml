(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)
type token_type = IntLiteral
                | FirstOperator
                | SecondOperator
                | RelationalOperator
                | EqualityOperator
                | LeftParenthesis
                | RightParenthesis
                | LeftCurly
                | RightCurly
                | ReturnKeyword
                | IfKeyword
                | ElseKeyword
                | WhileKeyword
                | AssignmentSymbol
                | Identifier
                | Punctuator
                | Whitespace
                | EOF
                | Epsilon

type literal = int

type ident = string

type op = Add
        | Sub
        | Mul
        | Div

type rel = BiggerEqual
         | LessEqual
         | Bigger
         | Less

type equ = Equal
         | NotEqual

type expr = Variable of string
          | Constant of literal
          | Bop of expr * op * expr
          | Relation of expr * rel * expr
          | Equality of expr * equ * expr

type stmt = Assignment of ident * expr
          | IfStatement of expr * stmt list * stmt list
          | WhileStatement of expr * stmt list
          | ReturnStatement of expr
          | EndStatement

type program = stmt list

let token_type_key_value = [
    "[0-9]+", IntLiteral;
    "[ \n\t\r\x0b]+", Whitespace;
    ";", Punctuator;
    "if", IfKeyword;
    "while", WhileKeyword;
    "else", ElseKeyword;
    "return", ReturnKeyword;
    "=", AssignmentSymbol;
    "[a-zA-Z][a-zA-z0-9]*", Identifier;
    "[+-]", FirstOperator;
    "[*/%]", SecondOperator;
    "[<>]", RelationalOperator;
    "[!=][=]]", EqualityOperator;
    "(", LeftParenthesis;
    ")", RightParenthesis;
    "{", LeftCurly;
    "}", RightCurly;
]

let token_type_regexp =
  List.map (fun pair -> (Str.regexp (fst pair) , (snd pair))) token_type_key_value

let rec tokenize_text text_string =
  if text_string = "" then
    [("", EOF)]
  else
    let matched_regexp = List.find (fun regexp -> Str.string_match (fst regexp) text_string 0) token_type_regexp in
    let end_index = Str.match_end () in
    let token_string = String.sub text_string 0 end_index in
    let new_string = String.sub text_string end_index ((String.length text_string) - end_index) in
    if snd matched_regexp == Whitespace then
      tokenize_text new_string
    else
      let token = (token_string, snd matched_regexp) in
      token :: (tokenize_text new_string)

let parse_const token =
  if snd token == IntLiteral then Constant (int_of_string (fst token))
  else raise (Failure "Can't parse constant")

let parse_variable token =
  if snd token == Identifier then Variable (fst token)
  else raise (Failure "Can't parse variable")

let parse_match (ttype: token_type) token_list =
  match token_list with
    | [] -> raise (Failure "Can't match anything with empty list")
    | h :: t -> if snd h = ttype then t else raise (Failure ("Syntax error: match error on token " ^ (fst h)))

let get_rel str =
  match str with
  | ">" -> Bigger
  | "<" -> Less
  | ">=" -> BiggerEqual
  | "<=" -> LessEqual
  | _ -> raise (Failure "Can't parse relational operator")

let get_equ str =
  match str with
  | "==" -> Equal
  | "!=" -> NotEqual
  | _ -> raise (Failure "Can't parse equality operator")

let get_sign str =
  match str with
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | _ -> raise (Failure "Can't parse arithmetic operator")

(* i need to refactor this *)
let get_string_rel str =
  match str with
  | Bigger      -> ">"
  | Less        -> "<"
  | BiggerEqual -> ">="
  | LessEqual   -> "<="

let get_string_equ str =
  match str with
  |  Equal    -> "=="
  |  NotEqual -> "!="

let get_string_sign oper =
  match oper with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"


let rec parse_sum token_list =
  match token_list with
    | [] -> raise (Failure "Illegal state in parse_sum")
    | x :: [] -> (fst (parse_prod [x])), []
    | x ->
          let product, rem = parse_prod x in
          if rem = [] then (product, rem)
          else if snd (List.hd rem) = FirstOperator then  (
            let operator = get_sign (fst (List.hd rem)) in
            let expression, rem_expr = parse_sum (List.tl rem) in
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
          let expression, rem = parse_sum t in
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
            let loc_operator = get_sign (fst operator) in
            let loc_product, rem = parse_prod (List.tl t) in
            (Bop (constant, loc_operator, loc_product)) , rem)
          else
            (constant, t)
       | Identifier ->
          let variable = parse_variable h in
          let operator = List.hd t in
          if snd operator = SecondOperator then (
            let loc_operator = get_sign (fst operator) in
            let loc_product, rem = parse_prod (List.tl t) in
            (Bop (variable, loc_operator, loc_product)) , rem)
          else
            (variable, t)
       | _ ->
          failwith "unimpl case in parse_prod"

let rec parse_expr token_list =
  match token_list with
    | [] -> raise (Failure "Illegal state in parse_expr")
    | x :: [] -> (fst (parse_sum [x])), []
    | x ->
          let product, rem = parse_sum x in
          if rem = [] then (product, rem)
          else if snd (List.hd rem) = RelationalOperator then  (
            let operator = get_rel (fst (List.hd rem)) in
            let expression, rem_expr = parse_expr (List.tl rem) in
            (Relation (product, operator, expression), rem_expr))
          else if snd (List.hd rem) = EqualityOperator then  (
            let operator = get_equ (fst (List.hd rem)) in
            let expression, rem_expr = parse_expr (List.tl rem) in
            (Equality (product, operator, expression), rem_expr))
          else
            (product, rem)

let rec parse_stmt (token_list: (string * token_type) list) =
  let stmt, tail =
    match token_list with
    | [] -> raise (Failure "No statements ahead")
    | h :: tail ->
       match snd h with
       | ReturnKeyword ->
          let expression, tail = parse_expr tail in
          let tail = parse_match Punctuator tail in
          (ReturnStatement expression, tail)
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
       | EOF ->
          (EndStatement, [])
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
  | Relation (x, y, z) ->
     print_string "(";
     print_expr x;
     let sign = get_string_rel y in
     print_string sign;
     print_expr z;
     print_string ")"
  | Equality (x, y, z) ->
     print_string "(";
     print_expr x;
     let sign = get_string_equ y in
     print_string sign;
     print_expr z;
     print_string ")"
  | Bop (x, y, z) ->
     print_string "(";
     print_expr x;
     let sign = get_string_sign y in
     print_string sign;
     print_expr z;
     print_string ")"
  | Constant x -> print_string ("(" ^ string_of_int x ^ ")")
  | Variable x -> print_string ("(" ^ x ^ ")")

let rec print_stmt ast_list =
  List.iter (fun ast ->
      match ast with
      | ReturnStatement x ->
         print_string "return ";
         print_expr x;
         print_string " ; ";
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
      | EndStatement ->
         print_string "\n";
    ) ast_list

let print_ast ast_list =
  print_stmt ast_list;
