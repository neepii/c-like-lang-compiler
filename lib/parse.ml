(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)
type token_type = IntLiteral
                | FirstOperator
                | SecondOperator
                | RelationalOperator
                | EqualityOperator
                | LeftParenthesis
                | RightParenthesis
                | BeginKeyword
                | EndKeyword
                | ReturnKeyword
                | ExternKeyword
                | DoKeyword
                | IfKeyword
                | ThenKeyword
                | ElseKeyword
                | WhileKeyword
                | AssignmentSymbol
                | Identifier
                | Punctuator
                | Whitespace
                | EOF
                | Epsilon
                | Comma

type literal = int

type ident = string

type op = Add
        | Sub
        | Mul
        | Div
        | Rem

type bool_op = GreaterEqual
             | LessEqual
             | Greater
             | Less
             | Equal
             | NotEqual

type expr = Variable of ident
          | Constant of literal
          | Negation of expr
          | Function of ident * expr list
          | Bop of expr * op * expr
          | BoolBop of expr * bool_op * expr
          | Epsilon

type storage = External
             | NoneType

type stmt = Assignment of ident * expr
          | IfStatement of expr * stmt list * stmt list
          | WhileStatement of expr * stmt list 
          | FuncInit of ident * expr list * stmt list
          | ReturnStatement of expr
          | ExpressionStatement of expr
          | FuncDecl of storage * ident * expr list
          | NoneStatement
          | EndStatement

let token_type_key_value = [
    "[0-9]+", IntLiteral;
    "[ \n\t\r\x0b]+", Whitespace;
    ",", Comma;
    ";", Punctuator;
    "begin", BeginKeyword;
    "do", DoKeyword;
    "then", ThenKeyword;
    "end", EndKeyword;
    "if", IfKeyword;
    "while", WhileKeyword;
    "else", ElseKeyword;
    "return", ReturnKeyword;
    "extern", ExternKeyword;
    "[!=]=", EqualityOperator;
    ":=", AssignmentSymbol;
    "[a-zA-Z][a-zA-z0-9_]*", Identifier;
    "[+-]", FirstOperator;
    "[*/%]", SecondOperator;
    "[<>]", RelationalOperator;
    "(", LeftParenthesis;
    ")", RightParenthesis;
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
    | h :: t -> 
       if snd h = ttype then t else raise (Failure ("Syntax error: match error on token " ^ (fst h)))

let get_bool_op str =
  match str with
  | ">" -> Greater
  | "<" -> Less
  | ">=" -> GreaterEqual
  | "<=" -> LessEqual
  | "==" -> Equal
  | "!=" -> NotEqual
  | _ -> raise (Failure "Can't parse equality operator")

let get_sign str =
  match str with
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Rem
  | _ -> raise (Failure "Can't parse arithmetic operator")

let get_string_bool_op str =
  match str with
  | Greater      -> ">"
  | Less        -> "<"
  | GreaterEqual -> ">="
  | LessEqual   -> "<="
  | Equal    -> "=="
  | NotEqual -> "!="

let get_string_sign oper =
  match oper with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Rem -> "%"

let head_type (token_list: (ident * token_type) list ) =
  if token_list != [] then
    snd (List.hd token_list)
  else
    Epsilon

let rec parse_prod token_list =
  match token_list with
    | [] -> raise (Failure "Illegal state in parse_prod")
    | x :: [] ->
       (parse_const x, [])
    | h :: t ->
       match snd h with
       | FirstOperator ->
          if fst h = "-" then
            let product, tail = parse_prod t in
            (Negation product, tail)
          else
            parse_prod t
       | LeftParenthesis ->
          let expression, tail = parse_sum t in
          let tail_without_rightparenthesis = parse_match RightParenthesis tail in
          if tail_without_rightparenthesis = [] then (expression, tail_without_rightparenthesis)
          else if snd (List.hd tail_without_rightparenthesis) = SecondOperator then
            let operator = get_sign (fst (List.hd tail_without_rightparenthesis)) in
            let product, tail = parse_prod (List.tl tail_without_rightparenthesis) in
            (Bop (expression, operator, product), tail)
          else
            (expression, tail_without_rightparenthesis)
       | IntLiteral ->
          let constant = parse_const h in
          let operator = List.hd t in
          if snd operator = SecondOperator then (
            let loc_operator = get_sign (fst operator) in
            let loc_product, tail = parse_prod (List.tl t) in
            (Bop (constant, loc_operator, loc_product)) , tail)
          else
            (constant, t)
       | Identifier ->
          if t != [] && snd (List.hd t) = LeftParenthesis then
            let name = fst h in
            let args, tail = parse_args t in
            (Function (name, args), tail)
          else
            let variable = parse_variable h in
            let operator = List.hd t in
            if snd operator = SecondOperator then (
              let loc_operator = get_sign (fst operator) in
              let loc_product, tail = parse_prod (List.tl t) in
              (Bop (variable, loc_operator, loc_product)) , tail)
            else
              (variable, t)
       | _ ->
          (Epsilon, h :: t)

and parse_sum token_list =
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
  
and parse_expr token_list =
  match token_list with
    | [] -> raise (Failure "Illegal state in parse_expr")
    | x :: [] -> (fst (parse_sum [x])), []
    | x ->
       let sum, tail = parse_sum x in
         match head_type tail with 
         | RelationalOperator -> 
            let operator = get_bool_op (fst (List.hd tail)) in
            let expression, tail = parse_expr (List.tl tail) in
            (BoolBop (sum, operator, expression), tail)
         | EqualityOperator ->
            let operator = get_bool_op (fst (List.hd tail)) in
            let expression, tail = parse_expr (List.tl tail) in
            (BoolBop (sum, operator, expression), tail)
         | _ ->
            (sum, tail)

and parse_exprs token_list =
  match token_list with
  | [] -> raise (Failure "Illegal state in parse_cse")
  | x :: [] -> 
     [fst (parse_expr [x])], []
  | x ->
     let expr, tail = parse_expr x in
         match head_type tail with
         | Comma ->
            (expr :: fst (parse_exprs tail), tail)
         | _ -> ([expr], tail)

and parse_args token_list =
  let tail = parse_match LeftParenthesis token_list in
  if head_type tail = RightParenthesis then
    let tail = parse_match RightParenthesis tail in
    ([], tail)
  else
    let expression, tail = parse_exprs tail in
    let tail = parse_match RightParenthesis tail in
    (expression, tail)

let rec parse_stmt token_list = 
  let head = List.hd token_list in
  let tail = List.tl token_list in
  match snd head with
  | Identifier ->
     if snd (List.hd tail) = LeftParenthesis then
       let expr, tail = parse_args tail in
       let tail = parse_match Punctuator tail in
       (ExpressionStatement (Function (fst head, expr)), tail)
     else
       let tail = parse_match AssignmentSymbol tail in
       let expression, tail = parse_expr tail in
       let tail = parse_match Punctuator tail in
       (Assignment ((fst head), expression), tail)
  | ReturnKeyword ->
     let expression, tail = parse_expr tail in
     let tail = parse_match Punctuator tail in
     (ReturnStatement expression, tail)
  | WhileKeyword ->
     let expression, tail = parse_expr tail in
     let tail = parse_match DoKeyword tail in
     let tail = parse_match BeginKeyword tail in
     let stmts, tail = parse_stmts tail in
     let tail = parse_match EndKeyword tail in
     let tail = parse_match Punctuator tail in
     (WhileStatement (expression, stmts), tail)
  | IfKeyword ->
     let expression, tail = parse_expr tail in
     let tail = parse_match ThenKeyword tail in
     let tail = parse_match BeginKeyword tail in
     let first_stmts, tail = parse_stmts tail in
     let tail = parse_match EndKeyword tail in
     let tail = parse_match Punctuator tail in
     if tail = [] || snd (List.hd tail) != ElseKeyword then
       (IfStatement (expression, first_stmts, []), tail)
     else
       let tail = parse_match ElseKeyword tail in
       let tail = parse_match BeginKeyword tail in
       let second_stmts, tail = parse_stmts tail in
       let tail = parse_match EndKeyword tail in
       let tail = parse_match Punctuator tail in
       (IfStatement (expression, first_stmts, second_stmts), tail)
  | EOF ->
     (EndStatement, [])
  | _ -> failwith "unimplemented case in parse_stmt"

and parse_stmts token_list =
  let stmt, tail = parse_stmt token_list in
  if tail = [] || snd (List.hd tail) = EndKeyword then
    ([stmt], tail)
  else
    let stmts, tail = parse_stmts tail in
    (stmt :: stmts, tail)

let rec parse_outer_stmt token_list =
  let stmt, tail =
    match token_list with
    | [] -> raise (Failure "No statements ahead")
    | h :: t -> 
       match snd h with
       | ExternKeyword ->
          (match snd (List.hd t) with
          | Identifier ->
             let symbol = fst (List.hd t) in
             let tail = parse_match Identifier t in
             if head_type tail = LeftParenthesis then
               let expr, tail = parse_args tail in
               let tail = parse_match Punctuator tail in
               (FuncDecl (External, symbol, expr), tail)
             else 
               failwith "Unimplemented: external without params"
          | _ -> raise (Failure "Syntax error: no identifier after extern keyword"))
       | Identifier ->
          let expression, tail = parse_args t in
          let tail = parse_match BeginKeyword tail in
          let stmts, tail = parse_stmts tail in
          let tail = parse_match EndKeyword tail in
          let tail = parse_match Punctuator tail in
          (FuncInit  (fst h, expression, stmts), tail)
       | EOF ->
          (EndStatement, [])
       | _ -> raise (Failure "Illegal state in parse_outer_stmt")
  in
  if tail = [] || snd (List.hd tail) = EndKeyword then
    ([stmt], tail)
  else
    let stmts, tail = parse_outer_stmt tail in
    (stmt :: stmts, tail)  

let parse (token_list: (string * token_type) list) =
  let ast, tail = parse_outer_stmt token_list in
  if tail = [] then ast else raise (Failure "Parser didn't reached the end")

let rec string_of_expr ast =
  match ast with
  | BoolBop (x, y, z) ->
     "("
     ^ string_of_expr x
     ^ get_string_bool_op y
     ^ string_of_expr z
     ^ ")"
  | Bop (x, y, z) ->
     "("
     ^ string_of_expr x
     ^ get_string_sign y
     ^ string_of_expr z
     ^ ")"
  | Constant x -> "(" ^ string_of_int x ^ ")"
  | Function (x, y) -> 
     let string = String.concat ", " (List.map string_of_expr y) in
     x ^ string
  | Negation x ->
     "(-"
     ^ string_of_expr x
     ^  ")"
  | Variable x -> "(" ^ x ^ ")"
  | Epsilon -> ""

let string_of_expr_list expr_list =
  String.concat ", " (List.map string_of_expr expr_list)

let string_of_var var = 
  match var with
  | Variable var -> var
  | _ -> raise (Failure "Error in string_of_var")

let rec string_of_stmt ast_list =
  match ast_list with
  | [] -> ""
  | h :: t -> 
     let string = 
      match h with
      | FuncDecl (spec, name, expr) ->
         (match spec with
         | External -> "extern "
         | NoneType -> ""
         )
         ^ name
         ^ string_of_expr_list expr
         ^ " ; "
      | ReturnStatement x ->
         "return "
         ^ string_of_expr x
         ^ " ; "
      | Assignment (x, y) ->
         "(" ^ x ^ ") = "
         ^ string_of_expr y
         ^  " ; "
      | ExpressionStatement x ->
         string_of_expr x ^ " ;\n"
      | FuncInit (x, y, z) ->
         x
         ^ string_of_expr_list y 
         ^ "{"
         ^ string_of_stmt z
         ^ "}"
      | WhileStatement (x, y) ->
          "while ("
         ^ string_of_expr x
         ^ ") { "
         ^ string_of_stmt y
         ^ "}"
      | IfStatement (x, y, z) ->
         "if ("
         ^ string_of_expr x
         ^  ") { "
         ^ string_of_stmt y
         ^  "}"
         ^ if z != [] then (
            " else {"
              ^ string_of_stmt z
              ^ "}"
         ) else ""
      | EndStatement -> "\n"
      | NoneStatement -> ""
      (* | Frame x -> failwith "Unimplemented in string_of_stmt" *)
     in
     string ^ string_of_stmt t

let print_expr ast = 
  print_endline (string_of_expr ast)

let print_ast ast_list =
  let string = string_of_stmt ast_list in
  print_endline string;
