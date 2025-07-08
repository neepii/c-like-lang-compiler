(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)

type token_type = IntLiteral
                | FirstOperator
                | SecondOperator
                | Keyword
                | NoneToken

type token = {
    text: string;
    ttype: token_type
}

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

let print_token token =
  Printf.printf "%s\n" (fst token)
