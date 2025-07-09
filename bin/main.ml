(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)

open Compiler.Parse

let () =
  let ic = open_in Sys.argv.(1) in
  try
    let line = input_line ic in
    let tokens = tokenize_text line in
    let ast = parse_expr tokens in
    print_ast ast;
  with e ->
    close_in_noerr ic;
    raise e
