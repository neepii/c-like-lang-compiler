(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)

open Compiler.Token

let () =
  let ic = open_in Sys.argv.(1) in
  try
    let line = input_line ic in
    let tokens = tokenize_text line in
    List.iter print_token tokens
  with e ->
    close_in_noerr ic;
    raise e


