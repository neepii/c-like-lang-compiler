(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)

open Compiler.Gen
open Compiler.Parse

let () =
  let ic = open_in Sys.argv.(1) in
  try
    let text = In_channel.input_all ic in
    let tokens = tokenize_text text in
    let ast = parse tokens in
    let dag = create_frame ast in
    let output = generate_code dag in
    let oc = open_out Sys.argv.(2) in
    output_string oc output;
    print_endline output;
    print_ast ast;
    close_in ic;
  with e ->
    close_in_noerr ic;
    raise e
