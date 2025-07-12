(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)

open Compiler.Parse
open Compiler.Gen

let () =
  let ic = open_in Sys.argv.(1) in
  try
    let line = In_channel.input_all ic in
    let tokens = tokenize_text line in
    let ast = parse tokens in
    let dag = create_dag ast in
    let output = generate_code dag in
    let oc = open_out "/tmp/X0101011.s" in
    output_string oc output;
    let _ = Unix.open_process_in "riscv64-linux-gnu-as -march=rv64gc /tmp/X0101011.s -o /tmp/temp.o && riscv64-linux-gnu-ld /tmp/temp.o" in
    print_endline output;
    print_ast ast;
    close_in ic;
  with e ->
    close_in_noerr ic;
    raise e
