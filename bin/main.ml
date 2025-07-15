(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)

open Compiler.Gen
open Compiler.Parse

let () =
  let ic = open_in Sys.argv.(1) in
  try
    let text = In_channel.input_all ic in
    let tokens = tokenize_text text in
    let ast = parse tokens in
    let dag = create_dag ast in
    let output = generate_code dag in
    let oc = open_out "/tmp/X0101011.s" in
    output_string oc output;
    let _ = Unix.open_process_in "riscv64-linux-gnu-as -march=rv64gc /tmp/X0101011.s -o /tmp/main_temp.o && riscv64-linux-gnu-ld /tmp/main_temp.o" in
    print_endline output;
    print_ast ast;
    close_in ic;
  with e ->
    close_in_noerr ic;
    raise e
