(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)
open Parse

type op_code = Move (* move source, destination*)
             | Call
             | Add
             | None

type ir_arg = Register of int
            | Immediate of int
            | Symbol of string
            | EffectiveAddress of int * int

type ir_instr = op_code * ir_arg list

type dagnode = ir_instr list

type symbol = {
    num: int;
    is_spilled: bool
}

let riscv64_reg_list = [|
    "zero";
    "ra";
    "sp";
    "gp";
    "tp";
    "t0";
    "t1";
    "t2";
    "fp"; (*or is "s0"?*)
    "s1";
    "a0";
    "a1";
    "a2";
    "a3";
    "a5";
    "a6";
    "a7";
    "s2";
    "s3";
    "s4";
    "s5";
    "s6";
    "s7";
    "s7";
    "s8";
    "s9";
    "s10";
    "s11";
    "t3";
    "t4";
    "t5";
    "t6";
|]

let symbol_table = Hashtbl.create 16

let move_instr arg1 arg2  =
  (Move, arg1 :: [arg2])

let call_instr =
  (Call, [])

let create_instr_for_expr expr rlist =
  match expr with
  | Constant x ->
     let new_reg = List.hd rlist in
     let rlist = List.tl rlist in
     let instruction = move_instr (Immediate x) (Register new_reg) in
     (instruction, rlist, new_reg)
  | Variable x ->
     let reg, _ = Hashtbl.find symbol_table x in
     ((None, []) , rlist, reg)
  | _ -> failwith "Unimplemented in create_instr_for_expr"

let rec create_dagnode ast rlist =
  match ast with
  | [] -> []
  | h :: t ->
    match h with
    | Assignment (x, y) ->
       let tac_list, rlist, reg = create_instr_for_expr y rlist in
       Hashtbl.add symbol_table x (reg, false);
       tac_list :: create_dagnode t rlist
    | ReturnStatement x ->
       let tac_list, rlist, reg = create_instr_for_expr x rlist in
       let first_move = move_instr (Register reg) (Register 10) in
       let second_move = move_instr (Immediate 94) (Register 16) in
       tac_list ::  first_move :: second_move :: call_instr :: create_dagnode t rlist
    | EndStatement -> []
    | _ -> raise (Failure "Can't create node for DAG")

let create_dag ast =
  let reg_list = [10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27] in
  create_dagnode ast reg_list


let string_of_ir_arg arg =
  match arg with
  | Register x ->  riscv64_reg_list.(x)
  | Immediate x -> string_of_int x
  | Symbol x -> x
  | EffectiveAddress (x, y) -> string_of_int x ^ "(" ^ riscv64_reg_list.(y) ^ ")"

let construct_move_operator operand1 operand2 =
  let first = string_of_ir_arg operand1 in
  let second = string_of_ir_arg operand2 in
  if first = second then "" else
    match operand1 with
    | Symbol _ ->   "  la " ^ second ^ ", " ^ first ^ "\n"
    | Register _ ->  (
       match operand2 with
       | Register _ -> "  add " ^ second ^ ", " ^ first ^ ", zero\n"
       | Symbol _ -> failwith "Unimpl reg -> sym"
       | EffectiveAddress _ -> "  sd " ^ second ^ ", " ^ first ^ "\n"
       | Immediate _ -> raise (Failure "trying to move imm -> imm")
    )
    | EffectiveAddress _ -> "  sd " ^ second ^ ", " ^ first ^ "\n"
    | Immediate _ -> (
       match operand2 with
       | Register _ -> "  li " ^ second ^ ", " ^ first ^ "\n"
       | Symbol _ -> failwith "Unimpl imm -> sym"
       | EffectiveAddress _ -> "  li t0, " ^ first ^ "\n" ^ "  sd t0, " ^ second ^ "\n"
       | Immediate _ -> raise (Failure "trying to move imm -> imm")
    )

let rec generate_code_rec tac =
  match tac with
  | [] -> ""
  | h :: t ->
     let op_code, args = h in
     match op_code with
     | Move ->
        assert(List.length args = 2);
        let source = List.nth args 0 in
        let destination = List.nth args 1 in
        construct_move_operator source destination ^ generate_code_rec t
     | Call ->
        "  ecall\n" ^ generate_code_rec t
     | None -> generate_code_rec t
     | _ -> failwith "Unimplemented in generate_code_rec"

let generate_code dagnode =
  ".section .data\n.text\n.global _start\n_start:\n"
  ^
  generate_code_rec dagnode
