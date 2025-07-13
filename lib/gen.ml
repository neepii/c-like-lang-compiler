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

let move_instr (arg1: ir_arg) (arg2: ir_arg) =
  (Move, [arg1; arg2])

let add_instr (arg1: ir_arg) (arg2: ir_arg) (arg3: ir_arg) =
  (Add, [arg1;arg2;arg3])

let call_instr =
  (Call, [])

let fold_bop arg1 arg2 op =
  match op with
  | Sub -> arg1 - arg2
  | Add -> arg1 + arg2
  | Mul -> arg1 * arg2
  | Div -> arg1 / arg2

let rec eval_expr expr (rlist: literal list) =
  match expr with
  | Constant x ->
     ([], rlist, Immediate x)
  | Variable x ->
     let reg, _ = Hashtbl.find symbol_table x in
     ([(None, [])] , rlist, reg)
  | Bop (x, y, z) -> (
     let tac_x, rlist, arg_x = eval_expr x rlist in
     let tac_z, rlist, arg_z = eval_expr z rlist in
     match (arg_x, arg_z) with
     | (Immediate x, Immediate z) -> ([], rlist, Immediate (fold_bop x z y))
     | _ -> 
     let new_arg, rlist = (Register (List.hd rlist), List.tl rlist) in
     let instruction =
       match y with
       | Add -> add_instr new_arg arg_x arg_z
       | Sub -> failwith "Unimplemented binary operation in create_instr_for expr"
       | Div -> failwith "Unimplemented binary operation in create_instr_for expr"
       | Mul -> failwith "Unimplemented binary operation in create_instr_for expr"
     in
     let tac_list = List.concat [tac_x; tac_z; [instruction]] in
     (tac_list, rlist, new_arg)
  )
  | _ -> failwith "Unimplemented in create_instr_for_expr"

let rec create_dagnode ast rlist =
  match ast with
  | [] -> []
  | h :: t ->
    match h with
    | Assignment (x, y) ->
       let tac_list, rlist, arg = eval_expr y rlist in
       Hashtbl.add symbol_table x (arg, false);
       List.concat [tac_list ; create_dagnode t rlist]
    | ReturnStatement x ->
       let tac_list, rlist, arg = eval_expr x rlist in
       let first_move = move_instr arg (Register 10) in
       let second_move = move_instr (Immediate 94) (Register 16) in
       List.concat [tac_list ;  [first_move] ; [second_move] ; [call_instr] ; create_dagnode t rlist]
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

let string_of_instruction operator operand_list =
  match operand_list with
  | [] -> operator ^ "\n"
  | x :: [] -> operator ^ " " ^ x ^ "\n"
  | h :: t -> 
     let list = List.map (fun str -> ", " ^ str) t in 
     let tail_string = List.fold_left ( ^ ) "" list in
     "  " ^ operator ^ " " ^ h ^ tail_string  ^ "\n"

let construct_move_operator operand1 operand2 =
  let first = string_of_ir_arg operand1 in
  let second = string_of_ir_arg operand2 in
  match (operand1, operand2) with
  | (Symbol _, _ ) ->  string_of_instruction "la" [second; first]
  | (Register _, Register _) -> string_of_instruction "add" [second; first; "zero"]
  | (Register _, EffectiveAddress _) -> string_of_instruction "sd" [second;first]
  | (EffectiveAddress _,  _) -> string_of_instruction "sd" [second;first]
  | (Immediate _, Register _) -> string_of_instruction "li" [second;first]
  | (Immediate _, EffectiveAddress _) -> string_of_instruction "li" ["t0"; first] ^ string_of_instruction "sd" ["t0"; second]

  | (Register _, Immediate _) -> raise (Failure "trying to move imm -> imm")
  | (Register _, Symbol _) -> failwith "Unimpl reg -> sym"
  | (Immediate _, Symbol _) -> failwith "Unimpl imm -> sym"
  | (Immediate _, Immediate _) -> raise (Failure "trying to move imm -> imm")

let construct_add_operator dest operand1 operand2 =
  let dst_string = string_of_ir_arg dest in
  let first = string_of_ir_arg operand1 in
  let second = string_of_ir_arg operand2 in
  match dest with
  | Register _ -> (
     match (operand1, operand2) with
     | (Register _ , Immediate _) -> string_of_instruction "addi" [dst_string;first;second]
     | (Immediate _, Register _) -> string_of_instruction "addi" [dst_string;second;first]
     | (Register _, Register _) -> string_of_instruction "add" [dst_string;first;second]
     | (Symbol _, _) -> raise (Failure "Cant add with symbol")
     | (Immediate _ , Immediate _) -> failwith "Unimplemented arith eval for add dst, imm, imm"
     | _ -> failwith "Uimplemented in construct_add_operator"
  )
  | _ -> failwith "Unimpl add to nonregister"

let rec generate_code_rec tac =
  match tac with
  | [] -> ""
  | h :: t ->
     let op_code, args = h in
     match op_code with
     | Add ->
        assert(List.length args = 3);
        let destination = List.nth args 0 in
        let first_arg = List.nth args 1 in
        let second_arg = List.nth args 2 in
        construct_add_operator destination first_arg second_arg ^ generate_code_rec t
     | Move ->
        assert(List.length args = 2);
        let source = List.nth args 0 in
        let destination = List.nth args 1 in
        construct_move_operator source destination ^ generate_code_rec t
     | Call ->
        "  ecall\n" ^ generate_code_rec t
     | None -> generate_code_rec t
     (* | _ -> failwith "Unimplemented in generate_code_rec" *)

let generate_code dagnode =
  ".section .data\n.text\n.global _start\n_start:\n"
  ^
  generate_code_rec dagnode
