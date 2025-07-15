(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)
open Parse

type op_code = Move
             | Call
             | ArithInstr of op
             | BranchJump of bool_op * int
             | Jump of int
             | Label of int
             | None

type ir_arg = Register of int
            | Immediate of int
            | Symbol of string
            | EffectiveAddress of int * int

type ir_instr = op_code * ir_arg list

type tac_list = ir_instr list

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

let label_counter = ref 0

let label_instr counter =
  counter := (!counter) + 1;
  (Label !counter, [])

let get_label_num label =
  let label_num, _ = label in
  match label_num with
  | Label x -> x
  | _ -> raise (Failure "Can't create jump tac")

let jump_instr label =
  let num = get_label_num label in
  (Jump num, [])

let branch_instr (sign: bool_op) (operand1: ir_arg) (operand2: ir_arg) (label: ir_instr) =
  let num = get_label_num label in
  (BranchJump (sign, num), [operand1; operand2])

let arith_instr op (arg1: ir_arg) (arg2: ir_arg) (arg3: ir_arg) =
  (ArithInstr op, [arg1; arg2; arg3])

let call_instr =
  (Call, [])

let negate_bool_op bool_op =
  match bool_op with
  | GreaterEqual -> Less
  | LessEqual -> GreaterEqual
  | Greater -> LessEqual
  | Less -> GreaterEqual
  | Equal -> NotEqual
  | NotEqual -> Equal

let fold_bop arg1 arg2 op =
  match op with
  | Sub -> arg1 - arg2
  | Add -> arg1 + arg2
  | Mul -> arg1 * arg2
  | Div -> arg1 / arg2
  | Rem -> arg1 mod arg2 (*TODO: test if this is actually a remainder *)

let rec eval_expr expr (rlist: literal list) =
  match expr with
  | Constant x ->
     let new_reg = List.hd rlist in
     let rlist = List.tl rlist in
     let move = move_instr (Immediate x) (Register new_reg) in
     ([move], rlist, Register new_reg)
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
        let instruction = arith_instr y new_arg arg_x arg_z in
        let tac_list = List.concat [tac_x; tac_z; [instruction]] in
        (tac_list, rlist, new_arg)
  )
  | _ -> failwith "Unimplemented in create_instr_for_expr"

let rec create_tac_list ast rlist =
  match ast with
  | [] -> []
  | h :: t ->
    match h with
    | Assignment (x, y) ->
       if Hashtbl.find_opt symbol_table x = None then
         let reg = List.hd rlist in
         let rlist = List.tl rlist in
         let tac_list, rlist, arg = eval_expr y rlist in
         let move = move_instr arg (Register reg) in
         Hashtbl.add symbol_table x (Register reg, false);
         List.concat [tac_list ; [move] ; create_tac_list t rlist] (* list.concat is slow, make your concat specifically for tac's*)
       else
         let reg, _ = Hashtbl.find symbol_table x in
         let tac_list, rlist, arg = eval_expr y rlist in
         let move = move_instr arg reg in
         Hashtbl.add symbol_table x (reg, false);
         List.concat [tac_list ; [move] ; create_tac_list t rlist]
    | ReturnStatement x ->
       let tac_list, rlist, arg = eval_expr x rlist in
       let first_move = move_instr arg (Register 10) in
       let second_move = move_instr (Immediate 94) (Register 16) in
       List.concat [tac_list ;  [first_move] ; [second_move] ; [call_instr] ; create_tac_list t rlist]
    | WhileStatement (x, y)  -> (
      let start_label = label_instr label_counter in
      let end_label = label_instr label_counter in
       match x with
       | BoolBop (a, sign, b) ->
          let tac_list_1, rlist, expr1 = eval_expr a rlist in
          let tac_list_2, rlist, expr2 = eval_expr b rlist in
          let branch = branch_instr (negate_bool_op sign) expr1 expr2 end_label in
          let jump = jump_instr start_label in
          let body = create_tac_list y rlist in
          (* this create list will not return rlist, because of new frame, i need to create tests to make sure it actually works *)
          List.concat [[start_label] ; tac_list_1 ; tac_list_2 ; [branch] ; body ; [jump; end_label] ; create_tac_list t rlist]
       | _ -> failwith "Unimplemented: While statement without relation")
    | IfStatement (x, y, z) ->
       let skip_then_branch_label = label_instr label_counter in
       (match x with
         | BoolBop (a, sign, b) ->
          let tac_list_1, rlist, expr1 = eval_expr a rlist in
          let tac_list_2, rlist, expr2 = eval_expr b rlist in
          let branch = branch_instr (negate_bool_op sign) expr1 expr2 skip_then_branch_label in
          let then_body = create_tac_list y rlist in
          if z = [] then
            List.concat [tac_list_1 ; tac_list_2 ; [branch] ; then_body ; [skip_then_branch_label] ; create_tac_list t rlist]
          else
            let end_label = label_instr label_counter in
            let jump = jump_instr end_label in
            let else_body = create_tac_list z rlist in
            List.concat [tac_list_1 ; tac_list_2 ; [branch] ; then_body ; [jump] ; [skip_then_branch_label] ; else_body ; [end_label] ; create_tac_list t rlist]
         | _ -> failwith "Unimplemented: If statement without relation")
    | EndStatement -> []
    (* | _ -> raise (Failure "Can't create node for DAG") *)

let create_dag ast =
  let reg_list = [10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27] in
  create_tac_list ast reg_list

let string_of_label num =
  "L" ^ string_of_int num

let string_of_ir_arg arg =
  match arg with
  | Register x ->  riscv64_reg_list.(x)
  | Immediate x -> string_of_int x
  | Symbol x -> x
  | EffectiveAddress (x, y) -> string_of_int x ^ "(" ^ riscv64_reg_list.(y) ^ ")"

let string_of_instruction operator operand_list =
  match operand_list with
  | [] -> "  " ^ operator ^ "\n"
  | x :: [] -> "  " ^ operator ^ " " ^ x ^ "\n"
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
     | (Immediate _ , Immediate _) -> raise (Failure "Illegal state: sub reg, imm, imm")
     | _ -> failwith "Uimplemented in construct_add_operator"
  )
  | _ -> failwith "Unimpl add to nonregister"

let construct_sub_operator dest operand1 operand2 =
  let dst_string = string_of_ir_arg dest in
  let first = string_of_ir_arg operand1 in
  let second = string_of_ir_arg operand2 in
  match dest with
  | Register _ -> (
     match (operand1, operand2) with
     | (Register _ , Immediate _) -> string_of_instruction "addi" [dst_string;first; "-" ^ second]
     | (Immediate _, Register _) -> string_of_instruction "addi" [dst_string;second; "-" ^ first]
     | (Register _, Register _) -> string_of_instruction "sub" [dst_string;first;second]
     | (Symbol _, _) -> raise (Failure "Cant sub with symbol")
     | (Immediate _ , Immediate _) -> raise (Failure "Illegal state: sub reg, imm, imm")
     | _ -> failwith "Uimplemented in construct_add_operator"
  )
  | _ -> failwith "Unimpl add to nonregister"

let construct_generic_operator string dest operand1 operand2 =
  let dst_string = string_of_ir_arg dest in
  let first = string_of_ir_arg operand1 in
  let second = string_of_ir_arg operand2 in
  match dest with
  | Register _ -> (
     match (operand1, operand2) with
     | (Register _, Register _) -> string_of_instruction string [dst_string;first;second]
     | (Symbol _, _) -> raise (Failure "Cant sub with symbol")
     | (Immediate _ , Immediate _) -> raise (Failure "Illegal state: sub reg, imm, imm")
     | _ -> failwith "Uimplemented in construct_add_operator"
  )
  | _ -> failwith "Unimpl add to nonregister"

let construct_arith_operator op dest operand1 operand2 =
  match op with
  | Add -> construct_add_operator dest operand1 operand2
  | Sub -> construct_sub_operator dest operand1 operand2
  | Mul -> construct_generic_operator "mul" dest operand1 operand2
  | Div -> construct_generic_operator "div" dest operand1 operand2
  | Rem -> construct_generic_operator "rem" dest operand1 operand2

let construct_branchjump_operator bool_op operand1 operand2 label =
  let op_string =
    match bool_op with
    | GreaterEqual -> "bge"
    | LessEqual -> "ble"
    | Greater -> "bgt"
    | Less -> "blt"
    | Equal -> "beq"
    | NotEqual -> "bne"
  in
  let first = string_of_ir_arg operand1 in
  let second = string_of_ir_arg operand2 in
  string_of_instruction op_string [first;second;label]


let rec generate_code_rec tac =
  match tac with
  | [] -> ""
  | h :: t ->
     let op_code, args = h in
     let tac =
       match op_code with
       | Move ->
          assert(List.length args = 2);
          let source = List.nth args 0 in
          let destination = List.nth args 1 in
          construct_move_operator source destination
       | Call ->
          "  ecall\n"
       | ArithInstr op ->
          assert(List.length args = 3);
          let destination = List.nth args 0 in
          let first_arg = List.nth args 1 in
          let second_arg = List.nth args 2 in
          construct_arith_operator op destination first_arg second_arg
       | BranchJump (bool_op, num_label) ->
          assert(List.length args = 2);
          let operand1 = List.nth args 0 in
          let operand2 = List.nth args 1 in
          construct_branchjump_operator bool_op operand1 operand2 (string_of_label num_label)
       | Jump dest ->
          string_of_instruction "j" [string_of_label dest]
       | Label num ->
          string_of_label num ^ ":\n"
       | None -> ""
       (* | _ -> failwith "Unimplemented in generate_code_rec" *)
     in
     tac ^  generate_code_rec t

let generate_code tac_list =
  ".global _start\n_start:\n\n"
  ^
  let code = generate_code_rec tac_list in
  Hashtbl.reset symbol_table;
  code

