(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)
open Parse

type ir_arg = SymbAddr of int
            | Immediate of int
            | Symbol of string
            | EffectiveAddress of int * ir_arg

type gen_arg = Register of int
             | Immediate of int
             | Symbol of string
             | EffectiveAddress of int * gen_arg


type ir_instr = Move of ir_arg * ir_arg
              | ArithInstr of op * ir_arg * ir_arg * ir_arg
              | Neg of ir_arg * ir_arg
              | BranchJump of bool_op * int * ir_arg * ir_arg
              | Syscall of int * ir_arg
              | Jump of int
              | Label of int
              | None

(* maximum is 18 on riscv64 *)
let num_of_registers_avail = 0

let label_counter = ref 0

let max_symb_addr_counter = ref 0

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



let move_instr (source: ir_arg) (destination: ir_arg) =
  Move (source, destination)

let neg_instr (source: ir_arg) (destination: ir_arg) =
  Neg (source, destination)

let check_max_symb (curr_value: int) (counter: int ref) =
  counter := if curr_value > !counter then curr_value else !counter

let label_instr counter =
  counter := (!counter) + 1;
  Label !counter

let get_label_num label =
  match label with
  | Label num -> num
  | _ -> raise (Failure "Illegal state in get_label_num")

let jump_instr label =
  let num = get_label_num label in
  Jump num

let branch_instr (sign: bool_op) (operand1: ir_arg) (operand2: ir_arg) (label: ir_instr) =
  let num = get_label_num label in
  BranchJump (sign, num, operand1,operand2)

let arith_instr op (arg1: ir_arg) (arg2: ir_arg) (arg3: ir_arg) =
  ArithInstr (op, arg1, arg2, arg3)

let none_instr = None

let syscall_instr call_number first_arg= 
  Syscall (call_number, first_arg)

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

let give_symb_addr avail_sym_num =
  check_max_symb avail_sym_num max_symb_addr_counter;
  (SymbAddr avail_sym_num , avail_sym_num + 1)

let rec eval_expr expr sym_num =
  match expr with
  | Constant x ->
     let symb_addr, sym_num = give_symb_addr sym_num in
     let move = move_instr (Immediate x) symb_addr in
     ([move], sym_num, symb_addr)
  | Variable x ->
     let symb_addr = Hashtbl.find symbol_table x in
     (match symb_addr with
     | SymbAddr _-> ([] , sym_num, symb_addr)
     | _ -> failwith "unimpl in evaluation of variable")
  | Bop (x, y, z) -> (
     let tac_x, sym_num, symb_x = eval_expr x sym_num in
     let tac_z, sym_num, symb_z = eval_expr z sym_num in
     (*rewrite please*)
     (* match (arg_x, arg_z) with *)
     (* | (Immediate x, Immediate z) -> ([], rlist, Immediate (fold_bop x z y)) *)
     (* | _ -> *)
     let symb_addr, sym_num = give_symb_addr sym_num in
     let instruction = arith_instr y symb_addr symb_x symb_z in
     let tac_list = List.concat [tac_x; tac_z; [instruction]] in
     (tac_list, sym_num, symb_addr)
  )
  | Negation x -> (
     match x with
     | Constant x -> 
        let symb_addr, sym_num = give_symb_addr sym_num in
        let move = move_instr (Immediate (-x)) symb_addr in
        ([move], sym_num, symb_addr)
     | _ -> (
        let tac, sym_num, arg = eval_expr x sym_num in
        let symb_addr, sym_num = give_symb_addr sym_num in
        match symb_addr with
        | SymbAddr _ ->
           let negation = neg_instr arg symb_addr in
           let tac_list = List.concat [tac; [negation]] in
           (tac_list, sym_num, symb_addr)
        | _-> failwith "Unimplemented negation for nonregister"
  ))
  | _ -> failwith "Unimplemented in create_instr_for_expr"



let rec create_tac_list ast sym_num =
  match ast with
  | [] -> []
  | h :: t ->
    match h with
    | Assignment (x, y) ->
       if Hashtbl.find_opt symbol_table x = None then
         let symb_addr, sym_num = give_symb_addr sym_num in
         let tac_list, sym_num, arg = eval_expr y sym_num in
         let move = move_instr arg symb_addr in
         Hashtbl.add symbol_table x symb_addr;
         List.concat [tac_list ; [move] ; create_tac_list t sym_num] (* list.concat is slow, make your concat specifically for tac's*)
       else
         let reg = Hashtbl.find symbol_table x in
         let tac_list, sym_num, arg = eval_expr y sym_num in
         let move = move_instr arg reg in
         Hashtbl.add symbol_table x reg;
         List.concat [tac_list ; [move] ; create_tac_list t sym_num]
    | ReturnStatement x ->
       let tac_list, sym_num, arg = eval_expr x sym_num in
       let syscall = syscall_instr 94 arg in
       List.concat [tac_list ;  [syscall] ; create_tac_list t sym_num]
    | WhileStatement (x, y)  -> (
      let start_label = label_instr label_counter in
      let end_label = label_instr label_counter in
       match x with
       | BoolBop (a, sign, b) ->
          let tac_list_1, sym_num, expr1 = eval_expr a sym_num in
          let tac_list_2, sym_num, expr2 = eval_expr b sym_num in
          let branch = branch_instr (negate_bool_op sign) expr1 expr2 end_label in
          let jump = jump_instr start_label in
          let body = create_tac_list y sym_num in
          (* this create list will not return sym_num, because of new frame, i need to create tests to make sure it actually works *)
          List.concat [[start_label] ; tac_list_1 ; tac_list_2 ; [branch] ; body ; [jump; end_label] ; create_tac_list t sym_num]
       | _ -> failwith "Unimplemented: While statement without relation")
    | IfStatement (x, y, z) ->
       let skip_then_branch_label = label_instr label_counter in
       (match x with
         | BoolBop (a, sign, b) ->
          let tac_list_1, sym_num, expr1 = eval_expr a sym_num in
          let tac_list_2, sym_num, expr2 = eval_expr b sym_num in
          let branch = branch_instr (negate_bool_op sign) expr1 expr2 skip_then_branch_label in
          let then_body = create_tac_list y sym_num in
          if z = [] then
            List.concat [tac_list_1 ; tac_list_2 ; [branch] ; then_body ; [skip_then_branch_label] ; create_tac_list t sym_num]
          else
            let end_label = label_instr label_counter in
            let jump = jump_instr end_label in
            let else_body = create_tac_list z sym_num in
            List.concat [tac_list_1 ; tac_list_2 ; [branch] ; then_body ; [jump] ; [skip_then_branch_label] ; else_body ; [end_label] ; create_tac_list t sym_num]
         | _ -> failwith "Unimplemented: If statement without relation")
    | EndStatement -> []
    (* | _ -> raise (Failure "Can't create node for DAG") *)

let create_frame ast =
  create_tac_list ast 0

let string_of_label num =
  "L" ^ string_of_int num

let get_asm_operand arg =
  match arg with
  | Register x -> riscv64_reg_list.(x)
  | Immediate x -> string_of_int x
  | Symbol x -> x
  | EffectiveAddress (x, y) ->
     let addr =
     (match y with 
     | Register n -> riscv64_reg_list.(n)
     | _ -> failwith "Unimplemented in get_asm_operand")
     in
     string_of_int x ^ "(" ^ addr  ^ ")"

let string_of_instruction operator operand_list =
  match operand_list with
  | [] -> "  " ^ operator ^ "\n"
  | x :: [] -> "  " ^ operator ^ " " ^ x ^ "\n"
  | h :: t ->
     let list = List.map (fun str -> ", " ^ str) t in
     let tail_string = List.fold_left ( ^ ) "" list in
     "  " ^ operator ^ " " ^ h ^ tail_string  ^ "\n"

let construct_three_arg_operator dst op1 op2 make_instr =
  let dst_string = get_asm_operand dst in
  let first = get_asm_operand op1 in
  let second = get_asm_operand op2 in
  match dst with
  | Register _ -> make_instr dst_string first second
  | EffectiveAddress _ ->
    make_instr dst_string first second
    ^ string_of_instruction "sd" ["t0"; dst_string]
  | _ -> failwith "Unimpl add to nonregister"


let construct_move_operator operand1 operand2 =
  let first = get_asm_operand operand1 in
  let second = get_asm_operand operand2 in
  match (operand1, operand2) with
  | (Register _, Register _) -> string_of_instruction "add" [second; first; "zero"]
  | (Symbol _, _ ) ->  string_of_instruction "la" [second; first]
  | (Register _, EffectiveAddress _) -> string_of_instruction "sd" [second;first]
  | (Immediate _, Register _) -> string_of_instruction "li" [second;first]
  | (Immediate _, EffectiveAddress _) -> string_of_instruction "li" ["t0"; first] ^ string_of_instruction "sd" ["t0"; second]

  | (EffectiveAddress _, Register _) -> string_of_instruction "ld" [second;first]
  | (EffectiveAddress _, Symbol _) -> failwith "Unimplemented in eff_addr -> symb"
  | (EffectiveAddress _, Immediate _) -> raise (Failure "trying to move effective address -> imm")
  | (EffectiveAddress _, EffectiveAddress _) ->
        string_of_instruction "ld" ["t2"; first]
        ^ string_of_instruction "sd" ["t2"; second]

  | (Register _, Immediate _) -> raise (Failure "trying to move imm -> imm")
  | (Register _, Symbol _) -> failwith "Unimpl reg -> sym"
  | (Immediate _, Symbol _) -> failwith "Unimpl imm -> sym"
  | (Immediate _, Immediate _) -> raise (Failure "trying to move imm -> imm")

let construct_add_operator dest operand1 operand2 =
  let make_instr x first second = 
    match (operand1, operand2) with
     | (Register _ , Immediate _) -> string_of_instruction "addi" [x;first;second]
     | (Immediate _, Register _) -> string_of_instruction "addi" [x;second;first]
     | (Register _, Register _) -> string_of_instruction "add" [x;first;second]
     | (Immediate num1 , Immediate num2) -> string_of_instruction "li" [x; string_of_int (num1 + num2)]

     | (EffectiveAddress _, Register _) -> 
        string_of_instruction "ld" ["t1"; first] 
        ^ string_of_instruction "add" [x;second;first]
     | (Register _, EffectiveAddress _) ->
        string_of_instruction "ld" ["t1"; second] 
        ^ string_of_instruction "add" [x;first;second]
     | (EffectiveAddress _, EffectiveAddress _) ->
        string_of_instruction "ld" ["t1"; first]  
        ^ string_of_instruction "ld" ["t2"; second] 
        ^ string_of_instruction "add" ["t0";"t1"; "t2"]
        ^ string_of_instruction "sd" ["t0"; x] 

     | (Symbol _, _) -> raise (Failure "Cant add with symbol")
     | _ -> failwith "Uimplemented in construct_add_operator"
  in
  construct_three_arg_operator dest operand1 operand2 make_instr

let construct_sub_operator dest operand1 operand2 =
  let make_instr x first second =          
    match (operand1, operand2) with
    | (Register _ , Immediate _) -> string_of_instruction "addi" [x;first; "-" ^ second]
    | (Immediate _, Register _) -> string_of_instruction "addi" [x;second; "-" ^ first]
    | (Register _, Register _) -> string_of_instruction "sub" [x;first;second]
    | (Immediate num1, Immediate num2) -> string_of_instruction "li" [x; string_of_int (num1 - num2)]

    | (EffectiveAddress _, Register _) -> 
       string_of_instruction "ld" ["t1"; first] 
       ^ string_of_instruction "sub" [x;second;first]
    | (Register _, EffectiveAddress _) ->
       string_of_instruction "ld" ["t1"; second] 
       ^ string_of_instruction "sub" [x;first;second]
    | (EffectiveAddress _, EffectiveAddress _) -> 
       string_of_instruction "ld" ["t1"; first] 
       ^ string_of_instruction "ld" ["t2"; second] 
       ^ string_of_instruction "sub" ["t0";"t1"; "t2"]
       ^ string_of_instruction "sd" ["t0"; x] 

    | (Symbol _, _) -> raise (Failure "Cant sub with symbol")

    | _ -> failwith "Uimplemented in construct_sub_operator"
  in
  construct_three_arg_operator dest operand1 operand2 make_instr

let construct_generic_operator string dest operand1 operand2 =
  let make_instr x first second = 
    match (operand1, operand2) with
    | (Register _, Register _) -> string_of_instruction string [x;first;second]
    | (Register _ , Immediate _) -> string_of_instruction string [x;first;second]
    | (Immediate _, Register _) -> string_of_instruction string [x;second;first]
    | (Immediate _ , Immediate _) -> raise (Failure "Illegal state in construct_generic_operator")

    | (EffectiveAddress _, Register _) -> 
       string_of_instruction "ld" ["t1"; first] 
       ^ string_of_instruction string [x;second;first]
    | (Register _, EffectiveAddress _) ->
       string_of_instruction "ld" ["t1"; second] 
       ^ string_of_instruction string [x;first;second]
    | (EffectiveAddress _, EffectiveAddress _) -> 
       string_of_instruction "ld" ["t1"; first] 
       ^ string_of_instruction "ld" ["t2"; second] 
       ^ string_of_instruction string ["t0";"t1"; "t2"]
       ^ string_of_instruction "sd" ["t0"; x] 

    | (Symbol _, _) -> failwith "Uimplemented in construct_generic_operator: Symbol"
    | _ -> failwith "Uimplemented in construct_generic_operator"
  in
  construct_three_arg_operator dest operand1 operand2 make_instr

let construct_arith_operator op dest operand1 operand2 =
  match op with
  | Add -> construct_add_operator dest operand1 operand2
  | Sub -> construct_sub_operator dest operand1 operand2
  | Mul -> construct_generic_operator "mul" dest operand1 operand2
  | Div -> construct_generic_operator "div" dest operand1 operand2
  | Rem -> construct_generic_operator "rem" dest operand1 operand2

let construct_neg_operator operand1 operand2 =
  let source = get_asm_operand operand1 in
  let destination = get_asm_operand operand2 in
  match (operand1, operand2) with 
    | (Register _, Register _) -> string_of_instruction "neg" [destination; source]
    | (Register _ , Immediate _) -> raise (Failure "trying to negate imm")
    | (Immediate _, Register _) -> 
       string_of_instruction "li" ["t0"; source]
       ^ string_of_instruction "neg" [destination; "t0"]
    | (Immediate _ , Immediate _) -> raise (Failure "Illegal state in construct_generic_operator")

    | (EffectiveAddress _, Register _) -> 
       string_of_instruction "ld" ["t0"; source] 
       ^ string_of_instruction "neg" [destination; "t0"]
    | (Register _, EffectiveAddress _) ->
       string_of_instruction "neg" ["t0"; source]
       ^ string_of_instruction "sd" ["t0"; destination]
    | (EffectiveAddress _, EffectiveAddress _) -> 
       string_of_instruction "ld" ["t0"; source] 
       ^ string_of_instruction "neg" ["t1"; "t0"]
       ^ string_of_instruction "sd" ["t1"; destination]

    | (Symbol _, _) -> failwith "Uimplemented in construct_generic_operator: Symbol"
    | _ -> failwith "Uimplemented in construct_generic_operator"    
  

let construct_branchjump_operator bool_op operand1 operand2 label =
  let label = string_of_label label in
  let op_string =
    match bool_op with
    | GreaterEqual -> "bge"
    | LessEqual -> "ble"
    | Greater -> "bgt"
    | Less -> "blt"
    | Equal -> "beq"
    | NotEqual -> "bne"
  in
  let first = get_asm_operand operand1 in
  let second = get_asm_operand operand2 in
  match (operand1, operand2) with
    | (Register _, Register _) -> string_of_instruction op_string [first;second;label]
    | (Register _ , Immediate _) -> 
       string_of_instruction "li" ["t0";second]
       ^ string_of_instruction op_string [first;"t0"]
    | (Immediate _, Register _) -> 
       string_of_instruction "li" ["t0";first]
       ^ string_of_instruction op_string ["t0"; second]
    | (Immediate _ , Immediate _) -> 
       string_of_instruction "li" ["t0";first]
       ^ string_of_instruction "li" ["t1";second]
       ^ string_of_instruction op_string ["t0"; "t1"]

    | (EffectiveAddress _, Register _) -> 
       string_of_instruction "ld" ["t0"; first]
       ^ string_of_instruction op_string ["t0"; second; label]
    | (Register _, EffectiveAddress _) ->
       string_of_instruction "ld" ["t0"; second] 
       ^ string_of_instruction op_string [first; "t0"; label]
    | (EffectiveAddress _, EffectiveAddress _) -> 
       string_of_instruction "ld" ["t1"; first] 
       ^ string_of_instruction "ld" ["t2"; second] 
       ^ string_of_instruction op_string ["t1"; "t2"; label]

    | (Symbol _, _) -> failwith "Uimplemented in construct_generic_operator: Symbol"
    | _ -> failwith "Uimplemented in construct_generic_operator"

let rec ir_to_gen_arg ir_arg =
  match ir_arg with
  | SymbAddr x -> 
     if x < num_of_registers_avail then 
       Register (x + 10) 
     else 
       let offset = 8 *(x - num_of_registers_avail) in
       EffectiveAddress (offset, Register 2)
  | Immediate x -> Immediate x
  | Symbol x -> Symbol x
  | EffectiveAddress (x,y) -> EffectiveAddress (x, ir_to_gen_arg y)

let rec generate_code_rec tac =
  match tac with
  | [] -> ""
  | h :: t ->
     let tac =
       match h with
       | Move (source, destination) ->
          let arg1 = ir_to_gen_arg source in
          let arg2 = ir_to_gen_arg destination in
          construct_move_operator arg1 arg2
       | ArithInstr (op, ir_destination, ir_operand1, ir_operand2) ->
          let gen_operand1 = ir_to_gen_arg ir_operand1 in
          let gen_operand2 = ir_to_gen_arg ir_operand2 in
          let gen_destination = ir_to_gen_arg ir_destination in
          construct_arith_operator op gen_destination gen_operand1 gen_operand2
       | BranchJump (bool_op, num_label, ir_operand1, ir_operand2) ->
          let gen_operand1 = ir_to_gen_arg ir_operand1 in
          let gen_operand2 = ir_to_gen_arg ir_operand2 in
          construct_branchjump_operator bool_op gen_operand1 gen_operand2 num_label
       | Syscall (num, arg1) ->
          let gen_arg = ir_to_gen_arg arg1 in
          construct_move_operator gen_arg (Register 10)
          ^ construct_move_operator (Immediate num) (Register 16)
          ^ "  ecall\n"
       | Jump dest ->
          string_of_instruction "j" [string_of_label dest]
       | Neg (source, destination) ->
          let arg1 = ir_to_gen_arg source in
          let arg2 = ir_to_gen_arg destination in
          construct_neg_operator arg1 arg2
       | Label num ->
          string_of_label num ^ ":\n"
       | None -> ""
       (* | _ -> failwith "Unimplemented in generate_code_rec" *)
     in
     tac ^  generate_code_rec t

let generate_code_frame tac_list =
  let additional_space = !max_symb_addr_counter - num_of_registers_avail in
  if additional_space > 0 then
    string_of_instruction "addi" ["sp"; "sp"; string_of_int (-8 * additional_space)]
  else "";
  ^ generate_code_rec tac_list

let generate_code tac_list =
  ".global _start\n_start:\n\n"
  ^
  let code = generate_code_frame tac_list in
  Hashtbl.reset symbol_table;
  code
