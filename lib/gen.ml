(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)
open Parse

type ir_arg = SymbAddr of int
            | Const of int
            | Symbol of string
            | EffectiveAddress of int * ir_arg
            | None

type gen_arg = Register of int
             | Immediate of int
             | Symbol of string
             | EffectiveAddress of int * gen_arg
             | None

type ir_instr = Move of ir_arg * ir_arg
              | ArithInstr of op * ir_arg * ir_arg * ir_arg
              | Neg of ir_arg * ir_arg
              | BranchJump of bool_op * string * ir_arg * ir_arg
              | Syscall of int * ir_arg
              | CallFunction of ident * ir_arg * ir_arg list
              | Return of ir_arg
              | Jump of string
              | Label of string
              | None

type symbol_type = Int64
                 | Function of storage * symbol_type list

type symbol_entry = {
    ir: ir_arg;
    (* gen: gen_arg; *)
    stype: symbol_type;
}


(* maximum is 11 on riscv64, excluding argument registers *)
let num_of_registers_avail = 11
let label_counter = ref 0
let max_symb_addr_counter = ref 0
let extern_symbol_list = ref []
let symb_addr_num_avail = ref 0

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

let func_instr name destination args = CallFunction (name, destination, args)
let move_instr source destination = Move (source, destination)
let neg_instr source destination = Neg (source, destination)
let jump_instr label = Jump label
let branch_instr sign operand1 operand2 label = BranchJump (sign, label, operand1,operand2)
let arith_instr op arg1 arg2 arg3 = ArithInstr (op, arg1, arg2, arg3)
let none_instr = None
let syscall_instr call_number first_arg = Syscall (call_number, first_arg)
let return_instr arg = Return arg

let check_max_symb (curr_value: int) (counter: int ref) =
  counter := if curr_value > !counter then curr_value else !counter

let create_num_label counter =
  counter := (!counter) + 1;
  ("L" ^ string_of_int !counter)

let add_to_ref_ident_list el ref =
  let list = !ref in
  ref := el :: list

let get_label_num label =
  match label with
  | Label num -> num
  | _ -> raise (Failure "Illegal state in get_label_num")

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
  | Rem -> arg1 mod arg2

let give_symb_addr avail_sym_num =
  let num = !symb_addr_num_avail in
  check_max_symb num max_symb_addr_counter;
  symb_addr_num_avail := num + 1;
  (SymbAddr num , avail_sym_num + 1)

let rec eval_expr_rec expr sym_num =
  match expr with
  | Constant x ->
     let symb_addr, sym_num = give_symb_addr sym_num in
     let move = move_instr (Const x) symb_addr in
     ([move], sym_num, symb_addr)
  | Variable x ->
     let symb_addr, _ = Hashtbl.find symbol_table x in

     (match symb_addr with
     | SymbAddr _ -> 
        ([] , sym_num, symb_addr)
     | _ -> failwith "unimpl in evaluation of variable")
  | Bop (x, y, z) -> (
    let tac_x, sym_num, symb_x = eval_expr_rec x sym_num in
    let tac_z, sym_num, symb_z = eval_expr_rec z sym_num in
    (* match (symb_x, symb_z) with *)
    (* | (Const x, Const z) -> ([], sym_num, Const (fold_bop x z y)) *)
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
       let move = move_instr (Const (-x)) symb_addr in
       ([move], sym_num, symb_addr)
    | _ -> (
      let tac, sym_num, arg = eval_expr_rec x sym_num in
      let symb_addr, sym_num = give_symb_addr sym_num in
      match symb_addr with
      | SymbAddr _ ->
         let negation = neg_instr arg symb_addr in
         let tac_list = List.concat [tac; [negation]] in
         (tac_list, sym_num, symb_addr)
      | _-> failwith "Unimplemented negation for nonregister"
  ))
  | _ -> failwith "Unimplemented in create_instr_for_expr"

let eval_expr expr sym_num =
  let tac, _, ir = eval_expr_rec expr sym_num in
  (tac, ir)

let rec allocate_params params sym_num =
  match params with
  | [] -> []
  | h :: t -> 
     match h with
     | Variable name ->
        let symb_addr, sym_num = give_symb_addr sym_num in
        Hashtbl.add symbol_table name (symb_addr, Int64);
        symb_addr :: allocate_params t sym_num
     | _ -> raise (Failure "Non variable in parameter function initialization")

let rec create_tac_list ast sym_num =
  match ast with
  | [] -> []
  | h :: t ->
    match h with
    | Assignment (x, y) ->
       if Hashtbl.find_opt symbol_table x = None then
         let symb_addr, sym_num = give_symb_addr sym_num in
         let tac_list, arg = eval_expr y sym_num in
         let move = move_instr arg symb_addr in
         Hashtbl.add symbol_table x (symb_addr, Int64);
         List.concat [tac_list ; [move] ; create_tac_list t sym_num] 
         (* list.concat is slow, make your concat specifically for tac's*)
       else
         let symb_addr, stype = Hashtbl.find symbol_table x in
         let tac_list, arg = eval_expr y sym_num in
         let move = move_instr arg symb_addr in
         Hashtbl.add symbol_table x (symb_addr, stype);
         List.concat [tac_list ; [move] ; create_tac_list t sym_num]
    | ReturnStatement x ->
       let tac_list, arg = eval_expr x sym_num in
       let return = return_instr arg in
       List.concat [tac_list ;  [return] ; create_tac_list t sym_num]
    | ExpressionStatement x ->
       (match x with
       | Function (name, args) ->
          let _, stype = Hashtbl.find symbol_table name in
          (match stype with
          | Function (_, params) ->
             if List.length params > List.length args then
               raise (Failure ("Error: too few arguments in " ^ name))
             else if List.length params < List.length args then
               raise (Failure ("Error: too many arguments in " ^ name))
             else if args != [] then
               let pair_list = List.map (fun x -> eval_expr x sym_num) args in
               let tac_list = List.concat (List.map fst pair_list) in
               let list = List.map snd pair_list in
               let ir_instr = func_instr name None list in
               List.concat [tac_list ;  [ir_instr] ; create_tac_list t sym_num]
             else
               let ir_instr = func_instr name None [] in
               ir_instr :: create_tac_list t sym_num
          | _ -> raise (Failure "Illegal state in create_tac_list: FunctionUsage"))
       | _ -> failwith "Unimplemented in ExpressionStatement")
    | WhileStatement (x, y)  -> (
      let start_label = create_num_label label_counter in
      let end_label = create_num_label label_counter in
       match x with
       | BoolBop (a, sign, b) ->
          let tac_list_1, expr1 = eval_expr a sym_num in
          let tac_list_2, expr2 = eval_expr b sym_num in
          let branch = branch_instr (negate_bool_op sign) expr1 expr2 end_label in
          let jump = jump_instr start_label in
          let body = create_tac_list y sym_num in
          List.concat [[Label start_label] ; tac_list_1 ; tac_list_2 ;
                       [branch] ; body ; [jump; Label end_label] ; create_tac_list t sym_num]
       | _ -> failwith "Unimplemented: While statement without relation")
    | FuncDecl (spec, symb, cse) ->
       (match spec with
       | External ->
          add_to_ref_ident_list symb extern_symbol_list;
          let type_list = List.map (fun _ -> Int64) cse in
          Hashtbl.add symbol_table symb (Symbol symb, Function (External, type_list))
       | NoneType -> raise (Failure "Illegal state in create_tac_list: FuncDecl"));
       create_tac_list t sym_num
    | IfStatement (x, y, z) ->
       let skip_then_branch_label = create_num_label label_counter in
       (match x with
         | BoolBop (a, sign, b) ->
          let tac_list_1, expr1 = eval_expr a sym_num in
          let tac_list_2, expr2 = eval_expr b sym_num in
          let branch = branch_instr (negate_bool_op sign) expr1 expr2 skip_then_branch_label in
          let then_body = create_tac_list y sym_num in
          if z = [] then
            List.concat [tac_list_1 ; tac_list_2 ; [branch] ;
                         then_body ; [Label skip_then_branch_label] ; create_tac_list t sym_num]
          else
            let end_label = create_num_label label_counter in
            let jump = jump_instr end_label in
            let else_body = create_tac_list z sym_num in
            List.concat [tac_list_1 ; tac_list_2 ; [branch] ; then_body ; 
                         [jump] ; [Label skip_then_branch_label] ; else_body ; 
                         [Label end_label] ; create_tac_list t sym_num]
         | _ -> failwith "Unimplemented: If statement without relation")
    | FuncInit (name, params, stmts) ->
       (* My programs can't have two variable with similar name in different scopes *)
       (* My current calling convention is terrible, must rewrite register saving before function call *)
       (* A function call can overwrite register values, caller does not save them *)
       (* I need to implement function init without any register allocation *)
       assert(List.length params <= 8);
       let type_list = List.map (fun _ -> Int64) params in
       List.iteri (fun i prm -> Hashtbl.add symbol_table (string_of_var prm) (SymbAddr i, Int64)) params;
       Hashtbl.add symbol_table name (Symbol name, Function (NoneType, type_list));
       let subprog =  Label name :: create_tac_list stmts sym_num in
       List.concat [subprog ; create_tac_list t sym_num]
    | EndStatement -> []
    | _ -> raise (Failure "Can't create node for DAG")

let create_frame ast =
  print_ast ast;
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
  | _ -> raise (Failure "None in get_asm_operand")

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
  if first = second then "" else
    match (operand1, operand2) with
    | (Register _, Register _) -> string_of_instruction "add" [second; first; "zero"]
    | (Symbol _, _ ) ->  string_of_instruction "la" [second; first]
    | (Register _, EffectiveAddress _) -> string_of_instruction "sd" [first;second]
    | (Immediate _, Register _) -> string_of_instruction "li" [second;first]
    | (Immediate _, EffectiveAddress _) -> 
       string_of_instruction "li" ["t0"; first] ^ string_of_instruction "sd" ["t0"; second]

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
    | _ -> raise (Failure "constructing None")

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



let rec ir_to_gen_arg ir_arg reg location =
  let regs_index = [| 9;17;18;19;20;21;22;23;24;25;26;27 |] in
  match ir_arg with
  | SymbAddr i -> 
     if location.(i) = -1 then
       let num = reg.(i) in
       Register regs_index.(num)
     else
       EffectiveAddress (8 * (location.(i) + 1), (Register 2))
  | Const x -> Immediate x
  | Symbol x -> Symbol x
  | EffectiveAddress (x,y) -> EffectiveAddress (x, ir_to_gen_arg y reg location)
  | None -> None

let construct_function name args reg location =
  assert(List.length args <= 7);
  let args = List.map (fun ir -> ir_to_gen_arg ir reg location) args in
  let length = List.length args in
  let allocate = if length = 0 then "" else string_of_instruction "addi" ["sp"; "sp"; string_of_int (-8 * length)] in
  let push = List.mapi (fun index reg -> construct_move_operator reg (EffectiveAddress (index, (Register 2)))) args in
  let move = List.mapi (fun index reg -> construct_move_operator reg (Register (index + 10))) args in
  let jump = string_of_instruction "call" [name] in
  let pop = List.mapi (fun index reg -> construct_move_operator (EffectiveAddress (index, (Register 2))) reg) args in
  String.concat "" (List.concat [[allocate] ; push ; move; [jump] ; pop])

let ident_is_func ident =
  let opt = Hashtbl.find_opt symbol_table ident in
  match opt with
  | Some x ->
     (match snd x with
     | Function (_, _) -> true
     | _ -> false)
  | None -> false

let rec generate_code_rec tac reg location =
  match tac with
  | [] -> ""
  | h :: t ->
     let tac =
       match h with
       | Move (source, destination) ->
          let arg1 = ir_to_gen_arg source reg location in
          let arg2 = ir_to_gen_arg destination reg location in
          construct_move_operator arg1 arg2
       | ArithInstr (op, ir_destination, ir_operand1, ir_operand2) ->
          let gen_operand1 = ir_to_gen_arg ir_operand1 reg location in
          let gen_operand2 = ir_to_gen_arg ir_operand2 reg location in
          let gen_destination = ir_to_gen_arg ir_destination reg location in
          construct_arith_operator op gen_destination gen_operand1 gen_operand2
       | BranchJump (bool_op, num_label, ir_operand1, ir_operand2) ->
          let gen_operand1 = ir_to_gen_arg ir_operand1 reg location in
          let gen_operand2 = ir_to_gen_arg ir_operand2 reg location in
          construct_branchjump_operator bool_op gen_operand1 gen_operand2 num_label
       | Syscall (num, arg1) ->
          let gen_arg = ir_to_gen_arg arg1 reg location in
          construct_move_operator gen_arg (Register 10)
          ^ construct_move_operator (Immediate num) (Register 16)
          ^ "  ecall\n"
       | Jump dest ->
          string_of_instruction "j" [dest]
       | Neg (source, destination) ->
          let arg1 = ir_to_gen_arg source reg location in
          let arg2 = ir_to_gen_arg destination reg location in
          construct_neg_operator arg1 arg2
       | Label ident ->
          ident ^ ":\n"
          ^ if ident_is_func ident then
              construct_arith_operator Sub (Register 2) (Register 2) (Immediate (8))
              ^ construct_move_operator (Register 1) (EffectiveAddress (0, Register 2))
            else ""
       | CallFunction (name, _, args) ->
          construct_function name args reg location
       | Return ir_arg ->
          let gen_arg = ir_to_gen_arg ir_arg reg location in
          construct_move_operator gen_arg (Register 10)
          ^ construct_move_operator (EffectiveAddress (0, Register 2)) (Register 1)
          ^ "  ret\n"
       | None -> ""
       (* | _ -> failwith "Unimplemented in generate_code_rec" *)
     in
     tac ^  generate_code_rec t reg location



let update_interval time ir live_intervals = 
  match ir with
  | SymbAddr num -> 
     let start_p, end_p, index = live_intervals.(num) in
     assert(end_p <= time);
     if start_p = -1 then
       live_intervals.(num) <- (time, time, index)
     else 
       live_intervals.(num) <- (start_p, time, index)
  | _ -> ()

let rec compute_live_intervals time tac_list live_intervals =
  match tac_list with
  | [] -> ()
  | h :: t -> 
     (match h with
     | Move (ir1, ir2) ->
        update_interval time ir1 live_intervals;
        update_interval time ir2 live_intervals;
     | ArithInstr (_, ir1, ir2, ir3) ->
        update_interval time ir1 live_intervals;
        update_interval time ir2 live_intervals;
        update_interval time ir3 live_intervals;
     | Neg (ir1, ir2) ->
        update_interval time ir1 live_intervals;
        update_interval time ir2 live_intervals;
     | BranchJump (_, _, ir1, ir2) ->
        update_interval time ir1 live_intervals;
        update_interval time ir2 live_intervals;
     | Syscall (_, ir1) ->
        update_interval time ir1 live_intervals;
     | CallFunction (_, ir1, ir_list) ->      
        update_interval time ir1 live_intervals;
        List.iter (fun ir -> update_interval time ir live_intervals) ir_list;
     | Return ir ->
        update_interval time ir live_intervals;
     | Jump _ -> ()
     | Label _ -> ()
     | None -> ()
     );
     compute_live_intervals (time + 1) t live_intervals

let spill_at_interval interval active register offset_stack last_offset =
  let rev_active = List.rev active in
  let _, end_i, i = interval in
  let _, end_j, j = List.hd rev_active in
  let active = 
    if end_j > end_i then 
      let reg_i = register.(i) in
      register.(j) <- reg_i;
      offset_stack.(j) <- last_offset;
      interval :: List.rev (List.tl rev_active) 
    else
      (* offset_stack.(i) <- last_offset; *)
      active
  in
  (active, last_offset + 1)

let expire_old_intervals interval register reg_avail active =
  match active with
  | [] -> ([], reg_avail)
  | h :: t -> 
     let _, end_j, j = h in
     let start_i, _, _ = interval in
     if end_j >= start_i then
       (active, reg_avail)
     else
       let freed_reg = register.(j) in
       let reg_avail = freed_reg :: reg_avail in
       (t, reg_avail)

let rec register_allocation_rec live_interval register reg_avail active offset last_offset  =
  match live_interval with
  | [] -> ()
  | h :: t -> 
     let _, _, i = h in
     let active =
       List.sort (fun a b -> 
           let _, a',  _ = a in
           let _, b',  _ = b in
           a' - b'
         ) active
     in
     let active, reg_avail = expire_old_intervals h register reg_avail active in
     if List.length active = num_of_registers_avail then
       let active, last_offset = spill_at_interval h active register offset last_offset in
       register_allocation_rec t register reg_avail active offset last_offset
     else 
       let new_reg = List.hd reg_avail in
       let reg_avail = List.tl reg_avail in
       register.(i) <- new_reg;
       let active = h :: active in
       register_allocation_rec t register reg_avail active offset last_offset


(**
   A linear scan register allocator
   @param live_interval an array of intervals of symbol addresses in which they are used in the program
   @param offset an array of offsets for stack-allocated variables
   @param register an array where index (symbol address number) corresponds to a specific register or not
 *)
let register_allocation live_interval offset register =
  let reg_avail = List.init num_of_registers_avail (fun i -> i) in
  let active = [] in
  Array.sort (fun a b -> 
      let a', _,  _ = a in
      let b', _,  _ = b in
      a' - b'
    ) live_interval;
  register_allocation_rec (Array.to_list live_interval) register reg_avail active offset 0


let rec string_of_ir ir =
  match ir with
  | SymbAddr num -> "t" ^ string_of_int num
  | Const num -> string_of_int num
  | Symbol str -> "L." ^ str
  | EffectiveAddress (offset,ir)  -> string_of_int offset ^ "(" ^ string_of_ir ir ^ ")"
  | None -> ""


let string_of_tac tac = 
  match tac with
     | Move (ir1, ir2) ->
     string_of_ir ir2 ^ " := " ^ string_of_ir ir1
     | ArithInstr (op, ir1, ir2, ir3) -> 
        string_of_ir ir1 ^ " := " ^ string_of_ir ir2 ^ get_string_sign op ^ string_of_ir ir3
     | Neg (ir1, ir2) ->
        string_of_ir ir2 ^ " := - (" ^ string_of_ir ir1 ^ ")"
     | BranchJump (bool_op, label, ir1, ir2) ->
        "branch to " ^ label ^ " if " ^ string_of_ir ir1 ^ get_string_bool_op bool_op ^ string_of_ir ir2
     | Syscall (num, ir1) ->
        "syscall " ^ string_of_int num ^ " with " ^ string_of_ir ir1
     | CallFunction (name, ir1, ir_list) ->      
        let arg_list = List.map string_of_ir ir_list in
        let arg_str = String.concat ", " arg_list in
        string_of_ir ir1 ^ " := " ^ name ^ "(" ^ arg_str ^ ")"
     | Jump label ->
        "jump to " ^ label
     | Label name ->
        "L." ^ name
     | Return name ->
        "return " ^ string_of_ir name
     | None -> ""


let generate_code_frame tac_list reg location =
  let offset_size = 
    Array.fold_left (fun x el -> if el != -1 then x + 1 else x ) 0 location
  in
  if offset_size > 0 then
    string_of_instruction "addi" ["sp"; "sp"; string_of_int (-8 * offset_size)]
  else ""
  ^ generate_code_rec tac_list reg location


let generate_code tac_list =
  let live_intervals = Array.init (!max_symb_addr_counter + 1) (fun i -> (-1, -1, i)) in
  compute_live_intervals 0 tac_list live_intervals;
  let offset_stack = Array.init (!max_symb_addr_counter + 1) (fun _ -> -1) in
  let register = Array.init (!max_symb_addr_counter + 1) (fun _ -> (-1)) in
  register_allocation live_intervals offset_stack register;
  let string = 
  String.concat "" (List.map (fun x -> ".extern " ^ x ^ "\n") !extern_symbol_list) 
  ^ ".global _start\n"
  ^ generate_code_frame tac_list register offset_stack
  ^ "_start:\n  call main\n  li a7, 94\n  ecall\n"
  in 
  label_counter := 0;
  extern_symbol_list := [];
  max_symb_addr_counter := 0;
  symb_addr_num_avail := 0;
  Hashtbl.reset symbol_table;
  string
