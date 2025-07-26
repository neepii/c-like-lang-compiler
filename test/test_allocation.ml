open Compiler.Gen
open Compiler.Parse

let%expect_test "compute_live_intervals 1" =
  label_counter := 0;
  extern_symbol_list := [];
  max_symb_addr_counter := 0;
  symb_addr_num_avail := 0;  
  Stdlib.Hashtbl.reset symbol_table;
  let ir_instructions =
    [
      move_instr (Const 1) (SymbAddr 1);
      move_instr (SymbAddr 1) (SymbAddr 2);
      move_instr (SymbAddr 2) (SymbAddr 3);
      move_instr (SymbAddr 3) (SymbAddr 4);
      syscall_instr 94 (SymbAddr 4);
    ]
  in
  let f i = (-1, -1, i) in
  let live_intervals = Stdlib.Array.init 5 f in
  compute_live_intervals 0 ir_instructions live_intervals;
  Stdlib.List.iteri (fun i el ->
      Stdlib.Printf.printf "%d: %s\n" i (string_of_tac el)
    ) ir_instructions;
  Stdlib.Array.iter (fun el ->
      let start_el, end_el, i_el = el in
      (Stdlib.Printf.printf "(%d, %d, %d)\n" start_el end_el i_el);
    ) live_intervals;

  [%expect {|
    0: t1 := 1
    1: t2 := t1
    2: t3 := t2
    3: t4 := t3
    4: syscall 94 with t4
    (-1, -1, 0)
    (0, 1, 1)
    (1, 2, 2)
    (2, 3, 3)
    (3, 4, 4)
    |}]

let%expect_test "compute_live_intervals 2" =
  label_counter := 0;
  extern_symbol_list := [];
  max_symb_addr_counter := 0;
  symb_addr_num_avail := 0;  
  Stdlib.Hashtbl.reset symbol_table;

  let text =
    "main () {acc=1; n=5;while(n>1){acc=acc*n; n=n-1;}}"
  in
  let tokens = tokenize_text text in
  let ast = parse tokens in
  let ir = create_frame ast in
  let f i = (-1, -1, i) in
  let live_intervals = Stdlib.Array.init 8 f in
  compute_live_intervals 0 ir live_intervals;
  Stdlib.List.iteri (fun i el ->
      Stdlib.Printf.printf "%d: %s\n" i (string_of_tac el)
    ) ir;
  Stdlib.Array.iter (fun el ->
      let start_el, end_el, i_el = el in
      (Stdlib.Printf.printf "(%d, %d, %d)\n" start_el end_el i_el);
    ) live_intervals;
  [%expect {|
    main{(acc) = (1) ; (n) = (5) ; while (((n)>(1))) { (acc) = ((acc)*(n)) ; (n) = ((n)-(1)) ; }}

    0: L.main
    1: t1 := 1
    2: t0 := t1
    3: t3 := 5
    4: t2 := t3
    5: L.L1
    6: t4 := 1
    7: branch to L2 if t2<=t4
    8: t5 := t0*t2
    9: t0 := t5
    10: t6 := 1
    11: t7 := t2-t6
    12: t2 := t7
    13: jump to L1
    14: L.L2
    (2, 9, 0)
    (1, 2, 1)
    (4, 12, 2)
    (3, 4, 3)
    (6, 7, 4)
    (8, 9, 5)
    (10, 11, 6)
    (11, 12, 7)
    |}]

  
let%expect_test "compute_live_intervals 3" =
  label_counter := 0;
  extern_symbol_list := [];
  max_symb_addr_counter := 0;
  symb_addr_num_avail := 0;  
  Stdlib.Hashtbl.reset symbol_table;
  let text = 
    "main () {b = 1; n=5; n = n + 1; return n; }"
  in
  let tokens = tokenize_text text in
  let ast = parse tokens in
  let ir = create_frame ast in
  let f i = (-1, -1, i) in
  let live = Stdlib.Array.init (!max_symb_addr_counter + 1) f in
  compute_live_intervals 0 ir live;
  Stdlib.Array.iter (fun el ->
      let start_el, end_el, i_el = el in
      (Stdlib.Printf.printf "(%d, %d, %d)\n" start_el end_el i_el);
    ) live;
  let offset_stack = Stdlib.Array.init !max_symb_addr_counter (fun _ -> -1) in
  let register = Stdlib.Array.init (!max_symb_addr_counter + 1) (fun _ -> (-1)) in
  register_allocation live offset_stack register;
  Stdlib.List.iteri (fun i el ->
      Stdlib.Printf.printf "%d: %s\n" i (string_of_tac el)
    ) ir;
  Stdlib.Array.iteri (fun i el ->
      Stdlib.Printf.printf "t%d = %d\n" i el
    ) register;
  [%expect {|
    main{(b) = (1) ; (n) = (5) ; (n) = ((n)+(1)) ; return (n) ; }

    (2, 2, 0)
    (1, 2, 1)
    (4, 8, 2)
    (3, 4, 3)
    (5, 6, 4)
    (6, 7, 5)
    0: L.main
    1: t1 := 1
    2: t0 := t1
    3: t3 := 5
    4: t2 := t3
    5: t4 := 1
    6: t5 := t2+t4
    7: t2 := t5
    8: syscall 94 with t2
    t0 = 1
    t1 = 0
    t2 = 0
    t3 = 1
    t4 = 1
    t5 = 2
    |}]

