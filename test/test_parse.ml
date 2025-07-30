open Compiler.Parse

let%expect_test "basic ast 1" =
  let text = "1 + 2 + 3 * 4" in
  let ast = parse_expr (tokenize_text text) in
  print_expr (fst ast);
  [%expect {| ((1)+((2)+((3)*(4)))) |}]

let%expect_test "basic ast with big num" =
  let text = "1 + 2123213 + 3 * 4" in
  let ast = parse_expr (tokenize_text text) in
  print_expr (fst ast);
  [%expect {| ((1)+((2123213)+((3)*(4)))) |}]

let%expect_test "long basic ast 1" =
  let text = "1 * 8 + 7 * 123 + 123212141 + 123 * 123 * 3" in
  let ast = parse_expr (tokenize_text text) in
  print_expr (fst ast);
  [%expect {| (((1)*(8))+(((7)*(123))+((123212141)+((123)*((123)*(3)))))) |}]

let%expect_test "long basic ast 2" =
  let text = "1 * 3 / 7 * 1337 + 123 - 123 - 421 / 321 * 2" in
  let ast = parse_expr (tokenize_text text) in
  print_expr (fst ast);
  [%expect {| (((1)*((3)/((7)*(1337))))+((123)-((123)-((421)/((321)*(2)))))) |}]

let%expect_test "long basic ast 3" =
  let text = "1 / 3 / 4 / 5 / 6 / 7" in
  let ast = parse_expr (tokenize_text text) in
  print_expr (fst ast);
  [%expect {| ((1)/((3)/((4)/((5)/((6)/(7)))))) |}]

let%expect_test "long basic ast 4" =
  let text = "1 * 3 / 4 * 5 / 6 * 7" in
  let ast = parse_expr (tokenize_text text) in
  print_expr (fst ast);
  [%expect {| ((1)*((3)/((4)*((5)/((6)*(7)))))) |}]

let%expect_test "parenthesis test 1" =
  let text = "1 + ( 2 + 3 )" in
  let ast = parse_expr (tokenize_text text) in
  print_expr (fst ast);
  [%expect {| ((1)+((2)+(3))) |}]

let%expect_test "parenthesis test 2" =
  let text = "( 1 + 2 ) + 3" in
  let ast = parse_expr (tokenize_text text) in
  print_expr (fst ast);
  [%expect {| (((1)+(2))+(3)) |}]

let%expect_test "parenthesis test 3" =
  let text = "1 * ( 2 + 3 )" in
  let ast = parse_expr (tokenize_text text) in
  print_expr (fst ast);
  [%expect {| ((1)*((2)+(3))) |}]

let%expect_test "parenthesis test 4" =
  let text = "( 2 + 3 ) * 4" in
  let ast = parse_expr (tokenize_text text) in
  print_expr (fst ast);
  [%expect {| (((2)+(3))*(4)) |}]

let%expect_test "unary test 1" =
  let text = "-( 2 + 3 )" in
  let ast = parse_expr (tokenize_text text) in
  print_expr (fst ast);
  [%expect {| (-((2)+(3))) |}]

let%expect_test "unary test 1" =
  let text = "+( 3 * 3 )" in
  let ast = parse_expr (tokenize_text text) in
  print_expr (fst ast);
  [%expect {| ((3)*(3)) |}]

let%expect_test "assignment test 1" =
  let text = "variable := 42 ;" in
  let ast, _ = parse_stmts (tokenize_text text) in
  print_ast ast;
  [%expect {| (variable) = (42) ; |}]

let%expect_test "assignment test 2" =
  let text = "foo := 42 * 1337 ; bar := 143 / 666 * 23 + 777 / 42 ;" in
  let ast, _ = parse_stmts (tokenize_text text) in
  print_ast ast;
  [%expect {| (foo) = ((42)*(1337)) ; (bar) = (((143)/((666)*(23)))+((777)/(42))) ; |}]

let%expect_test "whilestatement test 1" =
  let text = "while  1 + 2 do begin variable := 1 ; end;" in
  let ast, _ = parse_stmts (tokenize_text text) in
  print_ast ast;
  [%expect {| while (((1)+(2))) { (variable) = (1) ; } |}]

let%expect_test "ifstatement test 1" =
  let text = "if  1 + 2 then begin variable := 1 ; end;" in
  let ast, _ = parse_stmts (tokenize_text text) in
  print_ast ast;
  [%expect {| if (((1)+(2))) { (variable) = (1) ; } |}]

let%expect_test "tokenizer test 1" =
  let text = "if 1+2 then  begin variable:=1; end;" in
  let ast, _ = parse_stmts (tokenize_text text) in
  print_ast ast;
  [%expect {| if (((1)+(2))) { (variable) = (1) ; } |}]

let%expect_test "tokenizer test 2" =
  let text = "if 1+2 then  begin variable:=1; end;" in
  let ast, _ = parse_stmts (tokenize_text text) in
  print_ast ast;
  [%expect {| if (((1)+(2))) { (variable) = (1) ; } |}]

let%expect_test "nested if's test" =
  let text = "
              if 3 then begin
                if 2 then begin
                  if 3 then begin
                    return 1;
                  end;
                end;
              end;
              " in
  let ast, _ = parse_stmts (tokenize_text text) in
  print_ast ast;
  [%expect {| if ((3)) { if ((2)) { if ((3)) { return (1) ; }}} |}]


let%expect_test "factorial test" =
  let text = "acc:=1; n:=5; while n>1 do begin acc:=acc*n; n:=n-1; end;" in
  let ast, _ = parse_stmts (tokenize_text text) in
  print_ast ast;
  [%expect {| (acc) = (1) ; (n) = (5) ; while (((n)>(1))) { (acc) = ((acc)*(n)) ; (n) = ((n)-(1)) ; } |}]

let%expect_test "factorial test" =
  let text = "bool := n > 1 ;" in
  let ast, _ = parse_stmts (tokenize_text text) in
  print_ast ast;
  [%expect {| (bool) = ((n)>(1)) ; |}]

 
