(* -*- compile-command: "opam exec -- dune exec compiler"; -*- *)

type num = int

type first_op = Add
              | Sub

type second_op = Mult
               | Div

type prod = Second_binary_op of num * second_op * prod
          | Constant of num
          | Neg of num

type expr = First_binary_op of num * first_op * expr
          | Prod of prod

(* let parse_const *)

(* let parse_expr *)

(* let parse_prod *)
