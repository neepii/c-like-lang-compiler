open Compiler.Parse

let random_operator =
  match Random.int 4 with
  | 1 -> "+"
  | 2 -> "-"
  | 3 -> "*"
  | _ -> "/"

type tree =
  | Leaf of int
  | Node of tree * tree

let leaf x = Leaf x
let node x y = Node (x, y)

let tree_gen = 
  QCheck.Gen.(
    sized @@ fix 
               (fun self n -> 
                 match n with 
                 | 0 -> map leaf nat
                 | n -> frequency
                          [1, map leaf nat;
                           2, map2 node (self (n/2)) (self (n/2))]))

let rec string_of_gen_tree tree =
  match tree with
  | Leaf x ->  "(" ^ string_of_int x ^ ")"
  | Node (x, y) -> "(" ^ string_of_gen_tree x ^ random_operator ^ string_of_gen_tree y ^ ")"

let arbitrary_tree = 
  QCheck.make tree_gen ~print: string_of_gen_tree

let basic_arith_expr_test =
  QCheck.Test.make ~count:1000 ~name:"arith expression"
    arbitrary_tree (fun t -> 
      let text = string_of_gen_tree t in
      let tokens = tokenize_text text in
      let ast = parse_expr tokens in
      let compilers_string = string_of_expr (fst ast) in
      text = compilers_string);;

QCheck.Test.check_exn basic_arith_expr_test
