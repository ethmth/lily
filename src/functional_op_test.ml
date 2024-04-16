open Ast

let run_test description expr =
  print_endline (description ^ ": " ^ string_of_expr expr)

(* Define test cases for Map, Filter, and Reduce *)
let () =
  let test_list = ListExpr [LitInt 1; LitInt 2; LitInt 3] in
  let func1 = Id "fuc1" in  
  let func2 = Id "fuc2" in  
  let func3 = Id "fuc3" in  
  let zero = LitInt 0 in

  (* Testing Map *)
  let map_expr = Map(test_list, func1) in
  run_test "Map Test (list => fuc1)" map_expr;

  (* Testing Filter *)
  let filter_expr = Filter(test_list, func2) in
  run_test "Filter Test (list =>? fuc2)" filter_expr;

  (* Testing Reduce *)
  let reduce_expr = Reduce(test_list, func3, zero) in
  run_test "Reduce Test (list =>/ fuc3 with 0)" reduce_expr;
