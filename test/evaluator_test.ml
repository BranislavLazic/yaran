open Yaran
open Ast
open Evaluator
open Builtin
open Alcotest

let empty_env () = { values = Hashtbl.create 10 }

let test_eval_num () =
  let env = empty_env () in
  let result = eval (Atom (Num 123)) env ~debug:false in
  Alcotest.(check string) "evaluates number" "123" result

let test_eval_bool () =
  let env = empty_env () in
  let result = eval (Atom (Bool true)) env ~debug:false in
  Alcotest.(check string) "evaluates boolean" "true" result

let test_eval_add () =
  let env = empty_env () in
  let sexp = ListSexp [Atom (Operator "+"); Atom (Num 1); Atom (Num 2)] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check string) "evaluates addition" "3" result

let test_eval_sub () =
  let env = empty_env () in
  let sexp = ListSexp [Atom (Operator "-"); Atom (Num 5); Atom (Num 2)] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check string) "evaluates subtraction" "3" result

let test_eval_mul () =
  let env = empty_env () in
  let sexp = ListSexp [Atom (Operator "*"); Atom (Num 3); Atom (Num 4)] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check string) "evaluates multiplication" "12" result

let test_eval_div () =
  let env = empty_env () in
  let sexp = ListSexp [Atom (Operator "/"); Atom (Num 10); Atom (Num 2)] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check string) "evaluates division" "5" result

let test_eval_mod () =
  let env = empty_env () in
  let sexp = ListSexp [Atom (Operator "mod"); Atom (Num 10); Atom (Num 3)] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check string) "evaluates modulo" "1" result

let test_eval_gt () =
  let env = empty_env () in
  let sexp = ListSexp [Atom (Operator ">"); Atom (Num 5); Atom (Num 3)] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check string) "evaluates greater than" "true" result

let test_eval_lt () =
  let env = empty_env () in
  let sexp = ListSexp [Atom (Operator "<"); Atom (Num 5); Atom (Num 3)] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check string) "evaluates less than" "false" result

let test_eval_ge () =
  let env = empty_env () in
  let sexp = ListSexp [Atom (Operator ">="); Atom (Num 5); Atom (Num 5)] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check string) "evaluates greater than or equal" "true" result

let test_eval_le () =
  let env = empty_env () in
  let sexp = ListSexp [Atom (Operator "<="); Atom (Num 3); Atom (Num 5)] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check string) "evaluates less than or equal" "true" result

let test_eval_eq () =
  let env = empty_env () in
  let sexp = ListSexp [Atom (Operator "="); Atom (Num 5); Atom (Num 5)] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check string) "evaluates equality" "true" result

let test_eval_identifier () =
  let env = empty_env () in
  Hashtbl.add env.values "x" "10";
  let result = eval (Atom (Ident "x")) env ~debug:false in
  Alcotest.(check string) "evaluates identifier" "10" result

let test_eval_unknown_identifier () =
  let env = empty_env () in
  try
    let _ = eval (Atom (Ident "y")) env ~debug:false in
    fail "Expected Unsupported_operation exception"
  with
  | Unsupported_operation msg ->
      Alcotest.(check string) "unknown identifier error" "Unknown identifier: y" msg
  | _ -> fail "Unexpected exception type"

let test_eval_unsupported_operator () =
  let env = empty_env () in
  let sexp = ListSexp [Atom (Operator "unknown"); Atom (Num 1); Atom (Num 2)] in
  try
    let _ = eval sexp env ~debug:false in
    fail "Expected Unsupported_operation exception"
  with
  | Unsupported_operation msg ->
      Alcotest.(check string) "unsupported operator error" "Unsupported operator: unknown" msg
  | _ -> fail "Unexpected exception type"

let test_eval_nested_sexp () =
  let env = empty_env () in
  let sexp = ListSexp [Atom (Operator "+"); Atom (Num 1); ListSexp [Atom (Operator "*"); Atom (Num 2); Atom (Num 3)]] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check string) "evaluates nested sexp" "7" result

let () =
  run "Evaluator"
    [
      ("basic types",
       [
         test_case "number" `Quick test_eval_num;
         test_case "boolean" `Quick test_eval_bool;
       ]);
      ("arithmetic operations",
       [
         test_case "addition" `Quick test_eval_add;
         test_case "subtraction" `Quick test_eval_sub;
         test_case "multiplication" `Quick test_eval_mul;
         test_case "division" `Quick test_eval_div;
         test_case "modulo" `Quick test_eval_mod;
       ]);
      ("comparison operations",
       [
         test_case "greater than" `Quick test_eval_gt;
         test_case "less than" `Quick test_eval_lt;
         test_case "greater than or equal" `Quick test_eval_ge;
         test_case "less than or equal" `Quick test_eval_le;
         test_case "equality" `Quick test_eval_eq;
       ]);
      ("identifiers",
       [
         test_case "existing identifier" `Quick test_eval_identifier;
         test_case "unknown identifier error" `Quick test_eval_unknown_identifier;
       ]);
      ("error handling",
       [
         test_case "unsupported operator error" `Quick test_eval_unsupported_operator;
       ]);
      ("nested expressions",
       [
         test_case "nested sexp" `Quick test_eval_nested_sexp;
       ]);
    ]
