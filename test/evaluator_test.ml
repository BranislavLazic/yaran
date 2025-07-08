open Yaran
open Ast
open Evaluator
open Alcotest

let yaran_val_testable =
  testable (fun ppf v -> Fmt.string ppf (show_yaran_val v)) ( = )

let empty_env () =
  let env = { values = Hashtbl.create 10; parent = None } in
  List.iter
    (fun (name, func) -> Hashtbl.add env.values name (Builtin func))
    builtin_ops;
  env

let test_eval_num () =
  let env = empty_env () in
  let result = eval (Atom (Num 123.0)) env ~debug:false in
  Alcotest.(check yaran_val_testable) "evaluates number" (NumVal 123.0) result

let test_eval_bool () =
  let env = empty_env () in
  let result = eval (Atom (Bool true)) env ~debug:false in
  Alcotest.(check yaran_val_testable) "evaluates boolean" (BoolVal true) result

let test_eval_add () =
  let env = empty_env () in
  let sexp = ListSexp [ Atom (Ident "+"); Atom (Num 1.0); Atom (Num 2.0) ] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable) "evaluates addition" (NumVal 3.0) result

let test_eval_define () =
  let env = empty_env () in
  let sexp =
    ListSexp [ Atom (Ident "define"); Atom (Ident "x"); Atom (Num 10.0) ]
  in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable) "evaluates define" NilVal result;
  let value = Hashtbl.find env.values "x" in
  Alcotest.(check yaran_val_testable) "defines variable" (NumVal 10.0) value

let test_eval_define_fun () =
  let env = empty_env () in
  let sexp =
    ListSexp
      [
        Atom (Ident "define");
        ListSexp [ Atom (Ident "add"); Atom (Ident "x"); Atom (Ident "y") ];
        ListSexp [ Atom (Ident "+"); Atom (Ident "x"); Atom (Ident "y") ];
      ]
  in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable) "evaluates define function" NilVal result;
  let value = Hashtbl.find env.values "add" in
  match value with Closure _ -> () | _ -> fail "Expected a closure"

let test_eval_fun_application () =
  let env = empty_env () in
  let define_sexp =
    ListSexp
      [
        Atom (Ident "define");
        ListSexp [ Atom (Ident "add"); Atom (Ident "x"); Atom (Ident "y") ];
        ListSexp [ Atom (Ident "+"); Atom (Ident "x"); Atom (Ident "y") ];
      ]
  in
  let _ = eval define_sexp env ~debug:false in
  let apply_sexp =
    ListSexp [ Atom (Ident "add"); Atom (Num 1.0); Atom (Num 2.0) ]
  in
  let result = eval apply_sexp env ~debug:false in
  Alcotest.(check yaran_val_testable) "applies function" (NumVal 3.0) result

let test_eval_if_then () =
  let env = empty_env () in
  let sexp =
    ListSexp
      [ Atom (Ident "if"); Atom (Bool true); Atom (Num 1.0); Atom (Num 2.0) ]
  in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable) "evaluates if-then" (NumVal 1.0) result

let test_eval_if_else () =
  let env = empty_env () in
  let sexp =
    ListSexp
      [ Atom (Ident "if"); Atom (Bool false); Atom (Num 1.0); Atom (Num 2.0) ]
  in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable) "evaluates if-else" (NumVal 2.0) result

let () =
  run "Evaluator"
    [
      ( "basic types",
        [
          test_case "number" `Quick test_eval_num;
          test_case "boolean" `Quick test_eval_bool;
        ] );
      ("arithmetic operations", [ test_case "addition" `Quick test_eval_add ]);
      ( "special forms",
        [
          test_case "define variable" `Quick test_eval_define;
          test_case "define function" `Quick test_eval_define_fun;
          test_case "if-then" `Quick test_eval_if_then;
          test_case "if-else" `Quick test_eval_if_else;
        ] );
      ( "function application",
        [ test_case "apply function" `Quick test_eval_fun_application ] );
    ]
