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

let stdlib_code =
  {|
(define (len s) (string-length s))
(define (str-slice s start end) (substring s start end))
(define (str-upper s) (string-upcase s))
(define (str-lower s) (string-downcase s))
(define PI 3.141592653589793)
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (abs x) (if (< x 0) (- 0 x) x))
(define (max a b) (if (> a b) a b))
(define (min a b) (if (< a b) a b))
(define (pow base exp)
  (if (= exp 0)
      1
      (* base (pow base (- exp 1)))))
|}

let setup_env () =
  let env = empty_env () in
  let sexps = Result.get_ok (Parser.parse (Stream.of_string stdlib_code)) in
  List.iter (fun sexp -> ignore (eval sexp env ~debug:false)) sexps;
  env

let test_len () =
  let env = setup_env () in
  let sexp = ListSexp [ Atom (Ident "len"); Atom (Str "hello") ] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable) "evaluates len" (NumVal 5.0) result

let test_str_slice () =
  let env = setup_env () in
  let sexp =
    ListSexp
      [
        Atom (Ident "str-slice");
        Atom (Str "hello");
        Atom (Num 1.0);
        Atom (Num 3.0);
      ]
  in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable)
    "evaluates str-slice" (StrVal "ell") result

let test_str_upper () =
  let env = setup_env () in
  let sexp = ListSexp [ Atom (Ident "str-upper"); Atom (Str "hello") ] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable)
    "evaluates str-upper" (StrVal "HELLO") result

let test_str_lower () =
  let env = setup_env () in
  let sexp = ListSexp [ Atom (Ident "str-lower"); Atom (Str "HELLO") ] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable)
    "evaluates str-lower" (StrVal "hello") result

let test_pi () =
  let env = setup_env () in
  let sexp = Atom (Ident "PI") in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable)
    "evaluates PI" (NumVal 3.141592653589793) result

let test_square () =
  let env = setup_env () in
  let sexp = ListSexp [ Atom (Ident "square"); Atom (Num 3.0) ] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable) "evaluates square" (NumVal 9.0) result

let test_cube () =
  let env = setup_env () in
  let sexp = ListSexp [ Atom (Ident "cube"); Atom (Num 3.0) ] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable) "evaluates cube" (NumVal 27.0) result

let test_abs () =
  let env = setup_env () in
  let sexp = ListSexp [ Atom (Ident "abs"); Atom (Num (-3.0)) ] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable) "evaluates abs" (NumVal 3.0) result

let test_max () =
  let env = setup_env () in
  let sexp = ListSexp [ Atom (Ident "max"); Atom (Num 3.0); Atom (Num 5.0) ] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable) "evaluates max" (NumVal 5.0) result

let test_min () =
  let env = setup_env () in
  let sexp = ListSexp [ Atom (Ident "min"); Atom (Num 3.0); Atom (Num 5.0) ] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable) "evaluates min" (NumVal 3.0) result

let test_pow () =
  let env = setup_env () in
  let sexp = ListSexp [ Atom (Ident "pow"); Atom (Num 2.0); Atom (Num 3.0) ] in
  let result = eval sexp env ~debug:false in
  Alcotest.(check yaran_val_testable) "evaluates pow" (NumVal 8.0) result

let () =
  run "Stdlib"
    [
      ( "string functions",
        [
          test_case "len" `Quick test_len;
          test_case "str_slice" `Quick test_str_slice;
          test_case "str_upper" `Quick test_str_upper;
          test_case "str_lower" `Quick test_str_lower;
        ] );
      ( "math functions",
        [
          test_case "PI" `Quick test_pi;
          test_case "square" `Quick test_square;
          test_case "cube" `Quick test_cube;
          test_case "abs" `Quick test_abs;
          test_case "max" `Quick test_max;
          test_case "min" `Quick test_min;
          test_case "pow" `Quick test_pow;
        ] );
    ]
