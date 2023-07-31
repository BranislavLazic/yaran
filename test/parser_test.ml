open Yaran
open Opal
open Ast
open Alcotest

let test_parser_atom () =
  let res = Option.get (parse Parser.expr (LazyStream.of_string "1")) in
  Alcotest.(check @@ string)
    "same atoms" (show_sexp (Atom (Num 1))) (show_sexp res)

let test_parser_atom_in_list () =
  let res = Option.get (parse Parser.expr (LazyStream.of_string "((1))")) in
  Alcotest.(check @@ string)
    "same expression lists"
    (show_sexp (ListSexp [ ListSexp [ Atom (Num 1) ] ]))
    (show_sexp res)

let test_parser_nested_expressions () =
  let res =
    Option.get (parse Parser.expr (LazyStream.of_string "(1 2 (a__b a))"))
  in
  Alcotest.(check @@ string)
    "same expression lists"
    (show_sexp
       (ListSexp
          [
            Atom (Num 1);
            Atom (Num 2);
            ListSexp [ Atom (Ident "a__b"); Atom (Ident "a") ];
          ]))
    (show_sexp res)

let test_parser_operator_in_list () =
  let res =
    Option.get (parse Parser.expr (LazyStream.of_string "(++ 1 2 3)"))
  in
  Alcotest.(check @@ string)
    "same expression lists"
    (show_sexp
       (ListSexp
          [ Atom (Operator "++"); Atom (Num 1); Atom (Num 2); Atom (Num 3) ]))
    (show_sexp res)

let () =
  run "Parser"
    [
      ( "expressions test",
        [
          test_case "parse a nested expression of numbers" `Quick
            test_parser_nested_expressions;
        ] );
      ( "atoms test",
        [
          test_case "parse an atom" `Quick test_parser_atom;
          test_case "parse an atom in in a list" `Quick test_parser_atom_in_list;
        ] );
      ( "operators test",
        [
          test_case "parse an operator in a list" `Quick
            test_parser_operator_in_list;
        ] );
    ]
