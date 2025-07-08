open Yaran
open Ast
open Alcotest

let test_parser_atom () =
  let sexp_res = Result.get_ok (Parser.parse (Stream.of_string "123.0")) in
  Alcotest.(check @@ string)
    "same atoms"
    (show_sexp_list [ Atom (Num 123.0) ])
    (show_sexp_list sexp_res)

let test_parser_atom_in_list () =
  let sexp_res =
    Result.get_ok
      (Parser.parse
         (Stream.of_string "(+ 1.0 2.0 true false nil (a_c (33.0)))"))
  in
  Alcotest.(check @@ string)
    "same expressions"
    (show_sexp_list
       [
         ListSexp
           [
             Atom (Operator "+");
             Atom (Num 1.0);
             Atom (Num 2.0);
             Atom (Bool true);
             Atom (Bool false);
             Atom Nil;
             ListSexp [ Atom (Ident "a_c"); ListSexp [ Atom (Num 33.0) ] ];
           ];
       ])
    (show_sexp_list sexp_res)

let test_balanced_parens () =
  let sexp_res =
    Result.get_ok (Parser.parse (Stream.of_string "(+ 1.0 2.0)"))
  in
  Alcotest.(check @@ string)
    "same expressions"
    (show_sexp_list
       [ ListSexp [ Atom (Operator "+"); Atom (Num 1.0); Atom (Num 2.0) ] ])
    (show_sexp_list sexp_res)

let test_unbalanced_parens () =
  let err = Result.get_error (Parser.parse (Stream.of_string "(+ 1 2")) in
  Alcotest.(check @@ string)
    "same errors"
    (Parser.show_parser_error (ParserError "Unmatched ("))
    (Parser.show_parser_error err)

(* New tests *)

let test_parser_empty_list () =
  let sexp_res = Result.get_ok (Parser.parse (Stream.of_string "()")) in
  Alcotest.(check @@ string)
    "empty list"
    (show_sexp_list [ ListSexp [] ])
    (show_sexp_list sexp_res)

let test_parser_nested_empty_list () =
  let sexp_res = Result.get_ok (Parser.parse (Stream.of_string "(())")) in
  Alcotest.(check @@ string)
    "nested empty list"
    (show_sexp_list [ ListSexp [ ListSexp [] ] ])
    (show_sexp_list sexp_res)

let test_parser_missing_closing_parens_nested () =
  let err = Result.get_error (Parser.parse (Stream.of_string "(1 (2 3)")) in
  Alcotest.(check @@ string)
    "missing closing parens nested"
    (Parser.show_parser_error (ParserError "Unmatched ("))
    (Parser.show_parser_error err)

let test_parser_extra_closing_parens () =
  let err = Result.get_error (Parser.parse (Stream.of_string "(1 2))")) in
  Alcotest.(check @@ string)
    "extra closing parens"
    (Parser.show_parser_error (ParserError "Unmatched )"))
    (Parser.show_parser_error err)

let () =
  run "Parser"
    [
      ("atoms test", [ test_case "parse an atom" `Quick test_parser_atom ]);
      ( "lists test",
        [
          test_case "parse a complex s-expression" `Quick
            test_parser_atom_in_list;
          test_case "parse empty list" `Quick test_parser_empty_list;
          test_case "parse nested empty list" `Quick
            test_parser_nested_empty_list;
        ] );
      ( "parentheses test",
        [
          test_case "parse balanced parentheses" `Quick test_balanced_parens;
          test_case "fail to parse unbalanced parentheses" `Quick
            test_unbalanced_parens;
          test_case "fail on missing closing parentheses nested" `Quick
            test_parser_missing_closing_parens_nested;
          test_case "fail on extra closing parentheses" `Quick
            test_parser_extra_closing_parens;
        ] );
    ]
