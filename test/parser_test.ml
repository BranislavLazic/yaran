open Yaran
open Ast
open Alcotest

let test_parser_atom () =
  let sexp_res = Result.get_ok (Parser.parse (Stream.of_string "123")) in
  Alcotest.(check @@ string)
    "same atoms"
    (show_sexp (Atom (Num 123)))
    (show_sexp sexp_res)

let test_parser_atom_in_list () =
  let sexp_res =
    Result.get_ok
      (Parser.parse (Stream.of_string "(+ 1 2 true false nil (a_c (33)))"))
  in
  Alcotest.(check @@ string)
    "same expressions"
    (show_sexp
       (ListSexp
          [
            Atom (Operator "+");
            Atom (Num 1);
            Atom (Num 2);
            Atom (Bool true);
            Atom (Bool false);
            Atom Nil;
            ListSexp [ Atom (Ident "a_c"); ListSexp [ Atom (Num 33) ] ];
          ]))
    (show_sexp sexp_res)

let test_balanced_parens () =
  let sexp_res = Result.get_ok (Parser.parse (Stream.of_string "(+ 1 2)")) in
  Alcotest.(check @@ string)
    "same expressions"
    (show_sexp (ListSexp [ Atom (Operator "+"); Atom (Num 1); Atom (Num 2) ]))
    (show_sexp sexp_res)

let test_unbalanced_parens () =
  let err = Result.get_error (Parser.parse (Stream.of_string "(+ 1 2")) in
  Alcotest.(check @@ string)
    "same errors"
    (Parser.show_parser_error (ParserError "Unmatched ("))
    (Parser.show_parser_error err)

let () =
  run "Parser"
    [
      ("atoms test", [ test_case "parse an atom" `Quick test_parser_atom ]);
      ( "lists test",
        [
          test_case "parse a complex s-expression" `Quick
            test_parser_atom_in_list;
        ] );
      ( "parentheses test",
        [
          test_case "parse balanced parentheses" `Quick test_balanced_parens;
          test_case "fail to parse unbalanced parentheses" `Quick
            test_unbalanced_parens;
        ] );
    ]
