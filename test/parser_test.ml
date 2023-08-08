open Yaran
open Ast
open Alcotest

let test_parser_atom () =
  let sexp_res = Result.get_ok (Parser.parse (Stream.of_string "123")) in
  print_endline (show_sexp sexp_res);
  Alcotest.(check @@ string)
    "same atoms"
    (show_sexp (Atom (Num 123)))
    (show_sexp sexp_res)

let test_parser_atom_in_list () =
  let sexp_res =
    Result.get_ok (Parser.parse (Stream.of_string "(+ 1 2 (a_c (33)))"))
  in
  print_endline (show_sexp sexp_res);
  Alcotest.(check @@ string)
    "same expressions"
    (show_sexp
       (ListSexp
          [
            Atom (Operator "+");
            Atom (Num 1);
            Atom (Num 2);
            ListSexp [ Atom (Ident "a_c"); ListSexp [ Atom (Num 33) ] ];
          ]))
    (show_sexp sexp_res)

let () =
  run "Parser"
    [
      ("atoms test", [ test_case "parse an atom" `Quick test_parser_atom ]);
      ( "lists test",
        [
          test_case "parse a complex s-expression" `Quick
            test_parser_atom_in_list;
        ] );
    ]
