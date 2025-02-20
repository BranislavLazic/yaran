open Alcotest
open Yaran
open Token

let test_lexer_empty_expr () =
  let input = Stream.of_string "()" in
  let lexer = Lexer.init_lexer in
  let res_open_parens, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string)
    "same tokens" (show_tok OpenParens) (show_tok res_open_parens);
  let res_closed_parens, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string)
    "same tokens" (show_tok ClosedParens)
    (show_tok res_closed_parens);
  let res_eof, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string) "same tokens" (show_tok Eof) (show_tok res_eof)

let test_lexer_identifier () =
  let input = Stream.of_string "(abc)" in
  let lexer = Lexer.init_lexer in
  let res_open_parens, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string)
    "same tokens" (show_tok OpenParens) (show_tok res_open_parens);
  let res_ident, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string)
    "same tokens" (show_tok (Ident "abc")) (show_tok res_ident);
  let res_closed_parens, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string)
    "same tokens" (show_tok ClosedParens)
    (show_tok res_closed_parens);
  let res_eof, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string) "same tokens" (show_tok Eof) (show_tok res_eof)

let test_lexer_number () =
  let input = Stream.of_string "(123)" in
  let lexer = Lexer.init_lexer in
  let res_open_parens, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string)
    "same tokens" (show_tok OpenParens) (show_tok res_open_parens);
  let res_number, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string)
    "same tokens" (show_tok (Num "123")) (show_tok res_number);
  let res_closed_parens, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string)
    "same tokens" (show_tok ClosedParens)
    (show_tok res_closed_parens);
  let res_eof, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string) "same tokens" (show_tok Eof) (show_tok res_eof)

let test_lexer_operator () =
  let input = Stream.of_string "(+ 1 2)" in
  let lexer = Lexer.init_lexer in
  let res_open_parens, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string)
    "same tokens" (show_tok OpenParens) (show_tok res_open_parens);
  let res_operator, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string)
    "same tokens" (show_tok (Operator "+")) (show_tok res_operator);
  let res_num1, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string)
    "same tokens" (show_tok (Num "1")) (show_tok res_num1);
  let res_num2, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string)
    "same tokens" (show_tok (Num "2")) (show_tok res_num2);
  let res_closed_parens, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string)
    "same tokens" (show_tok ClosedParens)
    (show_tok res_closed_parens);
  let res_eof, _ = Lexer.next_token input lexer in
  Alcotest.(check @@ string) "same tokens" (show_tok Eof) (show_tok res_eof)

let test_lexer_read_toks_single_atom () =
  let input = Stream.of_string "123" in
  let toks = Result.get_ok (Lexer.read_all input Lexer.init_lexer) in
  let toks_str = List.map (fun t -> show_tok t) toks in
  Alcotest.(check @@ list string)
    "same tokens"
    [ show_tok (Token.Num "123") ]
    toks_str

let test_lexer_read_toks_simple_expression () =
  let input = Stream.of_string "(123)" in
  let toks = Result.get_ok (Lexer.read_all input Lexer.init_lexer) in
  let toks_str = List.map (fun t -> show_tok t) toks in
  Alcotest.(check @@ list string)
    "same tokens"
    [
      show_tok Token.OpenParens;
      show_tok (Token.Num "123");
      show_tok Token.ClosedParens;
    ]
    toks_str

let test_lexer_read_toks_ignore_whitespace () =
  let input = Stream.of_string "   (  111 abc  )     " in
  let toks = Result.get_ok (Lexer.read_all input Lexer.init_lexer) in
  let toks_str = List.map (fun t -> show_tok t) toks in
  Alcotest.(check @@ list string)
    "same tokens"
    [
      show_tok Token.OpenParens;
      show_tok (Token.Num "111");
      show_tok (Token.Ident "abc");
      show_tok Token.ClosedParens;
    ]
    toks_str

let test_lexer_read_toks_illegal () =
  let input = Stream.of_string "( ~)" in
  let err = Result.get_error (Lexer.read_all input Lexer.init_lexer) in
  Alcotest.(check @@ string)
    "same errors"
    (Lexer.show_lexer_error
       (LexerError "Illegal character '~' found at position index 2."))
    (Lexer.show_lexer_error err)

let test_lexer_mixed_tokens () =
  let input = Stream.of_string "(+ 123 abc)" in
  let toks = Result.get_ok (Lexer.read_all input Lexer.init_lexer) in
  let toks_str = List.map (fun t -> show_tok t) toks in
  Alcotest.(check @@ list string)
    "same tokens"
    [
      show_tok Token.OpenParens;
      show_tok (Token.Operator "+");
      show_tok (Token.Num "123");
      show_tok (Token.Ident "abc");
      show_tok Token.ClosedParens;
    ]
    toks_str

let () =
  run "Lexer"
    [
      ( "empty expression test",
        [
          test_case "should tokenize empty expression" `Quick
            test_lexer_empty_expr;
        ] );
      ( "identifier test",
        [
          test_case "should tokenize an identifier" `Quick test_lexer_identifier;
        ] );
      ( "number test",
        [ test_case "should tokenize a number" `Quick test_lexer_number ] );
      ( "operator test",
        [ test_case "should tokenize an operator" `Quick test_lexer_operator ]
      );
      ( "read all tokens",
        [
          test_case "should read a single atom" `Quick
            test_lexer_read_toks_single_atom;
          test_case "should read a simple expression" `Quick
            test_lexer_read_toks_simple_expression;
          test_case "should ignore a whitespace" `Quick
            test_lexer_read_toks_ignore_whitespace;
          test_case "should break reading at an illegal token" `Quick
            test_lexer_read_toks_illegal;
          test_case "should read mixed tokens" `Quick test_lexer_mixed_tokens;
        ] );
    ]
