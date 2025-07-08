open Ast
open Token

type parser_error = ParserError of string [@@deriving show]

exception Unmatched_parens of string

let to_ast = function
  | Bool b -> Atom (Bool b)
  | Nil -> Atom Nil
  | Ident id -> Atom (Ident id)
  | Num num -> Atom (Num num)
  | Operator op -> Atom (Operator op)
  | _ -> Atom Empty

let rec parse_one tokens =
  match tokens with
  | [] -> raise (Unmatched_parens "Unexpected end of input")
  | OpenParens :: rest ->
      let sexps, remaining_tokens = parse_list rest in
      (ListSexp sexps, remaining_tokens)
  | ClosedParens :: _ -> raise (Unmatched_parens "Unmatched )")
  | tok :: rest -> (to_ast tok, rest)

and parse_list tokens =
  match tokens with
  | [] -> raise (Unmatched_parens "Unmatched (")
  | ClosedParens :: rest -> ([], rest)
  | _ ->
      let sexp, rest = parse_one tokens in
      let sexps, remaining_tokens = parse_list rest in
      (sexp :: sexps, remaining_tokens)

let rec parse_all tokens =
  match tokens with
  | [] | [ Eof ] -> ([], [])
  | _ ->
      let sexp, rest = parse_one tokens in
      let sexps, remaining_tokens = parse_all rest in
      (sexp :: sexps, remaining_tokens)

let parse input =
  match Lexer.read_all input Lexer.init_lexer with
  | Ok toks -> (
      try
        let sexps, remaining_tokens = parse_all toks in
        match remaining_tokens with
        | [] | [ Eof ] -> Ok sexps
        | _ -> Error (ParserError "Unexpected tokens at end of input")
      with Unmatched_parens err -> Error (ParserError err))
  | Error (LexerError err) -> Error (ParserError err)
