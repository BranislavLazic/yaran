open Ast
open Token

type parser_error = ParserError of string [@@deriving show]

exception Unmatched_parens of string

let to_ast = function
  | Bool b -> Atom (Bool b)
  | Nil -> Atom Nil
  | Ident id -> Atom (Ident id)
  | Num num -> Atom (Num (int_of_string num))
  | Operator op -> Atom (Operator op)
  | _ -> Atom Empty

let try_parse parser tokens =
  try
    let sexps, remaining_tokens = parser tokens in
    match remaining_tokens with
    | [] -> Ok sexps (* All tokens consumed, successful parse *)
    | [Eof] -> Ok sexps (* All tokens consumed, successful parse *)
    | ClosedParens :: _ -> Error (ParserError "Unmatched )") (* Extra closing parenthesis *)
    | OpenParens :: _ -> Error (ParserError "Unmatched (") (* Unmatched opening parenthesis *)
    | _ -> Error (ParserError "Unexpected tokens after parsing") (* Other unexpected tokens *)
  with Unmatched_parens err -> Error (ParserError err)

let rec parse tokens =
  match tokens with
  | [] -> raise (Unmatched_parens "Unexpected end of input") (* Should not happen if input is valid *)
  | OpenParens :: rest ->
      let sexps, remaining_tokens = parse_list rest in
      (ListSexp sexps, remaining_tokens)
  | ClosedParens :: _ -> raise (Unmatched_parens "Unmatched )")
  | tok :: rest -> (to_ast tok, rest)

and parse_list tokens =
  match tokens with
  | [] -> raise (Unmatched_parens "Unmatched (") (* Missing closing parenthesis *)
  | ClosedParens :: rest -> ([], rest)
  | _ ->
      let sexp, rest = parse tokens in
      let sexps, remaining_tokens = parse_list rest in
      (sexp :: sexps, remaining_tokens)

let parse input =
  match Lexer.read_all input Lexer.init_lexer with
  | Ok toks -> (
      match try_parse parse toks with
      | Ok sexps -> Ok sexps (* Removed flatten *)
      | Error err -> Error err)
  | Error (LexerError err) -> Error (ParserError err)