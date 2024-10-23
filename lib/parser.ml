open Ast
open Token

type parser_error = ParserError of string [@@deriving show]

exception Unmatched_parens of string

let to_ast = function
  | Ident "nil" -> Atom Nil
  | Ident "true" -> Atom (Bool true)
  | Ident "false" -> Atom (Bool false)
  | Ident id -> Atom (Ident id)
  | Num num -> Atom (Num (int_of_string num))
  | Operator op -> Atom (Operator op)
  | _ -> Atom Empty

let try_parse parser tokens =
  try
    let sexps, _ = parser tokens in
    Ok sexps
  with Unmatched_parens err -> Error (ParserError err)

let rec parse = function
  | [] -> (ListSexp [], [])
  | OpenParens :: rest -> (
      match parse_list rest with
      | sexpr, ClosedParens :: rest' -> (sexpr, rest')
      | _ -> raise (Unmatched_parens "Unmatched ("))
  | ClosedParens :: _ -> raise (Unmatched_parens "Unmatched )")
  | tok :: rest -> (to_ast tok, rest)

and parse_list tokens =
  match tokens with
  | [] | ClosedParens :: _ -> (ListSexp [], tokens)
  | _ ->
      let sexp, rest = parse tokens in
      let rest_sexp, rest' = parse_list rest in
      (concat_sexps (ListSexp [ sexp ], rest_sexp), rest')

and concat_sexps = function
  | ListSexp left, ListSexp right -> ListSexp (left @ right)
  | _ -> ListSexp []

let parse input =
  match Lexer.read_all input Lexer.init_lexer with
  | Ok toks -> Result.map flatten (try_parse parse toks)
  | Error (LexerError err) -> Error (ParserError err)
