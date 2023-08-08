open Ast
open Token

type parser_error = { errors : string list }

let to_ast = function
  | Ident id -> Atom (Ident id)
  | Num n -> Atom (Num (int_of_string n))
  | Operator op -> Atom (Operator op)
  | _ -> Atom Empty

let concat_sexps lsexp rsexp =
  match (lsexp, rsexp) with
  | Atom a, ListSexp [] | ListSexp [], Atom a -> ListSexp [ Atom a ]
  | ListSexp l, ListSexp r -> ListSexp (l @ r)
  | ListSexp l, Atom a -> ListSexp (l @ [ Atom a ])
  | Atom a, ListSexp l -> ListSexp ([ Atom a ] @ l)
  | Atom l, Atom r -> ListSexp [ Atom l; Atom r ]

let rec tokens_to_sexp = function
  | [] -> ListSexp []
  | OpenParens :: t when List.hd (List.rev t) == ClosedParens ->
      ListSexp [ tokens_to_sexp t ]
  | h :: t ->
      if to_ast h != Atom Empty then concat_sexps (to_ast h) (tokens_to_sexp t)
      else tokens_to_sexp t

let rec find_illegal = function
  | [] -> []
  | Illegal e :: t -> [ e ] @ find_illegal t
  | _ :: t -> find_illegal t

let parse input =
  let tokens = Lexer.read_toks input Lexer.init_lexer in
  let illegal_errors = find_illegal tokens in
  if List.length illegal_errors > 0 then Error { errors = illegal_errors }
  else Ok (flatten (tokens_to_sexp tokens))
