open Token

type lexer = { read_position : int; ch : char option } [@@deriving show]

let init_lexer = { read_position = 0; ch = None }

let is_letter ch =
  (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'

let is_digit ch = ch >= '0' && ch <= '9'
let is_operator ch = List.exists (fun c -> c == ch) [ '+'; '-'; '*'; '/' ]
let is_whitespace ch = ch == ' ' || ch == '\t'

let try_next input =
  try Ok (Stream.next input) with Stream.Failure -> Error "End of file"

let read_char input ({ read_position; _ } : lexer) =
  match try_next input with
  | Ok ch -> { read_position = read_position + 1; ch = Some ch }
  | Error _ -> { read_position; ch = None }

let check_peek_char cond input =
  match Stream.peek input with Some ch when cond ch -> true | _ -> false

let concat_tokens = function
  | Ident lvalue, Ident rvalue -> Ident (lvalue ^ rvalue)
  | Num lvalue, Num rvalue -> Num (lvalue ^ rvalue)
  | Operator lvalue, Operator rvalue -> Operator (lvalue ^ rvalue)
  | Ident lvalue, _ -> Ident lvalue
  | Num lvalue, _ -> Num lvalue
  | Operator lvalue, _ -> Operator lvalue
  | ltok, rtok ->
      Illegal
        (Printf.sprintf "Illegal characters found %s, %s." (show_tok ltok)
           (show_tok rtok))

let rec next_token input lexer =
  let lxr = read_char input lexer in
  match lxr.ch with
  | Some '(' -> OpenParens
  | Some ')' -> ClosedParens
  | Some ch when is_letter ch ->
      let ident = Ident (String.make 1 ch) in
      if check_peek_char is_letter input then
        concat_tokens (ident, next_token input lxr)
      else ident
  | Some ch when is_digit ch ->
      let num = Num (String.make 1 ch) in
      if check_peek_char is_digit input then
        concat_tokens (num, next_token input lxr)
      else num
  | Some ch when is_operator ch ->
      let op = Operator (String.make 1 ch) in
      if check_peek_char is_operator input then
        concat_tokens (op, next_token input lxr)
      else op
  | Some ch when is_whitespace ch -> next_token input lxr
  | Some ch ->
      Illegal
        (Printf.sprintf "Illegal character '%s' found at position index %d."
           (String.make 1 ch) lxr.read_position)
  | None -> Eof

let rec read_toks input lexer =
  match next_token input lexer with
  | Eof -> []
  | Illegal i -> [ Illegal i ]
  | tok -> [ tok ] @ read_toks input lexer
