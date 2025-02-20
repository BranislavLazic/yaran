open Token

type lexer_error = LexerError of string [@@deriving show]

exception Illegal_token_found of string

type lexer = { read_position : int; ch : char option } [@@deriving show]

let init_lexer = { read_position = -1; ch = None }

let is_letter ch =
  (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'

let is_digit ch = ch >= '0' && ch <= '9'
let is_operator ch = List.exists (fun c -> c == ch) [ '+'; '-'; '*'; '/' ]
let is_whitespace ch = ch == ' ' || ch == '\t' || ch == '\n'

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
      raise
        (Illegal_token_found
           (Printf.sprintf "Illegal characters found %s, %s." (show_tok ltok)
              (show_tok rtok)))

(* Rip and tear... until it is None *)
let rec next_token input lexer =
  let lxr = read_char input lexer in
  match lxr.ch with
  | Some '(' -> (OpenParens, lxr)
  | Some ')' -> (ClosedParens, lxr)
  | Some ch when is_letter ch -> handle_letter input lxr ch
  | Some ch when is_digit ch -> handle_digit input lxr ch
  | Some ch when is_operator ch -> handle_operator input lxr ch
  | Some ch when is_whitespace ch -> next_token input lxr
  | Some ch ->
      raise
        (Illegal_token_found
           (Printf.sprintf "Illegal character '%s' found at position index %d."
              (String.make 1 ch) lxr.read_position))
  | None -> (Eof, lxr)

and handle_token input lxr ch token_constructor check_function =
  let token = token_constructor (String.make 1 ch) in
  if check_peek_char check_function input then
    let tok, lxr = next_token input lxr in
    (concat_tokens (token, tok), lxr)
  else (token, lxr)

and handle_letter input lxr ch =
  handle_token input lxr ch (fun s -> Ident s) is_letter

and handle_digit input lxr ch =
  handle_token input lxr ch (fun s -> Num s) is_digit

and handle_operator input lxr ch =
  handle_token input lxr ch (fun s -> Operator s) is_operator

let try_read read_fun input lexer =
  try Ok (read_fun input lexer)
  with Illegal_token_found e -> Error (LexerError e)

let rec unsafe_read_all input lexer =
  match next_token input lexer with
  | Eof, _ -> []
  | tok, lxr -> [ tok ] @ unsafe_read_all input lxr

let read_all input lexer = try_read unsafe_read_all input lexer
