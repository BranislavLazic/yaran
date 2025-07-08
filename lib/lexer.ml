open Token

type lexer_error = LexerError of string [@@deriving show]

exception Illegal_token_found of string

type lexer = { read_position : int; ch : char option } [@@deriving show]

let init_lexer = { read_position = -1; ch = None }

let is_letter ch =
  (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'

let is_digit ch = (ch >= '0' && ch <= '9') || ch == '.'

let is_operator ch =
  List.exists (fun c -> c == ch) [ '+'; '-'; '*'; '/'; '>'; '<'; '=' ]

let is_whitespace ch = ch == ' ' || ch == '\t' || ch == '\n'

let try_next input =
  try Ok (Stream.next input) with Stream.Failure -> Error "End of file"

let read_char input ({ read_position; _ } : lexer) =
  match try_next input with
  | Ok ch -> { read_position = read_position + 1; ch = Some ch }
  | Error _ -> { read_position; ch = None }

let check_peek_char cond input =
  match Stream.peek input with Some ch when cond ch -> true | _ -> false

let read_while predicate input initial_lxr initial_char =
  let rec read_chars current_lxr current_string =
    if check_peek_char predicate input then
      let next_lxr = read_char input current_lxr in
      match next_lxr.ch with
      | Some c -> read_chars next_lxr (current_string ^ String.make 1 c)
      | None -> (current_string, current_lxr)
    else (current_string, current_lxr)
  in
  read_chars initial_lxr (String.make 1 initial_char)

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

and handle_letter input lxr ch =
  let ident_string, final_lxr = read_while is_letter input lxr ch in
  (Token.lookup_ident ident_string, final_lxr)

and handle_digit input lxr ch =
  let num_string, final_lxr = read_while is_digit input lxr ch in
  (Num (float_of_string num_string), final_lxr)

and handle_operator input lxr ch =
  let op_string, final_lxr = read_while is_operator input lxr ch in
  (Operator op_string, final_lxr)

let try_read read_fun input lexer =
  try Ok (read_fun input lexer)
  with Illegal_token_found e -> Error (LexerError e)

let rec unsafe_read_all input lexer =
  match next_token input lexer with
  | Eof, _ -> []
  | tok, lxr -> [ tok ] @ unsafe_read_all input lxr

let read_all input lexer = try_read unsafe_read_all input lexer
