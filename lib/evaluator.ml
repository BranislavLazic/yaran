open Builtin
open Ast

type env = { values : (string, string) Hashtbl.t }

let eval sexp ~debug =
  match sexp with
  | ListSexp (Atom (Operator _) :: _) ->
      Printf.sprintf "%d" (eval_numeric_operation sexp)
  | Atom (Num n) -> Printf.sprintf "%d" n
  | Atom (Bool b) -> Printf.sprintf "%b" b
  | s when debug -> Printf.sprintf "%s" (show_sexp s)
  | _ ->
      raise
        (Unsupported_operation
           (Printf.sprintf "The expression cannot be evaluated."))

