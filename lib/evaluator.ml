open Builtin
open Ast

type environment = { values : (string, string) Hashtbl.t }

let rec eval sexp env ~debug =
  match sexp with
  | ListSexp (Atom (Operator op) :: args) -> eval_operator op args
  | Atom (Num n) -> Printf.sprintf "%d" n
  | Atom (Bool b) -> Printf.sprintf "%b" b
  | Atom (Ident id) -> eval_identifier id env
  | ListSexp sexps -> eval_list sexps env ~debug
  | s when debug -> Printf.sprintf "%s" (show_sexp s)
  | _ ->
      raise
        (Unsupported_operation
           (Printf.sprintf "The expression cannot be evaluated."))

and eval_operator op args =
  match op with
  | "+" ->
      Printf.sprintf "%d"
        (eval_numeric_operation (ListSexp (Atom (Operator "+") :: args)))
  | "-" ->
      Printf.sprintf "%d"
        (eval_numeric_operation (ListSexp (Atom (Operator "-") :: args)))
  | "*" ->
      Printf.sprintf "%d"
        (eval_numeric_operation (ListSexp (Atom (Operator "*") :: args)))
  | "/" ->
      Printf.sprintf "%d"
        (eval_numeric_operation (ListSexp (Atom (Operator "/") :: args)))
  | _ ->
      raise
        (Unsupported_operation (Printf.sprintf "Unsupported operator: %s" op))

and eval_identifier id env =
  try Hashtbl.find env.values id
  with Not_found ->
    raise (Unsupported_operation (Printf.sprintf "Unknown identifier: %s" id))

and eval_list sexps env ~debug =
  match sexps with
  | [] -> "nil"
  | _ -> String.concat " " (List.map (fun sexp -> eval sexp env ~debug) sexps)
