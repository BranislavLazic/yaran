open Ast

exception Not_number of string
exception Unsupported_operation of string

let rec for_all_nums sexp =
  match sexp with Atom (Num _) :: t -> true && for_all_nums t | _ -> false

let rec eval_numeric_operation = function
  | ListSexp (Atom (Operator "+") :: t) -> add t
  | ListSexp (Atom (Operator "-") :: t) -> subtract t
  | ListSexp (Atom (Operator "*") :: t) -> multiply t
  | ListSexp (Atom (Operator "/") :: t) -> divide t
  | ListSexp (Atom a :: _) ->
      raise
        (Unsupported_operation
           (Printf.sprintf "The operation '%s' is not supported." (show_atom a)))
  | _ ->
      raise
        (Unsupported_operation
           (Printf.sprintf "The operation is not supported."))

and add (sexp : sexp list) =
  List.fold_left (fun a c -> a + c) 0 (nums_only sexp)

and subtract (sexp : sexp list) =
  let nums = nums_only sexp in
  List.fold_left (fun a c -> a - c) (List.hd nums) (List.tl nums)

and multiply (sexp : sexp list) =
  List.fold_left (fun a c -> a * c) 1 (nums_only sexp)

and divide (sexp : sexp list) =
  let nums = nums_only sexp in
  List.fold_left (fun a c -> a / c) (List.hd nums) (List.tl nums)

and nums_only (sexp : sexp list) =
  match sexp with
  | Atom (Num num) :: rest -> [ num ] @ nums_only rest
  | ListSexp (Atom (Operator op) :: inner_rest) :: rest ->
      [
        eval_numeric_operation (ListSexp ([ Atom (Operator op) ] @ inner_rest));
      ]
      @ nums_only rest
  | [] -> []
  | _ -> raise (Not_number "Not a number.")
