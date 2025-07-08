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
  | ListSexp (Atom (Operator "mod") :: t) -> modulo t
  | ListSexp (Atom a :: _) ->
      raise
        (Unsupported_operation
           (Printf.sprintf "The operation '%s' is not supported." (show_atom a)))
  | _ ->
      raise
        (Unsupported_operation
           (Printf.sprintf "The operation is not supported."))

and add sexps = List.fold_left ( + ) 0 (nums_only sexps)

and subtract sexps =
  match nums_only sexps with
  | [] ->
      raise (Unsupported_operation "Subtraction requires at least one number.")
  | h :: t -> List.fold_left ( - ) h t

and multiply sexps = List.fold_left ( * ) 1 (nums_only sexps)

and divide sexps =
  match nums_only sexps with
  | [] -> raise (Unsupported_operation "Division requires at least one number.")
  | h :: t -> List.fold_left ( / ) h t

and modulo sexps =
  match nums_only sexps with
  | [ h; t ] -> h mod t
  | _ -> raise (Unsupported_operation "Modulo requires exactly two numbers.")

and nums_only sexps =
  List.map
    (function
      | Atom (Num num) -> num
      | ListSexp (Atom (Operator op) :: inner_rest) ->
          eval_numeric_operation (ListSexp (Atom (Operator op) :: inner_rest))
      | _ -> raise (Not_number "Not a number."))
    sexps

let eval_comparison_operation op sexps =
  match nums_only sexps with
  | [ h1; h2 ] -> (
      match op with
      | ">" -> h1 > h2
      | "<" -> h1 < h2
      | ">=" -> h1 >= h2
      | "<=" -> h1 <= h2
      | "=" -> h1 = h2
      | _ ->
          raise
            (Unsupported_operation
               (Printf.sprintf "The comparison operation '%s' is not supported."
                  op)))
  | _ ->
      raise
        (Unsupported_operation
           "Comparison operations require exactly two numbers.")
