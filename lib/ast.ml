type atom =
  | Num of float
  | Ident of string
  | Operator of string
  | Bool of bool
  | Nil
  | Empty
  | Str of string
[@@deriving show]

type sexp = Atom of atom | ListSexp of sexp list [@@deriving show]
type statement = Statement of sexp list [@@deriving show]

let show_sexp_list sexps = String.concat " " (List.map show_sexp sexps)

(* Return a single atom if the list has an only one element *)
let flatten = function
  | ListSexp s when List.length s == 1 -> List.hd s
  | s -> s
