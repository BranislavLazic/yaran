type atom =
  | Num of int
  | Ident of string
  | Operator of string
  | Bool of bool
  | Nil
  | Empty
  | Illegal of string
[@@deriving show]

type sexp = Atom of atom | ListSexp of sexp list [@@deriving show]
type statement = Statement of sexp list [@@deriving show]

(* Return a single atom if the list has an only one element *)
let flatten = function
  | ListSexp s when List.length s == 1 -> List.hd s
  | s -> s
