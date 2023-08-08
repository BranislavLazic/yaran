type atom = Num of int | Ident of string | Operator of string | Empty
[@@deriving show]

type sexp = Atom of atom | ListSexp of sexp list [@@deriving show]
type statement = Statement of sexp list [@@deriving show]

(* Flatten a list of S-Expressions or return a single atom if the list has an only one element *)
let flatten s =
  match s with
  | ListSexp [ ListSexp sexp ] -> ListSexp sexp
  | ListSexp s when List.length s == 1 -> List.hd s
  | s -> s
