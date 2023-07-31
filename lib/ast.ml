type atom = Num of int | Ident of string | Operator of string
[@@deriving show]

type sexp = Atom of atom | ListSexp of sexp list [@@deriving show]
type statement = Statement of sexp list [@@deriving show]
