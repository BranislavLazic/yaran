type tok =
  | OpenParens
  | ClosedParens
  | Ident of string
  | Num of float
  | Operator of string
  | Eof
  | Bool of bool
  | Nil
[@@deriving show]

let lookup_ident = function
  | "mod" -> Operator "mod"
  | "true" -> Bool true
  | "false" -> Bool false
  | "nil" -> Nil
  | s -> Ident s
