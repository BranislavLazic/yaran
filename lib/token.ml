type tok =
  | OpenParens
  | ClosedParens
  | Ident of string
  | Num of string
  | Operator of string
  | Eof
[@@deriving show]
