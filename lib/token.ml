type tok =
  | OpenParens
  | ClosedParens
  | Ident of string
  | Num of string
  | Operator of string
  | Illegal of string
  | Eof
[@@deriving show]
