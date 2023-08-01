open Opal
open Ast

let integer = many1 digit => implode % int_of_string => fun n -> Atom (Num n)

let ident =
  many (alpha_num <|> exactly '_') => implode => fun i -> Atom (Ident i)

let operator =
  many1 (exactly '+' <|> exactly '-' <|> exactly '/' <|> exactly '*') => implode
  => fun o -> Atom (Operator o)

let parens = between (exactly '(') (exactly ')')

let str =
  between (exactly '"') (exactly '"') (many (satisfy (fun s -> s != '"')))
  => implode
  => fun s -> Atom (Str s)

let atom = str <|> integer <|> operator <|> ident

let rec expr input =
  (parens (sep_by expr space) => (fun l -> ListSexp l) <|> atom) input
