open Yaran.Ast
open Yaran.Parser
open Opal

let parse_expr input =
  match parse expr input with
  | Some ans -> Printf.printf "%s" (show_sexp ans)
  | None -> print_endline "ERROR!"

let () =
  if Array.length Sys.argv - 1 < 1 then print_endline "Eval some expression"
  else parse_expr (LazyStream.of_string Sys.argv.(1))
