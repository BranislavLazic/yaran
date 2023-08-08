open Yaran.Ast
open Yaran.Parser

let parse_expr input =
  match parse input with
  | Ok ans -> Printf.printf "%s" (show_sexp ans)
  | Error e ->
      List.iter (fun e -> print_endline (Printf.sprintf "ERROR: %s" e)) e.errors

let () =
  if Array.length Sys.argv - 1 < 1 then print_endline "Eval some expression"
  else parse_expr (Stream.of_string Sys.argv.(1))
