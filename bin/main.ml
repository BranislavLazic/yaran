open Yaran.Ast
open Yaran.Parser

let parse_expr input =
  match parse input with
  | Ok ans -> Printf.printf "%s" (show_sexp ans)
  | Error (ParserError err) ->
      print_endline (Printf.sprintf "Error while parsing: \"%s\"" err)

let () =
  if Array.length Sys.argv - 1 < 1 then print_endline "Eval some expression"
  else parse_expr (Stream.of_string Sys.argv.(1))
