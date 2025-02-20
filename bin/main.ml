open Yaran.Parser
open Yaran.Evaluator

let run_program input =
  let env = { values = Hashtbl.create 1024 } in
  match parse input with
  | Ok ans -> Printf.printf "%s" (eval ans env ~debug:true)
  | Error (ParserError err) ->
      print_endline (Printf.sprintf "Error while parsing: \"%s\"" err)

let () =
  if Array.length Sys.argv - 1 < 1 then print_endline "Eval some expression"
  else run_program (Stream.of_string Sys.argv.(1))
