open Yaran.Parser
open Yaran.Evaluator

let initial_env = { values = Hashtbl.create 1024; parent = None }

let rec eval_list sexps env =
  match sexps with
  | [] -> Ok Yaran.Evaluator.NilVal
  | [ last ] -> (
      try Ok (eval last env ~debug:true)
      with Unsupported_operation err -> Error err)
  | head :: tail -> (
      try
        let _ = eval head env ~debug:false in
        eval_list tail env
      with Unsupported_operation err -> Error err)

let run_program input =
  match parse input with
  | Ok sexps -> (
      match eval_list sexps initial_env with
      | Ok value -> Printf.printf "%s\n" (show_yaran_val value)
      | Error err ->
          print_endline (Printf.sprintf "Error while evaluating: \"%s\"" err))
  | Error (ParserError err) ->
      print_endline (Printf.sprintf "Error while parsing: \"%s\"" err)

let load_stdlib () =
  let stdlib_paths = [ "stdlib/math.yaran"; "stdlib/strings.yaran" ] in
  List.iter
    (fun stdlib_path ->
      try
        let ic = open_in stdlib_path in
        let content = really_input_string ic (in_channel_length ic) in
        close_in ic;
        match parse (Stream.of_string content) with
        | Ok sexps -> (
            match eval_list sexps initial_env with
            | Ok _ -> ()
            | Error err ->
                Printf.eprintf "Failed to load stdlib: %s\n" err;
                exit 1)
        | Error (ParserError err) ->
            Printf.eprintf "Failed to load stdlib: %s\n" err;
            exit 1
      with Sys_error msg ->
        Printf.eprintf "Could not load stdlib: %s\n" msg;
        exit 1)
    stdlib_paths

let () =
  List.iter
    (fun (name, func) -> Hashtbl.add initial_env.values name (Builtin func))
    builtin_ops;
  load_stdlib ();
  if Array.length Sys.argv - 1 < 1 then print_endline "Eval some expression"
  else run_program (Stream.of_string Sys.argv.(1))
