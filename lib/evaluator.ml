open Ast

type yaran_val =
  | NumVal of float
  | BoolVal of bool
  | StrVal of string
  | NilVal
  | Closure of { params : Ast.sexp list; body : Ast.sexp; env : environment }
  | Builtin of (yaran_val list -> yaran_val)

and environment = {
  values : (string, yaran_val) Hashtbl.t;
  parent : environment option;
}

exception Unsupported_operation of string
exception Not_a_list of string

let show_yaran_val = function
  | NumVal n -> string_of_float n
  | BoolVal b -> string_of_bool b
  | StrVal s -> s
  | NilVal -> "nil"
  | Closure _ -> "<closure>"
  | Builtin _ -> "<builtin>"

let builtin_ops =
  [
    ( "+",
      fun args ->
        NumVal
          (List.fold_left ( +. ) 0.0
             (List.map
                (function
                  | NumVal n -> n
                  | _ -> raise (Unsupported_operation "Not a number"))
                args)) );
    ( "-",
      fun args ->
        let nums =
          List.map
            (function
              | NumVal n -> n
              | _ -> raise (Unsupported_operation "Not a number"))
            args
        in
        match nums with
        | [] ->
            raise
              (Unsupported_operation "Subtraction requires at least one number.")
        | h :: t -> NumVal (List.fold_left ( -. ) h t) );
    ( "*",
      fun args ->
        NumVal
          (List.fold_left ( *. ) 1.0
             (List.map
                (function
                  | NumVal n -> n
                  | _ -> raise (Unsupported_operation "Not a number"))
                args)) );
    ( "/",
      fun args ->
        let nums =
          List.map
            (function
              | NumVal n -> n
              | _ -> raise (Unsupported_operation "Not a number"))
            args
        in
        match nums with
        | [] ->
            raise
              (Unsupported_operation "Division requires at least one number.")
        | h :: t -> NumVal (List.fold_left ( /. ) h t) );
    ( ">",
      fun args ->
        let nums =
          List.map
            (function
              | NumVal n -> n
              | _ -> raise (Unsupported_operation "Not a number"))
            args
        in
        match nums with
        | [ h1; h2 ] -> BoolVal (h1 > h2)
        | _ ->
            raise
              (Unsupported_operation
                 "Comparison operations require exactly two numbers.") );
    ( "<",
      fun args ->
        let nums =
          List.map
            (function
              | NumVal n -> n
              | _ -> raise (Unsupported_operation "Not a number"))
            args
        in
        match nums with
        | [ h1; h2 ] -> BoolVal (h1 < h2)
        | _ ->
            raise
              (Unsupported_operation
                 "Comparison operations require exactly two numbers.") );
    ( ">=",
      fun args ->
        let nums =
          List.map
            (function
              | NumVal n -> n
              | _ -> raise (Unsupported_operation "Not a number"))
            args
        in
        match nums with
        | [ h1; h2 ] -> BoolVal (h1 >= h2)
        | _ ->
            raise
              (Unsupported_operation
                 "Comparison operations require exactly two numbers.") );
    ( "<=",
      fun args ->
        let nums =
          List.map
            (function
              | NumVal n -> n
              | _ -> raise (Unsupported_operation "Not a number"))
            args
        in
        match nums with
        | [ h1; h2 ] -> BoolVal (h1 <= h2)
        | _ ->
            raise
              (Unsupported_operation
                 "Comparison operations require exactly two numbers.") );
    ( "=",
      fun args ->
        let nums =
          List.map
            (function
              | NumVal n -> n
              | _ -> raise (Unsupported_operation "Not a number"))
            args
        in
        match nums with
        | [ h1; h2 ] -> BoolVal (h1 = h2)
        | _ ->
            raise
              (Unsupported_operation
                 "Comparison operations require exactly two numbers.") );
    ( "string-length",
      fun args ->
        match args with
        | [ StrVal s ] -> NumVal (float_of_int (String.length s))
        | _ -> raise (Unsupported_operation "string-length expects a string") );
    ( "string-append",
      fun args ->
        let strings =
          List.map
            (function
              | StrVal s -> s
              | _ ->
                  raise (Unsupported_operation "string-append expects strings"))
            args
        in
        StrVal (String.concat "" strings) );
    ( "substring",
      fun args ->
        match args with
        | [ StrVal s; NumVal start; NumVal len ] ->
            StrVal (String.sub s (int_of_float start) (int_of_float len))
        | _ ->
            raise
              (Unsupported_operation
                 "substring expects a string and two numbers") );
    ( "string-upcase",
      fun args ->
        match args with
        | [ StrVal s ] -> StrVal (String.uppercase_ascii s)
        | _ -> raise (Unsupported_operation "string-upcase expects a string") );
    ( "string-downcase",
      fun args ->
        match args with
        | [ StrVal s ] -> StrVal (String.lowercase_ascii s)
        | _ -> raise (Unsupported_operation "string-downcase expects a string")
    );
  ]

let empty_env () =
  let env = { values = Hashtbl.create 10; parent = None } in
  List.iter
    (fun (name, func) -> Hashtbl.add env.values name (Builtin func))
    builtin_ops;
  env

let rec eval sexp env ~debug =
  match sexp with
  | Atom (Num n) -> NumVal n
  | Atom (Bool b) -> BoolVal b
  | Atom (Str s) -> StrVal s
  | Atom (Ident id) -> eval_identifier id env
  | ListSexp [] -> NilVal
  | ListSexp [ ListSexp s ] -> eval (ListSexp s) env ~debug
  | ListSexp (Atom (Ident "define") :: tail) -> eval_define tail env ~debug
  | ListSexp (Atom (Ident "if") :: tail) -> eval_if tail env ~debug
  | ListSexp (Atom (Operator op) :: tail)
    when List.mem op (List.map fst builtin_ops) ->
      let func = List.assoc op builtin_ops in
      let args = List.map (fun arg -> eval arg env ~debug) tail in
      apply (Builtin func) args
  | ListSexp (head :: tail) ->
      let func = eval head env ~debug in
      let args = List.map (fun arg -> eval arg env ~debug) tail in
      apply func args
  | s when debug ->
      print_endline (show_sexp s);
      NilVal
  | _ ->
      raise
        (Unsupported_operation
           (Printf.sprintf "The expression cannot be evaluated."))

and eval_define args env ~debug =
  match args with
  | [ Atom (Ident name); value ] ->
      let value = eval value env ~debug in
      Hashtbl.add env.values name value;
      NilVal
  | [ ListSexp (Atom (Ident name) :: params); body ] ->
      let closure = Closure { params; body; env } in
      Hashtbl.add env.values name closure;
      NilVal
  | _ -> raise (Unsupported_operation "Invalid define syntax")

and eval_if args env ~debug =
  match args with
  | [ cond; then_branch ] -> (
      match eval cond env ~debug with
      | BoolVal true -> eval then_branch env ~debug
      | BoolVal false -> NilVal
      | _ -> raise (Unsupported_operation "Condition must be a boolean"))
  | [ cond; then_branch; else_branch ] -> (
      match eval cond env ~debug with
      | BoolVal true -> eval then_branch env ~debug
      | BoolVal false -> eval else_branch env ~debug
      | _ -> raise (Unsupported_operation "Condition must be a boolean"))
  | _ -> raise (Unsupported_operation "Invalid if syntax")

and eval_identifier id env =
  try Hashtbl.find env.values id
  with Not_found -> (
    match env.parent with
    | Some p -> eval_identifier id p
    | None ->
        raise
          (Unsupported_operation (Printf.sprintf "Unknown identifier: %s" id)))

and apply func args =
  match func with
  | Builtin f -> f args
  | Closure { params; body; env } ->
      let new_env = { values = Hashtbl.create 10; parent = Some env } in
      List.iter2
        (fun param arg ->
          match param with
          | Atom (Ident name) -> Hashtbl.add new_env.values name arg
          | _ -> raise (Unsupported_operation "Invalid parameter list"))
        params args;
      eval body new_env ~debug:false
  | _ ->
      raise
        (Unsupported_operation
           (Printf.sprintf "Not a function: %s" (show_yaran_val func)))
