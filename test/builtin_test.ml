open Alcotest
open Yaran

let test_builtin_add_numbers () =
  let res =
    Builtin.eval_numeric_operation
      (ListSexp
         [ Atom (Operator "+"); Atom (Num 5); Atom (Num 5); Atom (Num 6) ])
  in
  Alcotest.(check @@ int) "same nums" 16 res

let test_builtin_subtract_numbers () =
  let res =
    Builtin.eval_numeric_operation
      (ListSexp
         [ Atom (Operator "-"); Atom (Num 98); Atom (Num 5); Atom (Num 3) ])
  in
  Alcotest.(check @@ int) "same nums" 90 res

let test_builtin_multiply_numbers () =
  let res =
    Builtin.eval_numeric_operation
      (ListSexp
         [ Atom (Operator "*"); Atom (Num 5); Atom (Num 5); Atom (Num 2) ])
  in
  Alcotest.(check @@ int) "same nums" 50 res

let test_builtin_divide_numbers () =
  let res =
    Builtin.eval_numeric_operation
      (ListSexp
         [ Atom (Operator "/"); Atom (Num 30); Atom (Num 5); Atom (Num 2) ])
  in
  Alcotest.(check @@ int) "same nums" 3 res

let test_builtin_modulo_numbers () =
  let res =
    Builtin.eval_numeric_operation
      (ListSexp [ Atom (Operator "mod"); Atom (Num 10); Atom (Num 3) ])
  in
  Alcotest.(check @@ int) "same nums" 1 res

let test_builtin_greater_than () =
  let res =
    Builtin.eval_comparison_operation ">" [ Atom (Num 10); Atom (Num 5) ]
  in
  Alcotest.(check @@ bool) "10 > 5" true res

let test_builtin_less_than () =
  let res =
    Builtin.eval_comparison_operation "<" [ Atom (Num 5); Atom (Num 10) ]
  in
  Alcotest.(check @@ bool) "5 < 10" true res

let test_builtin_greater_than_or_equal () =
  let res =
    Builtin.eval_comparison_operation ">=" [ Atom (Num 10); Atom (Num 10) ]
  in
  Alcotest.(check @@ bool) "10 >= 10" true res

let test_builtin_less_than_or_equal () =
  let res =
    Builtin.eval_comparison_operation "<=" [ Atom (Num 5); Atom (Num 5) ]
  in
  Alcotest.(check @@ bool) "5 <= 5" true res

let test_builtin_equal () =
  let res =
    Builtin.eval_comparison_operation "=" [ Atom (Num 5); Atom (Num 5) ]
  in
  Alcotest.(check @@ bool) "5 = 5" true res

let test_builtin_add_and_multiply_numbers_nested () =
  let res =
    Builtin.eval_numeric_operation
      (ListSexp
         [
           Atom (Operator "+");
           Atom (Num 5);
           Atom (Num 5);
           ListSexp
             [ Atom (Operator "*"); Atom (Num 5); Atom (Num 2); Atom (Num 8) ];
         ])
  in
  Alcotest.(check @@ int) "same nums" 90 res

(* New tests for error handling and edge cases *)

let test_builtin_numeric_operation_not_number () =
  try
    let _ = Builtin.eval_numeric_operation (ListSexp [Atom (Operator "+"); Atom (Num 1); Atom (Bool true)]) in
    fail "Expected Not_number exception"
  with
  | Builtin.Not_number msg ->
      Alcotest.(check string) "not number error" "Not a number." msg
  | _ -> fail "Unexpected exception type"

let test_builtin_subtract_insufficient_args () =
  try
    let _ = Builtin.eval_numeric_operation (ListSexp [Atom (Operator "-")]) in
    fail "Expected Unsupported_operation exception"
  with
  | Builtin.Unsupported_operation msg ->
      Alcotest.(check string) "subtraction insufficient args error" "Subtraction requires at least one number." msg
  | _ -> fail "Unexpected exception type"

let test_builtin_divide_by_zero () =
  try
    let _ = Builtin.eval_numeric_operation (ListSexp [Atom (Operator "/"); Atom (Num 10); Atom (Num 0)]) in
    fail "Expected Division_by_zero exception"
  with
  | Division_by_zero ->
      Alcotest.(check string) "division by zero error" "Division by zero" "Division by zero"
  | _ -> fail "Unexpected exception type"

let test_builtin_modulo_insufficient_args () =
  try
    let _ = Builtin.eval_numeric_operation (ListSexp [Atom (Operator "mod"); Atom (Num 10)]) in
    fail "Expected Unsupported_operation exception"
  with
  | Builtin.Unsupported_operation msg ->
      Alcotest.(check string) "modulo insufficient args error" "Modulo requires exactly two numbers." msg
  | _ -> fail "Unexpected exception type"

let test_builtin_modulo_too_many_args () =
  try
    let _ = Builtin.eval_numeric_operation (ListSexp [Atom (Operator "mod"); Atom (Num 10); Atom (Num 3); Atom (Num 2)]) in
    fail "Expected Unsupported_operation exception"
  with
  | Builtin.Unsupported_operation msg ->
      Alcotest.(check string) "modulo too many args error" "Modulo requires exactly two numbers." msg
  | _ -> fail "Unexpected exception type"

let test_builtin_comparison_not_number () =
  try
    let _ = Builtin.eval_comparison_operation ">" [Atom (Num 1); Atom (Bool false)] in
    fail "Expected Not_number exception"
  with
  | Builtin.Not_number msg ->
      Alcotest.(check string) "comparison not number error" "Not a number." msg
  | _ -> fail "Unexpected exception type"

let test_builtin_comparison_insufficient_args () =
  try
    let _ = Builtin.eval_comparison_operation "=" [Atom (Num 1)] in
    fail "Expected Unsupported_operation exception"
  with
  | Builtin.Unsupported_operation msg ->
      Alcotest.(check string) "comparison insufficient args error" "Comparison operations require exactly two numbers." msg
  | _ -> fail "Unexpected exception type"

let () =
  run "Builtin"
    [
      (
        "eval",
        [
          test_case "add numbers" `Quick test_builtin_add_numbers;
          test_case "subtract numbers" `Quick test_builtin_subtract_numbers;
          test_case "multipy numbers" `Quick test_builtin_multiply_numbers;
          test_case "divide numbers" `Quick test_builtin_divide_numbers;
          test_case "modulo numbers" `Quick test_builtin_modulo_numbers;
          test_case "greater than" `Quick test_builtin_greater_than;
          test_case "less than" `Quick test_builtin_less_than;
          test_case "greater than or equal" `Quick
            test_builtin_greater_than_or_equal;
          test_case "less than or equal" `Quick test_builtin_less_than_or_equal;
          test_case "equal" `Quick test_builtin_equal;
          test_case "add and multiply numbers nested" `Quick
            test_builtin_add_and_multiply_numbers_nested;
        ] );
      ("error handling and edge cases",
       [
         test_case "numeric operation with non-number" `Quick test_builtin_numeric_operation_not_number;
         test_case "subtraction with insufficient arguments" `Quick test_builtin_subtract_insufficient_args;
         test_case "division by zero" `Quick test_builtin_divide_by_zero;
         test_case "modulo with insufficient arguments" `Quick test_builtin_modulo_insufficient_args;
         test_case "modulo with too many arguments" `Quick test_builtin_modulo_too_many_args;
         test_case "comparison with non-number" `Quick test_builtin_comparison_not_number;
         test_case "comparison with insufficient arguments" `Quick test_builtin_comparison_insufficient_args;
       ]);
    ]