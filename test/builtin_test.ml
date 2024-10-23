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

let () =
  run "Builtin"
    [
      ( "eval",
        [
          test_case "add numbers" `Quick test_builtin_add_numbers;
          test_case "subtract numbers" `Quick test_builtin_subtract_numbers;
          test_case "multipy numbers" `Quick test_builtin_multiply_numbers;
          test_case "divide numbers" `Quick test_builtin_divide_numbers;
          test_case "add and multiply numbers nested" `Quick
            test_builtin_add_and_multiply_numbers_nested;
        ] );
    ]
