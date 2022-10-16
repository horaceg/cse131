let test_lowercase () =
  Alcotest.(check string) "ldskjdals" "hello!" (String.lowercase_ascii "hEllo!")

let check_fun _ = Alcotest.(check int) "fdkh" (2 + 2) 4
let check_fibonacci () = Alcotest.(check int) "fibo" (Cse131.Fibo.fibonacci 4) 5
let check_max () = Alcotest.(check int) "max" (Cse131.Max.max 3 4) 4

let () =
  let open Alcotest in
  run "Utils"
    [
      ( "string-case",
        [
          test_case "Lower case" `Quick test_lowercase;
          test_case "lfdkjf" `Quick check_fun;
          test_case "fib" `Quick check_fibonacci;
          test_case "max" `Quick check_max;
        ] );
    ]
