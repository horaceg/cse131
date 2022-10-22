open Pa1
open Pa1.Runner
open Pa1.Expr
open Printf
open OUnit2

let t name program expected = name >:: test_run program name expected

let t_parse name program expected =
  name >:: fun _ -> assert_equal expected (Runner.parse_string program)

(* For folding *)
let t_f test_type (name, program, expected) = test_type name program expected
let t_err name program expected = name >:: test_err program name expected

let t_parse_err name program expected =
  name >:: test_parse_err program name expected

let f_to_s fname = Runner.string_of_file ("input/" ^ fname)
let forty_one = "(sub1 42)"
let forty = "(sub1 (sub1 42))"
let add1 = "(add1 (add1 (add1 3)))"
let def_x = "(let ((x 5)) x)"
let def_x2 = "(let ((x 5)) (sub1 x))"
let def_x3 = "(let ((x 5)) (let ((x 67)) (sub1 x)))"
let def_x4 = "(let ((x (let ((x 5)) (sub1 x)))) (sub1 x))"
let addnums = "(+ 5 10)"
let nested_add = "(+ 5 (+ 10 20))"
let nested_add2 = "(+ (- 10 5) 20)"
let nested_arith = "(- (* (- 54 3) 2) 102)"
let let_nested = "(let ((x (+ 5 (+ 10 20)))) (* x x))"
let failLet = "(let ((x  1) (y 1) (x 10)) x)"
let failID = "x"
let forty_one_p = EPrim1 (Sub1, ENumber 42)

let testFailList =
  [
    t_err "failLet" failLet "Compile error: Duplicate binding";
    t_err "failID" failID "Compile error: Unbound variable identifier x";
  ]

let suite =
  "suite"
  >::: [ t_parse "forty_one parse" forty_one forty_one_p ]
       @ [
           t "forty_one" forty_one "41";
           t "forty" forty "40";
           t "add1" add1 "6";
           t "def_x" def_x "5";
           t "def_x2" def_x2 "4";
           t "def_x3" def_x3 "66";
           t "def_x4" def_x4 "3";
           t "addnums" addnums "15";
           t "nested_add" nested_add "35";
           t "nested_add2" nested_add2 "25";
           t "nested_arith" nested_arith "0";
           t "let_nested" let_nested "1225";
         ]
       @ testFailList @ MyTests.myTestList

let () = run_test_tt_main suite
