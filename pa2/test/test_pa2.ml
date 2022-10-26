open Pa2
open Pa2.Runner
open Pa2.Expr
open Printf
module Sexp = Sexplib.Sexp

let expr_testable = Alcotest.testable Parser.pp ( = )
let parse_string s = Parser.parse @@ Sexp.of_string s

let parse_one name expected inputs () =
  Alcotest.check expr_testable name expected @@ parse_string inputs

let parse_case name expected inputs =
  Alcotest.test_case name `Quick @@ parse_one name expected inputs

let t name program expected =
  Alcotest.test_case program `Slow @@ test_run program name expected []

let t_i name program expected args =
  Alcotest.test_case program `Slow @@ test_run program name expected args

(* For folding *)
let t_f test_type (name, program, expected) = test_type name program expected

let t_err name program expected =
  Alcotest.test_case name `Slow @@ test_err program name expected []

let terr_i name program expected args =
  Alcotest.test_case name `Slow @@ test_err program name expected args

let t_parse_err name program expected =
  Alcotest.test_case name `Slow @@ test_parse_err program name expected

let f_to_s fname = Runner.string_of_file ("input/" ^ fname)
let forty_one = "(sub1 42)"
let forty_one_expr = EPrim1 (Sub1, ENumber 42)
let forty = "(sub1 (sub1 42))"
let forty_expr = EPrim1 (Sub1, EPrim1 (Sub1, ENumber 42))
let add1 = "(add1 (add1 (add1 3)))"
let add1_expr = EPrim1 (Add1, EPrim1 (Add1, EPrim1 (Add1, ENumber 3)))
let def_x = "(let ((x 5)) x)"
let x_expr = ELet ([ ("x", ENumber 5) ], EId "x")
let def_x2 = "(let ((x 5)) (sub1 x))"
let x2_expr = ELet ([ ("x", ENumber 5) ], EPrim1 (Sub1, EId "x"))
let def_x3 = "(let ((x 5)) (let ((x 67)) (sub1 x)))"

let x3_expr =
  ELet
    ([ ("x", ENumber 5) ], ELet ([ ("x", ENumber 67) ], EPrim1 (Sub1, EId "x")))

let def_x4 = "(let ((x (let ((x 5)) (sub1 x)))) (sub1 x))"

let x4_expr =
  ELet
    ( [ ("x", ELet ([ ("x", ENumber 5) ], EPrim1 (Sub1, EId "x"))) ],
      EPrim1 (Sub1, EId "x") )

let def_xy = "(let ((x 5) (y 6)) (+ x y))"

let xy_expr =
  ELet ([ ("x", ENumber 5); ("y", ENumber 6) ], EPrim2 (Plus, EId "x", EId "y"))

let addnums = "(+ 5 10)"
let addnums_expr = EPrim2 (Plus, ENumber 5, ENumber 10)
let nested_add = "(+ 5 (+ 10 20))"

let nested_add_expr =
  EPrim2 (Plus, ENumber 5, EPrim2 (Plus, ENumber 10, ENumber 20))

let nested_add2 = "(+ (- 10 5) 20)"

let nested_add2_expr =
  EPrim2 (Plus, EPrim2 (Minus, ENumber 10, ENumber 5), ENumber 20)

let nested_arith = "(- (* (- 54 3) 2) 102)"

let nested_arith_expr =
  EPrim2
    ( Minus,
      EPrim2 (Times, EPrim2 (Minus, ENumber 54, ENumber 3), ENumber 2),
      ENumber 102 )

let let_nested = "(let ((x (+ 5 (+ 10 20)))) (* x x))"

let let_nested_expr =
  ELet
    ( [ ("x", EPrim2 (Plus, ENumber 5, EPrim2 (Plus, ENumber 10, ENumber 20))) ],
      EPrim2 (Times, EId "x", EId "x") )

let let_empty = "(let () 5)"
let let_empty_expr = ELet ([], ENumber 5)

let complexExpression =
  "(let ((x 10) (y 5) (z 3)) (let ((t 2)) "
  ^ "(add1 (+ x (+ y (* (- t z) x))))))"

let ifTest = "(if true 5 6)"
let ifTestLet = "(let ((x 5)) (if (== x 7) 7 8))"
let boolTest = "true"
let isBoolTest = "(isBool false)"
let isBoolTestF = "(isBool 5)"
let isNumTest = "(isNum 5)"
let num_p_overflow = "4611686018427387904"
let num_p_underflow = "-4611686018427387905"
let failLet = "(let ((x  1) (y 1) (x 10)) x)"
let failID = "x"
let failTypes = "(add1 true)"

let testFailList =
  [
    t_err "failLet" failLet "Multiple bindings for variable identifier x";
    t_err "failID" failID "Variable identifier x unbound";
    t_err "failTypes" failTypes "expected a number";
    t_err "parserNumOverflow" num_p_overflow "Non-representable number";
    t_err "parserNumUnderflow" num_p_underflow "Non-representable number";
    terr_i "failInput" "input" "input must be a boolean or a number" [ "0r" ];
    terr_i "failInputType" "(add1 input)" "expected a number" [ "true" ];
  ]

let input_tests =
  [
    t_i "input1" "input" "42" [ "42" ];
    t_i "input2" "input" "true" [ "true" ];
    t_i "input3" "input" "false" [ "false" ];
    t_i "input_default" "input" "false" [];
    t_i "input_shadow" "(let ((input 10)) input)" "10" [ "true" ];
    terr_i "inputerr1" "input" "input must be a boolean or a number" [ "ABC" ];
    terr_i "inputerr_max" "input" "input is not a representable number"
      [ "4611686018427387904" ];
    terr_i "inputerr_min" "input" "input is not a representable number"
      [ "-4611686018427387905" ];
    terr_i "inputerr_case" "input" "input must be a boolean or a number"
      [ "False" ];
  ]

let suite =
  [
    t "forty_one" forty_one "41";
    t "forty" forty "40";
    t "add1" add1 "6";
    t "addnums" addnums "15";
    t "nested_add" nested_add "35";
    t "nested_add2" nested_add2 "25";
    t "mul" "(* 5 2)" "10";
    t "nested_arith" nested_arith "0";
    t "def_x" def_x "5";
    t "def_x2" def_x2 "4";
    t "def_x3" def_x3 "66";
    t "def_x4" def_x4 "3";
    t "def_xy" def_xy "11";
    t "let_nested" let_nested "1225";
  ]
  @ testFailList
(* @ MyTests.myTestList *)

let parse_suite =
  [
    parse_case forty_one forty_one_expr forty_one;
    parse_case forty forty_expr forty;
    parse_case add1 add1_expr add1;
    parse_case def_x x_expr def_x;
    parse_case def_x2 x2_expr def_x2;
    parse_case def_x3 x3_expr def_x3;
    parse_case def_x4 x4_expr def_x4;
    parse_case def_xy xy_expr def_xy;
    parse_case addnums addnums_expr addnums;
    parse_case nested_add nested_add_expr nested_add;
    parse_case nested_add2 nested_add2_expr nested_add2;
    parse_case nested_arith nested_arith_expr nested_arith;
    parse_case let_nested let_nested_expr let_nested;
    parse_case let_empty let_empty_expr let_empty;
  ]

let letlet = "(let (let 5) let)"

let () =
  Sys.chdir "../../..";
  Alcotest.run "Parsing" [ ("parse", parse_suite); ("compile", suite) ]
