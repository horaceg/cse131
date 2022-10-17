module Btree = struct
  open Cse131.Utils.Btree

  let check_leaf () = inorder_str @@ Leaf |> Alcotest.(check string) "leaf" ""

  let check_one () =
    inorder_str @@ Node ("a", Leaf, Leaf) |> Alcotest.(check string) "one" "a"

  let check_two () =
    inorder_str @@ Node ("a", Node ("b", Leaf, Leaf), Leaf)
    |> Alcotest.(check string) "two left" "ba"

  let check_two_bis () =
    inorder_str @@ Node ("a", Leaf, Node ("b", Leaf, Leaf))
    |> Alcotest.(check string) "two right" "ab"

  let check_three () =
    inorder_str @@ Node ("a", Node ("b", Leaf, Leaf), Node ("c", Leaf, Leaf))
    |> Alcotest.(check string) "three" "bac"

  let check_size () =
    size @@ Node ("a", Node ("b", Leaf, Leaf), Leaf)
    |> Alcotest.(check int) "size" 2

  let check_height () =
    size @@ Node ("a", Node ("b", Leaf, Leaf), Leaf)
    |> Alcotest.(check int) "height" 2
end

module Llist = struct
  open Cse131.Utils.Llist

  let check_increment_all () =
    increment_all [ 1; 2; 3 ]
    |> Alcotest.(check @@ list int) "increment all" [ 2; 3; 4 ]

  let check_long_strings () =
    long_strings 2 [ "aa"; "aaa"; "a" ]
    |> Alcotest.(check @@ list string) "long strings" [ "aaa" ]

  let check_every_other () =
    every_other [ 1; 2; 3; 4 ]
    |> Alcotest.(check @@ list int) "every other" [ 1; 3 ]

  let check_sum_all () =
    sum_all [ [ 1; 2 ]; [ 3; 4 ] ]
    |> Alcotest.(check @@ list int) "sum all" [ 3; 7 ]
end

module Tup = struct
  open Cse131.Utils.Tup

  let check_sum_squares () =
    sum_of_squares [ (1, 1); (2, 3) ]
    |> Alcotest.(check int) "sum of squares" 15

  let check_remainders () =
    remainders 3 [ 4; 6; 11 ]
    |> Alcotest.(check @@ list @@ pair int int)
         "remainders"
         [ (1, 1); (2, 0); (3, 2) ]
end

module Opt = struct
  open Cse131.Utils.Opt

  let check_mean () =
    mean [ 1; 2; 3 ] |> Alcotest.(check @@ option int) "mean" @@ Some 2

  let check_mean_empty () =
    mean [] |> Alcotest.(check @@ option int) "mean empty" None

  let check_list_max () =
    list_max [ 1; 2; 3 ] |> Alcotest.(check @@ option int) "list max" @@ Some 3

  let check_list_max_empty () =
    list_max [] |> Alcotest.(check @@ option int) "list max empty" @@ None
end

let main () =
  let open Alcotest in
  run "Utils"
    [
      ( "btree",
        [
          test_case "leaf" `Quick Btree.check_leaf;
          test_case "one" `Quick Btree.check_one;
          test_case "two left" `Quick Btree.check_two;
          test_case "two right" `Quick Btree.check_two_bis;
          test_case "three" `Quick Btree.check_three;
          test_case "size" `Quick Btree.check_size;
          test_case "height" `Quick Btree.check_height;
        ] );
      ( "llist",
        [
          test_case "increment all" `Quick Llist.check_increment_all;
          test_case "long strings" `Quick Llist.check_long_strings;
          test_case "every other" `Quick Llist.check_every_other;
          test_case "sum all" `Quick Llist.check_sum_all;
        ] );
      ( "tuples",
        [
          test_case "sum squares" `Quick Tup.check_sum_squares;
          test_case "remainders" `Quick Tup.check_remainders;
        ] );
      ( "Option",
        [
          test_case "mean" `Quick Opt.check_mean;
          test_case "mean empty" `Quick Opt.check_mean_empty;
          test_case "list max" `Quick Opt.check_list_max;
          test_case "list max empty" `Quick Opt.check_list_max_empty;
        ] );
    ]
