module Btree = struct
  open Cse131.Btree

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
  open Cse131.Llist

  let check_increment_all () =
    increment_all [ 1; 2; 3 ]
    |> Alcotest.(check @@ list int) "increment all" [ 2; 3; 4 ]

  let check_long_strings () =
    long_strings 2 [ "aa"; "aaa"; "a" ]
    |> Alcotest.(check @@ list string) "long strings" [ "aaa" ]
end

let () =
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
        ] );
    ]
