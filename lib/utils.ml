module Btree = struct
  type t = Leaf | Node of string * t * t

  let rec inorder_str bt =
    match bt with
    | Leaf -> ""
    | Node (s, left, right) -> inorder_str left ^ s ^ inorder_str right

  let rec size bt =
    match bt with Leaf -> 0 | Node (_, l, r) -> 1 + size l + size r

  let rec height bt =
    match bt with Leaf -> 0 | Node (_, l, r) -> 1 + max (height l) (height r)
end

module Llist = struct
  let rec increment_all = function
    | [] -> []
    | h :: q -> (h + 1) :: increment_all q

  let rec long_strings i l =
    match l with
    | [] -> []
    | h :: q ->
        if String.length h > i then h :: long_strings i q else long_strings i q

  let every_other l =
    let rec aux take ll =
      match ll with
      | [] -> []
      | h :: q -> if take then h :: aux (not take) q else aux (not take) q
    in
    aux true l

  let rec sum_all l =
    match l with [] -> [] | h :: q -> List.fold_left ( + ) 0 h :: sum_all q
end

module Tup = struct
  let rec sum_of_squares = function
    | [] -> 0
    | (a, b) :: q -> (a * a) + (b * b) + sum_of_squares q

  let rec remainders divider = function
    | [] -> []
    | h :: q -> (h / divider, h mod divider) :: remainders divider q
end

module Opt = struct
  let mean l =
    let rec aux acc len = function
      | [] -> acc / len
      | h :: q -> aux (acc + h) (len + 1) q
    in
    match l with [] -> None | l -> Some (aux 0 0 l)

  let list_max l =
    let rec aux ll =
      match ll with [] -> Int.min_int | h :: q -> max h @@ aux q
    in
    match l with [] -> None | _ -> Some (aux l)
end
