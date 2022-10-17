type t = Leaf | Node of string * t * t

let rec inorder_str bt =
  match bt with
  | Leaf -> ""
  | Node (s, left, right) -> inorder_str left ^ s ^ inorder_str right

let rec size bt =
  match bt with Leaf -> 0 | Node (_, l, r) -> 1 + size l + size r

let rec height bt =
  match bt with Leaf -> 0 | Node (_, l, r) -> 1 + max (height l) (height r)
