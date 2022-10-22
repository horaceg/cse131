type prim1 = Add1 | Sub1
type prim2 = Plus | Minus | Times

type expr =
  | ELet of (string * expr) list * expr
  | EPrim1 of prim1 * expr
  | EPrim2 of prim2 * expr * expr
  | ENumber of int
  | EId of string
