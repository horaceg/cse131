open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Expr

let boa_max = int_of_float (2. ** 62.) - 1
let boa_min = -int_of_float (2. ** 62.)
let valid_id_regex = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let number_regex = Str.regexp "^[-]?[0-9]+"
let reserved_words = [ "let"; "add1"; "sub1"; "isNum"; "isBool"; "if" ]
let reserved_constants = [ "true"; "false" ]
let int_of_string_opt s = try Some (int_of_string s) with _ -> None

let rec parse (sexp : Sexp.t) = (* TODO *) failwith "Not yet implemented"
and parse_binding binding = (* TODO *) failwith "Not yet implemented"

let prim1_to_string prim =
  match prim with
  | Add1 -> "add1"
  | Sub1 -> "sub1"
  | IsNum -> "isNum"
  | IsBool -> "isBool"

let prim2_to_string prim =
  match prim with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Less -> "<"
  | Greater -> ">"
  | Equal -> "=="

let rec to_sexp expr =
  match expr with
  | EId s -> Sexp.Atom s
  | ENumber n -> Sexp.Atom (string_of_int n)
  | EPrim1 (prim1, e) ->
      Sexp.List [ Sexp.Atom (prim1_to_string prim1); to_sexp e ]
  | EPrim2 (prim2, e1, e2) ->
      Sexp.List [ Sexp.Atom (prim2_to_string prim2); to_sexp e1; to_sexp e2 ]
  | ELet (l, e) ->
      Sexp.List
        [
          Sexp.Atom "let";
          Sexp.List
            (List.map (fun (s, e) -> Sexp.List [ Sexp.Atom s; to_sexp e ]) l);
          to_sexp e;
        ]
  | EBool b -> Sexp.Atom (string_of_bool b)
  | EIf (cond, if_true, if_false) ->
      Sexp.List [ to_sexp cond; to_sexp if_true; to_sexp if_false ]

let pp ppf expr = Sexp.pp ppf @@ to_sexp expr
