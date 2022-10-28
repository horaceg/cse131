open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Expr

let boa_max = int_of_float (2. ** 62.) - 1
let boa_min = -int_of_float (2. ** 62.)
let valid_id_regex = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let number_regex = Str.regexp "^-?[0-9]+"
let reserved_words = [ "let"; "add1"; "sub1"; "isNum"; "isBool"; "if" ]
let reserved_constants = [ "true"; "false" ]
let int_of_string_opt s = try Some (int_of_string s) with _ -> None

let maybe_id identifier =
  if List.mem identifier (reserved_constants @ reserved_words) then
    failwith @@ Printf.sprintf "Reserved keyword : %s" identifier
  else identifier

let parse_atom s =
  if Str.string_match number_regex s 0 then ENumber (int_of_string s)
  else if
    Str.string_match valid_id_regex s 0
    && (not @@ List.mem s @@ reserved_constants @ reserved_words)
  then EId s
  else
    match s with
    | "true" -> EBool true
    | "false" -> EBool false
    | _ -> failwith @@ Printf.sprintf "unrecognized atom %s" s

let rec parse (sexp : Sexp.t) =
  let parse_multiple = function
    | Atom "add1" :: [ arg ] -> EPrim1 (Add1, parse arg)
    | Atom "sub1" :: [ arg ] -> EPrim1 (Sub1, parse arg)
    | Atom "isNum" :: [ arg ] -> EPrim1 (IsNum, parse arg)
    | Atom "isBool" :: [ arg ] -> EPrim1 (IsBool, parse arg)
    | [ Atom "+"; arg1; arg2 ] -> EPrim2 (Plus, parse arg1, parse arg2)
    | [ Atom "-"; arg1; arg2 ] -> EPrim2 (Minus, parse arg1, parse arg2)
    | [ Atom "*"; arg1; arg2 ] -> EPrim2 (Times, parse arg1, parse arg2)
    | [ Atom "<"; arg1; arg2 ] -> EPrim2 (Less, parse arg1, parse arg2)
    | [ Atom ">"; arg1; arg2 ] -> EPrim2 (Greater, parse arg1, parse arg2)
    | [ Atom "=="; arg1; arg2 ] -> EPrim2 (Equal, parse arg1, parse arg2)
    | [ Atom "if"; cond; t; e ] -> EIf (parse cond, parse t, parse e)
    | Atom "let" :: List bindings :: [ e2 ] ->
        ELet (parse_bindings bindings, parse e2)
    | _ -> failwith "parser error: Invalid s-expression"
  in
  match sexp with
  | List sexps -> parse_multiple sexps
  | Sexp.Atom s -> parse_atom s

and parse_bindings bindings =
  match bindings with
  | [] -> []
  | Sexp.List bd :: q -> (
      match bd with
      | [ Sexp.Atom a; b ] -> (maybe_id a, parse b) :: parse_bindings q
      | _ -> failwith "parser error: Expected (atom, sexp) in let expression")
  | _ -> failwith "parser error: Expected Sexp list after let binding"

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
