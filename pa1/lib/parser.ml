module Sexp = Sexplib.Sexp
open Expr

(* Defines rules for what ids are valid -- ids must match the regex and not
 * be a reserved word *)
let reserved_words = [ "let"; "add1"; "sub1" ]

(* Converts a string to Some int or None if not convertable *)
let int_of_string_opt s = try Some (int_of_string s) with _ -> None

let maybe_id identifier =
  if List.mem identifier reserved_words then failwith "Reserved keyword"
  else identifier

let rec parse sexp =
  let aux sxps =
    match sxps with
    | Sexp.Atom "add1" :: [ arg ] -> EPrim1 (Add1, parse arg)
    | Sexp.Atom "sub1" :: [ arg ] -> EPrim1 (Sub1, parse arg)
    | [ Sexp.Atom "+"; arg1; arg2 ] -> EPrim2 (Plus, parse arg1, parse arg2)
    | [ Sexp.Atom "-"; arg1; arg2 ] -> EPrim2 (Minus, parse arg1, parse arg2)
    | [ Sexp.Atom "*"; arg1; arg2 ] -> EPrim2 (Times, parse arg1, parse arg2)
    | Sexp.Atom "let" :: Sexp.List bindings :: [ e2 ] ->
        ELet (parse_bindings bindings, parse e2)
    | _ -> failwith "parser error: Invalid s-expression"
  in
  match sexp with
  | Sexp.Atom s -> (
      match int_of_string_opt s with None -> EId s | Some i -> ENumber i)
  | List sexps -> aux sexps

and parse_bindings bindings =
  match bindings with
  | [] -> []
  | Sexp.List bd :: q -> (
      match bd with
      | [ Sexp.Atom a; b ] -> (maybe_id a, parse b) :: parse_bindings q
      | _ -> failwith "parser error: Expected (atom, sexp) in let expression")
  | _ -> failwith "parser error: Expected Sexp list after let binding"

let prim1_to_string prim = match prim with Add1 -> "add1" | Sub1 -> "sub1"

let prim2_to_string prim =
  match prim with Plus -> "+" | Minus -> "-" | Times -> "*"

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

let pp ppf expr = Sexp.pp ppf @@ to_sexp expr
