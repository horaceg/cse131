module Sexp = Sexplib.Sexp
open Expr

(* Defines rules for what ids are valid -- ids must match the regex and not
 * be a reserved word *)
let reserved_words = [ "let"; "add1"; "sub1" ]

(* Converts a string to Some int or None if not convertable *)
let int_of_string_opt s = try Some (int_of_string s) with _ -> None

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
    | _ -> failwith "parser error: Unrecognized instruction"
  in
  match sexp with
  | Sexp.Atom s -> (
      match int_of_string_opt s with None -> EId s | Some i -> ENumber i)
  | List sexps -> aux sexps

and parse_bindings bindings =
  match bindings with
  | [] -> []
  | Sexp.List bd :: q ->
      (match bd with
      | [] -> []
      | Sexp.Atom a :: b :: rest -> (a, parse b) :: parse_bindings rest
      | _ -> failwith "parser error")
      @ parse_bindings q
  | _ -> failwith "parser error: Expected Sexp list after let binding"
