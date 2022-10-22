module Sexp = Sexplib.Sexp
open Expr

(* Defines rules for what ids are valid -- ids must match the regex and not
 * be a reserved word *)
let reserved_words = [ "let"; "add1"; "sub1" ]

(* Converts a string to Some int or None if not convertable *)
let int_of_string_opt s = try Some (int_of_string s) with _ -> None

let rec parse sexp = (* TODO *) failwith "Not yet implemented"
and parse_binding binding = (* TODO *) failwith "Not yet implemented"
