open Pa1.Compile
open Pa1.Runner
open Printf
open Lexing

let () =
  let name = Sys.argv.(1) in
  let input_file = open_in name in
  let program = compile_file_to_string input_file in
  printf "%s\n" program;;

