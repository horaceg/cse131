open Unix
open Filename
open Str
open Compile
open Printf
open ExtLib

type ('a, 'b) either = Left of 'a | Right of 'b

let either_printer e =
  match e with Left v -> sprintf "Error: %s\n" v | Right v -> v

let parse_string s =
  let sexp = Sexplib.Sexp.of_string s in
  Parser.parse sexp

let parse_file input_file =
  let sexp = Sexplib.Sexp.input_sexp input_file in
  Parser.parse sexp

let compile_file_to_string input_file =
  let input_program = parse_file input_file in
  compile_to_string input_program

let compile_string_to_string s =
  let input_program = parse_string s in
  compile_to_string input_program

let make_tmpfiles name =
  let null_stdin, _ = pipe () in
  let stdout_name = temp_file ("stdout_" ^ name) ".out" in
  let stdin_name = temp_file ("stderr_" ^ name) ".err" in
  ( openfile stdout_name [ O_RDWR ] 0o600,
    stdout_name,
    openfile stdin_name [ O_RDWR ] 0o600,
    stdin_name,
    null_stdin )

(* Read a file into a string *)
let string_of_file file_name =
  let inchan = open_in file_name in
  let buf = Bytes.create (in_channel_length inchan) in
  really_input inchan buf 0 (in_channel_length inchan);
  Bytes.to_string buf

let run p outdir outfile =
  let out = outdir ^ "/" ^ outfile in
  let maybe_asm_string =
    try Right (compile_to_string p)
    with Failure s -> Left ("Compile error: " ^ s)
  in
  match maybe_asm_string with
  | Left s -> Left s
  | Right asm_string ->
      let outfile = open_out (out ^ ".s") in
      fprintf outfile "%s" asm_string;
      close_out outfile;
      let bstdout, bstdout_name, bstderr, bstderr_name, bstdin =
        make_tmpfiles "build"
      in
      let rstdout, rstdout_name, rstderr, rstderr_name, rstdin =
        make_tmpfiles "build"
      in
      let built_pid =
        Unix.create_process "make"
          (Array.of_list [ ""; out ^ ".run" ])
          bstdin bstdout bstderr
      in
      let _, status = waitpid [] built_pid in

      let try_running =
        match status with
        | WEXITED 0 -> Right (string_of_file rstdout_name)
        | WEXITED _ ->
            Left
              (sprintf "Finished with error while building %s:\n%s" out
                 (string_of_file bstderr_name))
        | WSIGNALED n ->
            Left (sprintf "Signalled with %d while building %s." n out)
        | WSTOPPED n ->
            Left (sprintf "Stopped with signal %d while building %s." n out)
      in

      let result =
        match try_running with
        | Left _ -> try_running
        | Right msg -> (
            printf "%s" msg;
            let ran_pid =
              Unix.create_process
                ("./" ^ out ^ ".run")
                (Array.of_list []) rstdin rstdout rstderr
            in
            let _, status = waitpid [] ran_pid in
            match status with
            | WEXITED 0 -> Right (string_of_file rstdout_name)
            | WEXITED n ->
                Left (sprintf "Error %d: %s" n (string_of_file rstdout_name))
            | WSIGNALED n ->
                Left (sprintf "Signalled with %d while running %s." n out)
            | WSTOPPED n ->
                Left (sprintf "Stopped with signal %d while running %s." n out))
      in

      List.iter close [ bstdout; bstderr; bstdin; rstdout; rstderr; rstdin ];
      List.iter unlink
        [ bstdout_name; bstderr_name; rstdout_name; rstderr_name ];
      result

let either_pp ppf e = either_printer e |> print_string

let test_run program_str outfile expected _ =
  let program = parse_string program_str in
  let result = run program "output" outfile in
  Alcotest.(check @@ testable either_pp ( = ))
    program_str
    (Right (expected ^ "\n"))
    result

let compare_programs check result =
  match (check, result) with
  | Left expect_msg, Left actual_message ->
      String.exists actual_message expect_msg
  | _ -> false

let test_err program_str outfile errmsg _ =
  let program = parse_string program_str in
  let result = run program "output" outfile in
  Alcotest.(check @@ testable either_pp compare_programs)
    program_str (Left errmsg) result

let try_parse program_str =
  try Right (parse_string program_str)
  with Failure errmsg -> Left ("Parse error: " ^ errmsg)

let either_parse_printer e =
  match e with Left s -> "Error: " ^ s ^ "\n" | Right _ -> "AST\n"

let either_parse_pp ppf e = either_parse_printer e |> print_string

let test_parse_err program_str outfile errmsg _ =
  let result = try_parse program_str in
  Alcotest.(check @@ testable either_parse_pp compare_programs)
    program_str (Left errmsg) result
