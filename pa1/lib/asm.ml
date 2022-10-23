open Printf

type reg = RAX | RSP
type arg = Const of int | Reg of reg | RegOffset of int * reg

type instruction =
  | IMov of arg * arg
  | IAdd of arg * arg
  | ISub of arg * arg
  | IMul of arg * arg
  | ILabel of string
  | ICmp of arg * arg
  | IJne of string
  | IJmp of string
  | IRet

let count = ref 0

let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count

let r_to_asm (r : reg) : string = match r with RAX -> "rax" | RSP -> "rsp"

let arg_to_asm (a : arg) : string =
  match a with
  | Const n -> sprintf "%d" n
  | Reg r -> r_to_asm r
  | RegOffset (n, r) ->
      let reg = match r with RAX -> "rax" | RSP -> "rsp" in
      sprintf "[%s - %d]" reg n

let i_to_asm (i : instruction) : string =
  match i with
  | IMov (dest, value) ->
      sprintf "  mov %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IAdd (dest, to_add) ->
      sprintf "  add %s, %s" (arg_to_asm dest) (arg_to_asm to_add)
  | ISub (dest, to_sub) ->
      sprintf "  sub %s, %s" (arg_to_asm dest) (arg_to_asm to_sub)
  | IMul (dest, to_mul) ->
      sprintf "  mul %s, %s" (arg_to_asm dest) (arg_to_asm to_mul)
  | ICmp (left, right) ->
      (* TODO *)
      failwith "Not yet implemented"
  | ILabel name -> sprintf "%s:" name
  | IJne label ->
      (* TODO *)
      failwith "Not yet implemented"
  | IJmp label ->
      (* TODO *)
      failwith "Not yet implemented"
  | IRet -> "       ret"

let to_asm (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (i_to_asm i)) "" is
