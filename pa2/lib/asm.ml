open Printf

type reg = RAX | RBX | RDI | RSP
type size = DWORD_PTR | WORD_PTR | BYTE_PTR

type arg =
  | Const of int
  | Const64 of int64
  | HexConst of int64
  | Reg of reg
  | RegOffset of int * reg
  | Sized of size * arg

type instruction =
  | IMov of arg * arg
  | IAdd of arg * arg
  | ISub of arg * arg
  | IMul of arg * arg
  | IShr of arg * arg
  | ISar of arg * arg
  | IShl of arg * arg
  | IAnd of arg * arg
  | IOr of arg * arg
  | IXor of arg * arg
  | ILabel of string
  | IPush of arg
  | IPop of arg
  | ICall of string
  | IRet
  | ICmp of arg * arg
  | IJne of string
  | IJe of string
  | IJmp of string
  | IJno of string
  | IJl of string
  | IJg of string
  | IJo of string

let count = ref 0

let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count

let r_to_asm (r : reg) : string =
  match r with RAX -> "rax" | RBX -> "rbx" | RDI -> "rdi" | RSP -> "rsp"

let s_to_asm (s : size) : string =
  match s with DWORD_PTR -> "DWORD" | WORD_PTR -> "WORD" | BYTE_PTR -> "BYTE"

let rec arg_to_asm (a : arg) : string =
  match a with
  | Const n -> sprintf "%d" n
  | Const64 n -> sprintf "%Ld" n
  | HexConst n -> sprintf "%#Lx" n
  | Reg r -> r_to_asm r
  | RegOffset (n, r) ->
      (* TODO *)
      failwith "Not yet implemented"
  | Sized (s, a) -> s_to_asm s ^ " " ^ arg_to_asm a

let i_to_asm (i : instruction) : string =
  match i with
  | IMov (dest, value) ->
      sprintf "  mov %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IAdd (dest, to_add) ->
      sprintf "  add %s, %s" (arg_to_asm dest) (arg_to_asm to_add)
  | ISub (dest, to_sub) ->
      sprintf "  sub %s, %s" (arg_to_asm dest) (arg_to_asm to_sub)
  | IMul (dest, to_mul) ->
      sprintf "  imul %s, %s" (arg_to_asm dest) (arg_to_asm to_mul)
  | IAnd (dest, mask) ->
      sprintf "  and %s, %s" (arg_to_asm dest) (arg_to_asm mask)
  | IOr (dest, mask) ->
      (* TODO *)
      failwith "Not yet implemented"
  | IXor (dest, mask) ->
      (* TODO *)
      failwith "Not yet implemented"
  | IShr (dest, to_shift) ->
      (* TODO *)
      failwith "Not yet implemented"
  | ISar (dest, to_shift) ->
      (* TODO *)
      failwith "Not yet implemented"
  | IShl (dest, to_shift) ->
      (* TODO *)
      failwith "Not yet implemented"
  | ICmp (left, right) ->
      sprintf "  cmp %s, %s" (arg_to_asm left) (arg_to_asm right)
  | IPush arg ->
      (* TODO *)
      failwith "Not yet implemented"
  | IPop arg ->
      (* TODO *)
      failwith "Not yet implemented"
  | ICall str ->
      (* TODO *)
      failwith "Not yet implemented"
  | ILabel name -> name ^ ":"
  | IJne label -> sprintf "  jne near %s" label
  | IJe label -> sprintf "  je near %s" label
  | IJno label -> sprintf "  jno near %s" label
  | IJo label -> sprintf "  jo near %s" label
  | IJl label -> sprintf "  jl near %s" label
  | IJg label -> sprintf "  jg near %s" label
  | IJmp label -> sprintf "  jmp near %s" label
  | IRet -> "  ret"

let to_asm (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (i_to_asm i)) "" is
