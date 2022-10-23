open Printf
open Expr
open Asm

let rec find ls x =
  match ls with
  | [] -> None
  | (y, v) :: _ when y = x -> Some v
  | _ :: rest -> find rest x

let stackloc si = RegOffset (-8 * si, RSP)

let rec compile_expr e si env =
  match e with
  | ENumber n -> [ IMov (Reg RAX, Const n) ]
  | EId x -> compile_id si env x
  | EPrim1 (op, e) -> compile_prim1 op e si env
  | EPrim2 (op, e1, e2) -> compile_prim2 op e1 e2 si env
  | _ -> failwith "not implemented"

and compile_id si env x =
  match find env x with
  | None -> failwith @@ sprintf "Unbound ID: %s" x
  | Some si -> [ IMov (Reg RAX, stackloc si) ]

and compile_prim1 op e si env =
  let arg_exprs = compile_expr e si env in
  let new_instr =
    match op with
    | Add1 -> IAdd (Reg RAX, Const 1)
    | Sub1 -> ISub (Reg RAX, Const 1)
  in
  arg_exprs @ [ new_instr ]

and compile_prim2 op e1 e2 si env =
  let arg_exprs_1 = compile_expr e1 si env in
  let arg_exprs_2 = compile_expr e2 (si + 1) env in
  let context =
    arg_exprs_1
    @ [ IMov (stackloc si, Reg RAX) ]
    @ arg_exprs_2
    @ [ IMov (stackloc (si + 1), Reg RAX); IMov (Reg RAX, stackloc si) ]
  in
  let reg_offset = stackloc (si + 1) in
  let core_instr =
    match op with
    | Plus -> IAdd (Reg RAX, reg_offset)
    | Minus -> ISub (Reg RAX, reg_offset)
    | Times -> IMul (Reg RAX, reg_offset)
  in
  context @ [ core_instr ]

let compile_to_string prog =
  let prelude =
    "section .text\n" ^ "global our_code_starts_here\n"
    ^ "our_code_starts_here:"
  in
  let compiled = compile_expr prog 1 [] in
  let as_assembly_string = to_asm (compiled @ [ IRet ]) in
  sprintf "%s%s\n" prelude as_assembly_string
