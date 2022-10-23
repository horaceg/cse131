open Printf
open Expr
open Asm

let rec find ls x =
  match ls with
  | [] -> None
  | (y, v) :: _ when y = x -> Some v
  | _ :: rest -> find rest x

let stackloc si = RegOffset (-8 * si, RSP)

let compile_id si env x =
  match find env x with
  | None -> failwith @@ sprintf "Unbound ID: %s" x
  | Some si -> [ IMov (Reg RAX, stackloc si) ]

let rec compile_expr si env e =
  match e with
  | ENumber n -> [ IMov (Reg RAX, Const n) ]
  | EId x -> compile_id si env x
  | EPrim1 (op, e) -> compile_prim1 si env op e
  | EPrim2 (op, e1, e2) -> compile_prim2 si env op e1 e2
  | ELet (l, e) -> compile_let si env l e

and compile_let si env l post_expr =
  let rec bind acc env ll =
    match ll with
    | [] -> (env, acc)
    | (var, e) :: t ->
        let content = compile_expr si env e in
        let binding = IMov (stackloc si, Reg RAX) in
        bind (acc @ content @ [ binding ]) ((var, si) :: env) t
  in
  let newenv, bindings = bind [] env l in
  bindings @ compile_expr (si + 1) newenv post_expr

and compile_prim1 si env op e =
  let arg_exprs = compile_expr si env e in
  let new_instr =
    match op with
    | Add1 -> IAdd (Reg RAX, Const 1)
    | Sub1 -> ISub (Reg RAX, Const 1)
  in
  arg_exprs @ [ new_instr ]

and compile_prim2 si env op e1 e2 =
  let arg_exprs_1 = compile_expr si env e1 in
  let arg_exprs_2 = compile_expr (si + 1) env e2 in
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
  let compiled = compile_expr 1 [] prog in
  let as_assembly_string = to_asm (compiled @ [ IRet ]) in
  sprintf "%s%s\n" prelude as_assembly_string
