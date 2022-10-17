(* module Sexp = struct
  type t = List of t list | Atom of string
  (*
     let parse l =
       let rec aux acc ll =
         match ll with
         | [] -> List acc
         | ")" :: q ->
             let parsed = List acc in
             List ([ parsed ] @ aux [] q)
         | "(" :: q -> aux [] q
       in

       match l with [] -> None | _ -> aux [] l *)

  let to_string sxp =
    let rec aux depth s =
      match s with
      | Atom a -> " " ^ a ^ " "
      | List l ->
          let rem =
            "\n " ^ String.concat " " ^ "(" ^ List.fold_left ( ^ ) ""
            @@ List.map to_string l
          in
          rem ^ ")"
    in
    aux 0 sxp

  let print sxp = print_endline @@ to_string sxp

  let example =
    List [ Atom "+"; Atom "39"; List [ Atom "-"; Atom "5"; Atom "4" ] ]
end

let () = Sexp.(print example) *)
