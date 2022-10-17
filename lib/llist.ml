let rec increment_all = function
  | [] -> []
  | h :: q -> (h + 1) :: increment_all q

let rec long_strings i l =
  match l with
  | [] -> []
  | h :: q ->
      if String.length h > i then h :: long_strings i q else long_strings i q

