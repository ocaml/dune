(* We'd like to use String.equal but that's OCaml >= 4.03 *)
let not_flag x = not ("--print-errors" = x)

let variable_prefix = "--variable="

let variable_arg args =
  let prefix_len = String.length variable_prefix in
  let rec loop = function
    | [] -> None
    | arg :: args ->
      if String.length arg >= prefix_len
         && String.sub arg 0 prefix_len = variable_prefix
      then Some (String.sub arg prefix_len (String.length arg - prefix_len))
      else loop args
  in
  loop args
;;

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  match variable_arg args with
  | Some variable -> Printf.printf "value-for-%s\n" variable
  | None ->
    let args = List.filter not_flag args in
    Format.printf "@[<v>%a@]@."
      (Format.pp_print_list Format.pp_print_string) args
