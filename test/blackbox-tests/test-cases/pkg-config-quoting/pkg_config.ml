(* We'd like to use String.equal but that's OCaml >= 4.03 *)
let not_flag x = not ("--print-errors" = x)

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let args = List.filter not_flag args in
  Format.printf "%a@."
    (Format.pp_print_list Format.pp_print_string) args
