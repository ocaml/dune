(* We'd like to use String.equal but that's OCaml >= 4.03 *)
let not_flag x = not ("--print-errors" = x)

let version () =
  try Sys.getenv "FAKE_PKGCONF_VERSION" with Not_found -> "2.4.3"

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [ "--version" ] -> print_endline (version ())
  | args ->
    let args = List.filter not_flag args in
    Format.printf "@[<v>%a@]@."
      (Format.pp_print_list Format.pp_print_string) args
