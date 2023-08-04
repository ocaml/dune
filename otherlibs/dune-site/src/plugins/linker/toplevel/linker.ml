(*
   Prior to OCaml 4.13.0, [load_file] was in the Topdirs module.
   Beginning with OCaml 4.13.0, load_file is in the Toploop module.
   In order to be able to compile with OCaml versions either
   before or after, open both modules and let the compiler
   find [load_file] where it is defined.
*)
open Topdirs [@@ocaml.warning "-33"]
open Toploop [@@ocaml.warning "-33"]

let load filename =
  let buf = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buf in
  match load_file ppf filename with
  | true -> ()
  | false ->
    Format.pp_print_flush ppf ();
    failwith
    @@ Format.asprintf "Failed to load file `%s': %s" filename (Buffer.contents buf)
;;
