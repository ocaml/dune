open Stdune
open Cmdliner
open Import.Let_syntax
module Re = Dune_re

let rewrite_paths build_path_prefix_map =
  match Build_path_prefix_map.decode_map build_path_prefix_map with
  | Error msg ->
    Printf.eprintf "Cannot decode %s: %s\n%!" build_path_prefix_map msg;
    exit 2
  | Ok map ->
    let abs_path_re =
      let not_dir = Printf.sprintf " \n\r\t%c" Bin.path_sep in
      Re.(compile (seq [ char '/'; rep1 (diff any (set not_dir)) ]))
    in
    fun s ->
      Re.replace abs_path_re s ~f:(fun g ->
          Build_path_prefix_map.rewrite map (Re.Group.get g 0))

let sanitizer (command : Sanitizer.Command.t) =
  command.output |> Ansi_color.strip
  |> rewrite_paths command.build_path_prefix_map

let term =
  let+ () = Term.pure () in
  Sanitizer.impl_sanitizer sanitizer stdin stdout

let command =
  (term, Term.info "sanitize" ~doc:"Sanitize the output of shell phrases.")
