open Stdune
open Cmdliner
open Import.Let_syntax
module Re = Dune_re
module Configurator = Configurator.V1

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

let make_ext_replace config =
  let tbl =
    List.filter_map [ "ext_exe"; "ext_dll"; "ext_asm"; "ext_lib"; "ext_obj" ]
      ~f:(fun var ->
        match Configurator.ocaml_config_var config var with
        | Some "" -> None
        | Some s -> Some (s, "$" ^ var)
        | None -> (
          match (var, Configurator.ocaml_config_var config "system") with
          | "ext_exe", Some "Win32" -> Some (".exe", var)
          | _ -> None ))
  in
  let re =
    Re.(
      compile
        (seq
           [ diff any (char '/')
           ; alt (List.map tbl ~f:(fun (s, _) -> str s))
           ; eow
           ]))
  in
  let map = String.Map.of_list_reduce tbl ~f:(fun _ x -> x) in
  fun s ->
    Re.replace re s ~f:(fun g ->
        let s = Re.Group.get g 0 in
        sprintf "%c%s" s.[0] (String.Map.find_exn map (String.drop s 1)))

let sanitizer =
  let ext_replace =
    if Option.is_some (Env.get Env.initial "INSIDE_DUNE") then
      make_ext_replace (Configurator.create "sanitizer")
    else
      fun s ->
    s
  in
  fun (command : Sanitizer.Command.t) ->
    command.output |> Ansi_color.strip |> ext_replace
    |> rewrite_paths command.build_path_prefix_map

let term =
  let+ () = Term.pure () in
  Sanitizer.impl_sanitizer sanitizer stdin stdout

let command =
  (term, Term.info "sanitize" ~doc:"Sanitize the output of shell phrases.")
