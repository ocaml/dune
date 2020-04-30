open Stdune
open Cmdliner
open Import.Let_syntax
module Re = Dune_re
module Configurator = Configurator.V1

let rewrite_paths =
  let var = "BUILD_PATH_PREFIX_MAP" in
  match Sys.getenv var with
  | exception Not_found -> fun s -> s
  | s -> (
    match Build_path_prefix_map.decode_map s with
    | Error msg ->
      Printf.eprintf "Cannot decode %s: %s\n%!" var msg;
      exit 2
    | Ok map ->
      let abs_path_re =
        let not_dir = Printf.sprintf " \n\r\t%c" Bin.path_sep in
        Re.(compile (seq [ char '/'; rep1 (diff any (set not_dir)) ]))
      in
      fun s ->
        Re.replace abs_path_re s ~f:(fun g ->
            Build_path_prefix_map.rewrite map (Re.Group.get g 0)) )

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

let term =
  let+ exit_code =
    Arg.(
      value & opt int 0
      & info [ "exit-code" ] ~docv:"NUM"
          ~doc:"Exit code of the shell command we are sanitizing the output of.")
  and+ file =
    Arg.(value & pos 0 (some string) None (Arg.info [] ~docv:"FILE"))
  in
  let ext_replace =
    if Option.is_some (Env.get Env.initial "INSIDE_DUNE") then
      make_ext_replace (Configurator.create "sanitizer")
    else
      fun s ->
    s
  in
  let sanitize s = s |> Ansi_color.strip |> ext_replace |> rewrite_paths in
  ( match file with
  | Some fn ->
    Io.String_path.read_file fn
    |> sanitize |> String.split_lines
    |> List.iter ~f:(fun line -> Printf.printf "  %s\n" line)
  | None -> (
    let isatty = Unix.(isatty stdout) in
    try
      while true do
        let line = input_line stdin in
        Printf.printf "  %s\n" (sanitize line);
        if isatty then flush stdout
      done
    with End_of_file -> () ) );
  if exit_code <> 0 then Printf.printf "  [%d]\n" exit_code

let command =
  (term, Term.info "sanitize" ~doc:"Sanitize the output of shell phrases.")
