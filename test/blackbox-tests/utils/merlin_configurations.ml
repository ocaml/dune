open Stdune
module Processed = Dune_rules.Merlin.Processed

let print file { Processed.mode; is_default; kind; counterpart; directives = _ } =
  let mode =
    match mode with
    | Dune_lang.Compilation_mode.Ocaml -> "ocaml"
    | Melange -> "melange"
  in
  Printf.printf
    "%s: %s %b %s %s\n"
    file
    mode
    is_default
    (Ocaml.Ml_kind.to_string kind)
    (Option.map counterpart ~f:Path.to_string |> Option.value ~default:"-")
;;

let () =
  Path.set_root (Path.External.cwd ());
  Path.Build.set_build_dir (Path.Outside_build_dir.of_string "_build");
  match Array.to_list Sys.argv with
  | _ :: config :: (_ :: _ as files) ->
    let config =
      match Processed.load_file (Path.of_string config) with
      | Ok config -> config
      | Error message -> failwith message
    in
    List.iter files ~f:(fun file ->
      let path = Path.Build.relative (Path.Build.of_string "default") file in
      match Processed.configurations config ~file:path with
      | None -> Printf.printf "%s: none\n" file
      | Some configurations -> Nonempty_list.iter configurations ~f:(print file))
  | _ -> failwith "usage: merlin_configurations CONFIG FILE..."
;;
