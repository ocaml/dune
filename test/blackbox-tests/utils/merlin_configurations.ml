open Stdune
module Processed = Dune_rules.Merlin.Processed

let rec sexp_to_json = function
  | Sexp.Atom atom -> Json.string atom
  | Sexp.List items -> Json.list (List.map items ~f:sexp_to_json)
;;

let print ~json file { Processed.mode; is_default; kind; counterpart; directives } =
  let mode =
    match mode with
    | Dune_lang.Compilation_mode.Ocaml -> "ocaml"
    | Melange -> "melange"
  in
  let kind = Ocaml.Ml_kind.to_string kind in
  let counterpart = Option.map counterpart ~f:Path.to_string in
  if json
  then
    Json.assoc
      [ "file", Json.string file
      ; "mode", Json.string mode
      ; "is_default", Json.bool is_default
      ; "kind", Json.string kind
      ; ( "counterpart"
        , Option.map counterpart ~f:Json.string |> Option.value ~default:`Null )
      ; "directives", sexp_to_json directives
      ]
    |> Json.to_string
    |> print_endline
  else
    Printf.printf
      "%s: %s %b %s %s\n"
      file
      mode
      is_default
      kind
      (Option.value counterpart ~default:"-")
;;

let () =
  Path.set_root (Path.External.cwd ());
  Path.Build.set_build_dir (Path.Outside_build_dir.of_string "_build");
  let json, args =
    match List.tl (Array.to_list Sys.argv) with
    | "--json" :: args -> true, args
    | args -> false, args
  in
  match args with
  | config :: (_ :: _ as files) ->
    let config =
      match Processed.load_file (Path.of_string config) with
      | Ok config -> config
      | Error message -> failwith message
    in
    List.iter files ~f:(fun file ->
      let path = Path.Build.relative (Path.Build.of_string "default") file in
      match Processed.configurations config ~file:path with
      | None -> if not json then Printf.printf "%s: none\n" file
      | Some configurations -> Nonempty_list.iter configurations ~f:(print ~json file))
  | _ -> failwith "usage: merlin_configurations [--json] CONFIG FILE..."
;;
