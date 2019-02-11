open Import

module Dune_fmt = Dune.Dune_fmt

let doc = "Format dune files"

let man =
  [ `S "DESCRIPTION"
  ; `P {|$(b,dune format-dune-file) reads a dune file and outputs a formatted
           version. |}
  ]

let info = Term.info "format-dune-file" ~doc ~man

let term =
  let%map path_opt =
    let docv = "FILE" in
    let doc = "Path to the dune file to parse." in
    Arg.(value & pos 0 (some path) None & info [] ~docv ~doc)
  and inplace =
    let doc = "Modify the file in place" in
    Arg.(value & flag & info ["inplace"] ~doc)
  in
  let (input, output) =
    match path_opt, inplace with
    | None, false ->
      (None, None)
    | Some path, true ->
      let path = Arg.Path.path path in
      (Some path, Some path)
    | Some path, false ->
      (Some (Arg.Path.path path), None)
    | None, true ->
      die "--inplace requires a file name"
  in
  Dune_fmt.format_file ~input ~output

let command = term, info
