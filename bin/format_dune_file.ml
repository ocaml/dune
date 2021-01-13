open Import
open Stdune
module Format_dune_lang = Dune_engine.Format_dune_lang

let doc = "Format dune files"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune format-dune-file) reads a dune file and outputs a formatted
           version. This is a low-level command, meant to implement editor
           support for example. To reformat a dune project, see the "Automatic
           formatting" section in the manual.|}
  ]

let info = Term.info "format-dune-file" ~doc ~man

let term =
  let+ path_opt =
    let docv = "FILE" in
    let doc = "Path to the dune file to parse." in
    Arg.(value & pos 0 (some path) None & info [] ~docv ~doc)
  and+ version =
    let docv = "VERSION" in
    let doc = "Which version of Dune language to use." in
    let default =
      Dune_lang.Syntax.greatest_supported_version Dune_engine.Stanza.syntax
    in
    Arg.(value & opt version default & info [ "dune-version" ] ~docv ~doc)
  in
  let input = Option.map ~f:Arg.Path.path path_opt in
  Format_dune_lang.format_file ~version ~input ~output:None

let command = (term, info)
