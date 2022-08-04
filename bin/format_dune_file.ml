open Import
open Stdune

let doc = "Format dune files"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune format-dune-file) reads a dune file and outputs a formatted
           version. This is a low-level command, meant to implement editor
           support for example. To reformat a dune project, see the "Automatic
           formatting" section in the manual.|}
  ]

let info = Cmd.info "format-dune-file" ~doc ~man

let format_file ~version ~input =
  let with_input =
    match input with
    | Some path -> fun f -> Io.with_lexbuf_from_file path ~f
    | None ->
      fun f ->
        Exn.protect
          ~f:(fun () -> f (Lexing.from_channel stdin))
          ~finally:(fun () -> close_in_noerr stdin)
  in
  match with_input Dune_lang.Format.parse with
  | Sexps sexps ->
    Format.fprintf Format.std_formatter "%a%!" Pp.to_fmt
      (Dune_lang.Format.pp_top_sexps ~version sexps)
  | OCaml_syntax loc -> (
    match input with
    | None -> User_error.raise ~loc [ Pp.text "OCaml syntax is not supported." ]
    | Some path ->
      Io.with_file_in path ~f:(fun ic -> Io.copy_channels ic stdout))

let term =
  let+ path_opt =
    let docv = "FILE" in
    let doc = "Path to the dune file to parse." in
    Arg.(value & pos 0 (some path) None & info [] ~docv ~doc)
  and+ version =
    let docv = "VERSION" in
    let doc = "Which version of Dune language to use." in
    let default =
      Dune_lang.Syntax.greatest_supported_version Dune_lang.Stanza.syntax
    in
    Arg.(value & opt version default & info [ "dune-version" ] ~docv ~doc)
  in
  let input = Option.map ~f:Arg.Path.path path_opt in
  format_file ~version ~input

let command = Cmd.v info term
