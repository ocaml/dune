open Stdune
open Import

let doc = "Generate merlin files (internal)"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {| $(b, dune merlin-file) generates a .merlin file for a particular file.
       This operation is internal to dune.
       |}
  ]

let info = Term.info "merlin-file" ~doc ~man

let term =
  let+ common = Common.term
  and+ path =
    let docv = "FILE" in
    let doc = "Source file" in
    Arg.(required & pos 0 (some Arg.path) None & info [] ~docv ~doc)
  in
  let path = Path.Source.of_string (Arg.Path.arg path) in
  Common.set_common common ~targets:[];
  Scheduler.go ~common (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup common in
      let src_dir =
        match
          let open Option.O in
          let* parent = Path.Source.parent path in
          Dune.File_tree.find_dir parent
        with
        | Some d -> Dune.File_tree.Dir.path d
        | None ->
          User_error.raise
            [ Pp.textf "Invalid source file %s"
                (Path.Source.to_string_maybe_quoted path)
            ]
      in
      let context : Context.t =
        match
          setup.workspace.contexts
          |> List.find ~f:(fun (ctx : Dune.Context.t) -> ctx.merlin)
        with
        | Some c -> c
        | None -> User_error.raise [ Pp.textf "no merlin context is defined" ]
      in
      let target =
        let dir = Path.Build.append_source context.build_dir src_dir in
        Path.Build.relative dir Dune.Merlin.merlin_filename |> Path.build
      in
      let+ () = do_build [ File target ] in
      Io.read_file target |> print_string)

let command = (term, info)
