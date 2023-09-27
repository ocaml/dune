open Import

let doc = "Build and view the documentation of an OCaml project"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune ocaml doc) builds and then opens the documention of an OCaml project in the users default browser.|}
  ; `Blocks Common.help_secs
  ]
;;

let info = Cmd.info "doc" ~doc ~man

let term =
  let+ common = Common.term in
  let config = Common.init common in
  let request (setup : Import.Main.build_system) =
    let dir = Path.(relative root) (Common.prefix_target common ".") in
    let open Action_builder.O in
    let+ () =
      Alias.in_dir
        ~name:(Dune_engine.Alias.Name.of_string "doc")
        ~recursive:true
        ~contexts:setup.contexts
        dir
      |> Alias.request
    in
    let is_default ctx = ctx |> Context.name |> Dune_engine.Context_name.is_default in
    let doc_ctx = List.find_exn setup.contexts ~f:is_default in
    let toplevel_index_path = Dune_rules.Odoc.Paths.toplevel_index doc_ctx in
    let absolute_toplevel_index_path =
      Path.(toplevel_index_path |> build |> to_absolute_filename)
    in
    Dune_console.print
      [ Pp.textf "Docs built. Index can be found here: %s\n" absolute_toplevel_index_path
      ];
    let url = "file://" ^ absolute_toplevel_index_path in
    let cmd =
      let open Option.O in
      let path = Env_path.path Env.initial in
      let* cmd_name, args =
        match (Platform.OS.value : Platform.OS.t) with
        | Darwin -> Some ("open", [ "-u" ])
        | Linux -> Some ("xdg-open", [])
        | Windows -> None
        | Other | FreeBSD | NetBSD | OpenBSD -> None
      in
      let+ p = Bin.which ~path cmd_name in
      ( p
      , (* First element of argv is the name of the command. *)
        (cmd_name :: args) @ [ url ] )
    in
    match cmd with
    | Some (cmd, args) ->
      Proc.restore_cwd_and_execve (Path.to_absolute_filename cmd) args ~env:Env.initial
    | None ->
      User_warning.emit
        [ Pp.text
            "No browser could be found, you will have to open the documentation yourself."
        ]
  in
  Build_cmd.run_build_command ~common ~config ~request
;;

let cmd = Cmd.v info term
