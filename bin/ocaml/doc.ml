open Import
module Main = Import.Main

let doc = "Build and view the documentation of an OCaml project"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune ocaml doc) builds and then opens the documentation of an OCaml project in the users default browser.|}
  ; `Blocks Common.help_secs
  ]
;;

let info = Cmd.info "doc" ~doc ~man

let lock_odoc_if_dev_tool_enabled () =
  match Lazy.force Lock_dev_tool.is_enabled with
  | false -> Action_builder.return ()
  | true -> Action_builder.of_memo (Lock_dev_tool.lock_dev_tool Odoc)
;;

let term =
  let+ builder = Common.Builder.term in
  let common, config = Common.init builder in
  let request (setup : Main.build_system) =
    let dir = Path.(relative root) (Common.prefix_target common ".") in
    let open Action_builder.O in
    let* () = lock_odoc_if_dev_tool_enabled () in
    let+ () =
      Alias.in_dir ~name:Dune_rules.Alias.doc ~recursive:true ~contexts:setup.contexts dir
      |> Alias.request
    in
    let relative_toplevel_index_path =
      let toplevel_index_path =
        let is_default ctx = ctx |> Context.name |> Dune_engine.Context_name.is_default in
        let doc_ctx = List.find_exn setup.contexts ~f:is_default in
        Dune_rules.Odoc.Paths.toplevel_index doc_ctx
      in
      Path.(toplevel_index_path |> build |> to_string_maybe_quoted)
    in
    Console.print
      [ Pp.textf "Docs built. Index can be found here: %s" relative_toplevel_index_path ];
    match
      let open Option.O in
      let* cmd_name, args =
        match Platform.OS.value with
        | Darwin -> Some ("open", [])
        | Other | FreeBSD | NetBSD | OpenBSD | Haiku | Linux -> Some ("xdg-open", [])
        | Windows -> None
      in
      let+ open_command =
        let path = Env_path.path Env.initial in
        Bin.which ~path cmd_name
      in
      open_command, args @ [ relative_toplevel_index_path ]
    with
    | Some (cmd, args) ->
      Proc.restore_cwd_and_execve (Path.to_absolute_filename cmd) args ~env:Env.initial
    | None ->
      User_warning.emit
        [ Pp.text
            "No browser could be found, you will have to open the documentation yourself."
        ]
  in
  Build.run_build_command ~common ~config ~request
;;

let cmd = Cmd.v info term
