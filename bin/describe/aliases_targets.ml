open Import

let ls_term_gen extra_args fetch_results =
  let+ builder = Common.Builder.term
  (* CR-someday Alizter: document this option *)
  and+ paths = Arg.(value & pos_all string [ "." ] & info [] ~docv:"DIR" ~doc:None)
  and+ context =
    Common.context_arg
      ~doc:(Some "The context to look in. Defaults to the default context.")
  and+ extra = extra_args in
  let common, config = Common.init builder in
  let request (_ : Dune_rules.Main.build_system) =
    let header = List.length paths > 1 in
    let open Action_builder.O in
    let+ paragraphs =
      Action_builder.List.map paths ~f:(fun path ->
        (* The user supplied directory *)
        let dir = Path.of_string path in
        (* The _build and source tree version of this directory *)
        let build_dir, src_dir =
          match (dir : Path.t) with
          | In_source_tree d ->
            Path.Build.append_source (Dune_engine.Context_name.build_dir context) d, d
          | In_build_dir d ->
            let src_dir =
              (* We only drop the build context if it is correct. *)
              match Path.Build.extract_build_context d with
              | Some (dir_context_name, d) ->
                if
                  Dune_engine.Context_name.equal
                    context
                    (Dune_engine.Context_name.of_string
                       (Filename.to_string dir_context_name))
                then d
                else
                  User_error.raise
                    [ Pp.textf
                        "Directory %s is not in context %S."
                        (Path.to_string_maybe_quoted dir)
                        (Dune_engine.Context_name.to_string context)
                    ]
              | None -> Code_error.raise "aliases_targets: build dir without context" []
            in
            d, src_dir
          | External _ ->
            User_error.raise
              [ Pp.textf
                  "Directories outside of the project are not supported: %s"
                  (Path.to_string_maybe_quoted dir)
              ]
        in
        (* Check if the directory exists. *)
        let* () =
          Action_builder.of_memo
          @@
          let open Memo.O in
          (* First check if it's a directory target *)
          Load_rules.load_dir ~dir:(Path.build build_dir)
          >>= function
          | Load_rules.Loaded.Build_under_directory_target _ ->
            User_error.raise
              [ Pp.textf
                  "Directory %s is a directory target. This command does not support the \
                   inspection of directory targets."
                  (Path.to_string dir)
              ]
          | External _ | Build _ | Source _ ->
            (* Check if directory exists in source tree or is a valid build-only directory *)
            Source_tree.find_dir src_dir
            >>= (function
             | Some _ -> Memo.return () (* Exists in source tree *)
             | None ->
               (* Not in source tree, check if it's a valid build-only subdirectory *)
               (match Path.Build.parent build_dir with
                | None ->
                  (* Build context root always exists *)
                  Memo.return ()
                | Some parent_dir ->
                  Load_rules.load_dir ~dir:(Path.build parent_dir)
                  >>| (function
                   | Load_rules.Loaded.Build { allowed_subdirs; _ } ->
                     let subdir_set =
                       Dune_engine.Dir_set.descend
                         allowed_subdirs
                         (Path.Build.basename build_dir)
                     in
                     if Dune_engine.Dir_set.here subdir_set
                     then ()
                     else
                       User_error.raise
                         [ Pp.textf "Directory %s does not exist." (Path.to_string dir) ]
                   | _ ->
                     User_error.raise
                       [ Pp.textf "Directory %s does not exist." (Path.to_string dir) ])))
        in
        let+ targets = fetch_results extra build_dir in
        (* If we are printing multiple directories, we print the directory
           name as a header. *)
        (if header then [ Pp.textf "%s:" (Path.to_string dir) ] else [])
        @ [ Pp.concat_map targets ~f:Pp.text ~sep:Pp.space ]
        |> Pp.concat ~sep:Pp.space)
    in
    Console.print
      [ Pp.vbox @@ Pp.concat_map ~f:Pp.vbox paragraphs ~sep:(Pp.seq Pp.space Pp.space) ]
  in
  Scheduler_setup.go_with_rpc_server ~common ~config
  @@ fun () ->
  let open Fiber.O in
  Build.run_build_system ~request >>| fun (_ : (unit, [ `Already_reported ]) result) -> ()
;;

module Aliases_cmd = struct
  let fetch_results () (dir : Path.Build.t) =
    let open Action_builder.O in
    let+ alias_targets =
      let+ load_dir =
        Action_builder.of_memo (Load_rules.load_dir ~dir:(Path.build dir))
      in
      match load_dir with
      | Load_rules.Loaded.Build build -> Dune_engine.Alias.Name.Map.keys build.aliases
      | _ -> []
    in
    List.map ~f:Dune_engine.Alias.Name.to_string alias_targets
  ;;

  let term = ls_term_gen (Term.const ()) fetch_results

  let command =
    let doc = "Print aliases in a given directory. Works similarly to ls." in
    Cmd.v (Cmd.info "aliases" ~doc ~envs:Common.envs) term
  ;;
end

module Targets_cmd = struct
  let fetch_results all (dir : Path.Build.t) =
    let open Action_builder.O in
    let+ load_dir = Action_builder.of_memo (Load_rules.load_dir ~dir:(Path.build dir)) in
    match load_dir with
    | Load_rules.Loaded.Build { rules_here; allowed_subdirs; _ } ->
      let file_targets =
        Path.Build.Map.keys rules_here.by_file_targets
        |> List.filter_map ~f:(fun path ->
          if Path.Build.equal (Path.Build.parent_exn path) dir
          then Some (Path.Build.basename path |> Filename.to_string)
          else None)
      in
      let dir_targets =
        Path.Build.Map.keys rules_here.by_directory_targets
        |> List.filter_map ~f:(fun path ->
          if Path.Build.equal (Path.Build.parent_exn path) dir
          then Some ((Path.Build.basename path |> Filename.to_string) ^ Filename.dir_sep)
          else None)
      in
      let subdirs =
        match Dune_engine.Dir_set.toplevel_subdirs allowed_subdirs with
        | Infinite -> []
        | Finite set ->
          Filename.Set.to_list set
          |> Filename.L.to_string
          |> List.filter ~f:(fun name -> all || String.length name = 0 || name.[0] <> '.')
          |> List.map ~f:(fun name -> name ^ Filename.dir_sep)
      in
      List.sort ~compare:String.compare (file_targets @ dir_targets @ subdirs)
    | _ -> []
  ;;

  let extra_args =
    Arg.(
      value
      & flag
      & info
          [ "a"; "all" ]
          ~doc:(Some "Show hidden directories (those starting with '.')."))
  ;;

  let term = ls_term_gen extra_args fetch_results

  let command =
    let doc = "Print targets in a given directory. Works similarly to ls." in
    Cmd.v (Cmd.info "targets" ~doc ~envs:Common.envs) term
  ;;
end
