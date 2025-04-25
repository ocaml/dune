open Import

type name_synopses =
  { name : string
  ; synopses : (Loc.t * Dune_engine.Synopsis.t) list
  }

let ls_term (fetch_results : Path.Build.t -> name_synopses list Action_builder.t) =
  let+ builder = Common.Builder.term
  and+ paths = Arg.(value & pos_all string [ "." ] & info [] ~docv:"DIR")
  and+ context =
    Common.context_arg ~doc:"The context to look in. Defaults to the default context."
  in
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
                    (Dune_engine.Context_name.of_string dir_context_name)
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
          Source_tree.find_dir src_dir
          >>= function
          | Some _ -> Memo.return ()
          | None ->
            (* The directory didn't exist. We therefore check if it was a
               directory target and error for the user accordingly. *)
            let+ is_dir_target =
              Load_rules.is_under_directory_target (Path.build build_dir)
            in
            if is_dir_target
            then
              User_error.raise
                [ Pp.textf
                    "Directory %s is a directory target. This command does not support \
                     the inspection of directory targets."
                    (Path.to_string dir)
                ]
            else
              User_error.raise
                [ Pp.textf "Directory %s does not exist." (Path.to_string dir) ]
        in
        let+ targets = fetch_results build_dir in
        (* If we are printing multiple directories, we print the directory
           name as a header. *)

        (* TODO: Pp.enumerate can be used to display synopsis in fancy way *)
        (if header then [ Pp.textf "%s:" (Path.to_string dir) ] else [])
        @ [ Pp.concat_map targets ~sep:Pp.cut ~f:(fun { name; synopses } ->
              Pp.concat
                ~sep:(if synopses = [] then Pp.nop else Pp.cut)
                [ Pp.hbox (Pp.textf "%s" name)
                ; Pp.enumerate synopses ~f:(fun (loc, synopsis) ->
                    Pp.concat
                      ~sep:Pp.space
                      [ Loc.pp_file_colon_line loc
                      ; Pp.text (Dune_engine.Synopsis.value synopsis)
                      ])
                ])
          ]
        |> Pp.concat ~sep:Pp.cut)
    in
    Console.print
      [ Pp.vbox @@ Pp.concat_map ~f:Pp.vbox paragraphs ~sep:(Pp.seq Pp.space Pp.space) ]
  in
  Scheduler.go ~common ~config
  @@ fun () ->
  let open Fiber.O in
  Build_cmd.run_build_system ~common ~request
  >>| fun (_ : (unit, [ `Already_reported ]) result) -> ()
;;

module Aliases_cmd = struct
  let fetch_results (dir : Path.Build.t) =
    let open Action_builder.O in
    let+ alias_targets =
      let+ load_dir =
        Action_builder.of_memo (Load_rules.load_dir ~dir:(Path.build dir))
      in
      match load_dir with
      | Load_rules.Loaded.Build build ->
        let name_synopses =
          build.aliases
          |> Dune_engine.Alias.Name.Map.mapi ~f:(fun name (_, synopses) ->
            let name = Dune_engine.Alias.Name.to_string name in
            { name; synopses })
          |> Dune_engine.Alias.Name.Map.values
        in
        name_synopses
      | _ -> []
    in
    alias_targets
  ;;

  let term = ls_term fetch_results

  let command =
    let doc = "Print aliases in a given directory. Works similarly to ls." in
    Cmd.v (Cmd.info "aliases" ~doc ~envs:Common.envs) term
  ;;
end

module Targets_cmd = struct
  let fetch_results (dir : Path.Build.t) =
    let open Action_builder.O in
    let+ targets =
      let open Memo.O in
      Target.all_direct_targets (Some (Path.Build.drop_build_context_exn dir))
      >>| Path.Build.Map.to_list
      |> Action_builder.of_memo
    in
    List.filter_map targets ~f:(fun (path, target_info) ->
      match Path.Build.equal (Path.Build.parent_exn path) dir with
      | false -> None
      | true ->
        (* directory targets can be distinguied by the trailing path separator
        *)
        let synopses synopsis loc =
          synopsis |> Option.to_list |> List.map ~f:(fun synopsis -> loc, synopsis)
        in
        Some
          (match target_info with
           | { target_type = Target.File; synopsis; loc } ->
             let target_name = Path.Build.basename path in
             { name = target_name; synopses = synopses synopsis loc }
           | { target_type = Target.Directory; synopsis; loc } ->
             { name = Path.Build.basename path ^ Filename.dir_sep
             ; synopses = synopses synopsis loc
             }))
  ;;

  let term = ls_term fetch_results

  let command =
    let doc = "Print targets in a given directory. Works similarly to ls." in
    Cmd.v (Cmd.info "targets" ~doc ~envs:Common.envs) term
  ;;
end
