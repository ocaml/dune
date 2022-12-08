open Stdune
module Log = Dune_util.Log
module Context = Dune_rules.Context
module Action_builder = Dune_engine.Action_builder
module Build_system = Dune_engine.Build_system
module Load_rules = Dune_engine.Load_rules
open Action_builder.O

(* CR-someday amokhov: Split [File] into [File] and [Dir] for clarity. *)
type t =
  | File of Path.t
  | Alias of Alias.t

let request targets =
  List.fold_left targets ~init:(Action_builder.return ()) ~f:(fun acc target ->
      acc
      >>>
      match target with
      | File path -> Action_builder.path path
      | Alias a -> Alias.request a)

let target_hint (_setup : Dune_rules.Main.build_system) path =
  let open Memo.O in
  let sub_dir = Option.value ~default:path (Path.parent path) in
  (* CR-someday amokhov:

     We currently provide the same hint for all targets. It would be nice to
     indicate whether a hint corresponds to a file or to a directory target. *)
  let root =
    match sub_dir with
    | External e ->
      Code_error.raise "target_hint: external path"
        [ ("path", Path.External.to_dyn e) ]
    | In_source_tree d -> d
    | In_build_dir d -> Path.Build.drop_build_context_exn d
  in
  let+ candidates =
    Load_rules.all_direct_targets (Some root) >>| Path.Build.Map.keys
  in
  let candidates =
    if Path.is_in_build_dir path then List.map ~f:Path.build candidates
    else
      List.map candidates ~f:(fun path ->
          match Path.Build.extract_build_context path with
          | None -> Path.build path
          | Some (_, path) -> Path.source path)
  in
  let candidates =
    (* Only suggest hints for the basename, otherwise it's slow when there are
       lots of files *)
    List.filter_map candidates ~f:(fun path ->
        if Path.equal (Path.parent_exn path) sub_dir then
          Some (Path.to_string path)
        else None)
  in
  let candidates = String.Set.of_list candidates |> String.Set.to_list in
  User_message.did_you_mean (Path.to_string path) ~candidates

let resolve_path path ~(setup : Dune_rules.Main.build_system) =
  let open Memo.O in
  let checked = Util.check_path setup.contexts path in
  let can't_build path =
    let+ hint = target_hint setup path in
    Error hint
  in
  let as_source_dir src =
    Dune_engine.Source_tree.dir_exists src >>| function
    | true ->
      Some
        [ Alias
            (Alias.in_dir ~name:Dune_engine.Alias.Name.default ~recursive:true
               ~contexts:setup.contexts path)
        ]
    | false -> None
  in
  let matching_targets src =
    Memo.parallel_map setup.contexts ~f:(fun ctx ->
        let path = Path.append_source (Path.build ctx.Context.build_dir) src in
        Load_rules.is_target path >>| function
        | Yes _ | Under_directory_target_so_cannot_say -> Some (File path)
        | No -> None)
    >>| List.filter_opt
  in
  let matching_target () =
    Load_rules.is_target path >>| function
    | Yes _ | Under_directory_target_so_cannot_say -> Some [ File path ]
    | No -> None
  in
  match checked with
  | External _ -> Memo.return (Ok [ File path ])
  | In_source_dir src -> (
    matching_targets src >>= function
    | [] -> (
      as_source_dir src >>= function
      | Some res -> Memo.return (Ok res)
      | None -> can't_build path)
    | l -> Memo.return (Ok l))
  | In_build_dir (_ctx, src) -> (
    matching_target () >>= function
    | Some res -> Memo.return (Ok res)
    | None -> (
      as_source_dir src >>= function
      | Some res -> Memo.return (Ok res)
      | None -> can't_build path))
  | In_install_dir _ -> (
    matching_target () >>= function
    | Some res -> Memo.return (Ok res)
    | None -> can't_build path)

let expand_path (root : Workspace_root.t)
    ~(setup : Dune_rules.Main.build_system) ctx sv =
  let sctx =
    Dune_engine.Context_name.Map.find_exn setup.scontexts (Context.name ctx)
  in
  let dir =
    Path.Build.relative ctx.Context.build_dir
      (String.concat ~sep:Filename.dir_sep root.to_cwd)
  in
  let* expander =
    Action_builder.of_memo (Dune_rules.Super_context.expander sctx ~dir)
  in
  let expander =
    Dune_rules.Dir_contents.add_sources_to_expander sctx expander
  in
  let+ s = Dune_rules.Expander.expand_str expander sv in
  Path.relative Path.root (root.reach_from_root_prefix ^ s)

let resolve_alias root ~recursive sv ~(setup : Dune_rules.Main.build_system) =
  match Dune_lang.String_with_vars.text_only sv with
  | Some s ->
    Ok [ Alias (Alias.of_string root ~recursive s ~contexts:setup.contexts) ]
  | None -> Error [ Pp.text "alias cannot contain variables" ]

let resolve_target root ~setup target =
  match target with
  | Dune_rules.Dep_conf.Alias sv as dep ->
    Action_builder.return
      (Result.map_error
         ~f:(fun hints -> (dep, hints))
         (resolve_alias root ~recursive:false sv ~setup))
  | Alias_rec sv as dep ->
    Action_builder.return
      (Result.map_error
         ~f:(fun hints -> (dep, hints))
         (resolve_alias root ~recursive:true sv ~setup))
  | File sv as dep ->
    let f ctx =
      let* path = expand_path root ~setup ctx sv in
      Action_builder.of_memo (resolve_path path ~setup)
      >>| Result.map_error ~f:(fun hints -> (dep, hints))
    in
    Action_builder.List.map setup.contexts ~f
    >>| Result.List.concat_map ~f:Fun.id
  | dep -> Action_builder.return (Error (dep, []))

let resolve_targets root (config : Dune_config.t)
    (setup : Dune_rules.Main.build_system) user_targets =
  match user_targets with
  | [] -> Action_builder.return []
  | _ ->
    let+ targets =
      Action_builder.List.map user_targets ~f:(resolve_target root ~setup)
    in
    (match config.display with
    | Simple { verbosity = Verbose; _ } ->
      Log.info
        [ Pp.text "Actual targets:"
        ; Pp.enumerate
            (List.concat_map targets ~f:(function
              | Ok targets -> targets
              | Error _ -> []))
            ~f:(function
              | File p -> Pp.verbatim (Path.to_string_maybe_quoted p)
              | Alias a -> Alias.pp a)
        ]
    | _ -> ());
    targets

let resolve_targets_exn root config setup user_targets =
  resolve_targets root config setup user_targets
  >>| List.concat_map ~f:(function
        | Error (dep, hints) ->
          User_error.raise
            [ Pp.textf "Don't know how to build %s"
                (Arg.Dep.to_string_maybe_quoted dep)
            ]
            ~hints
        | Ok targets -> targets)

let interpret_targets root config setup user_targets =
  let* () = Action_builder.return () in
  resolve_targets_exn root config setup user_targets >>= request
