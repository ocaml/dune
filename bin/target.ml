open Import
open Action_builder.O

module Request = struct
  (* CR-someday amokhov: Split [File] into [File] and [Dir] for clarity. *)
  type t =
    | File of Path.t
    | Alias of Alias.t
end

let request targets =
  List.fold_left targets ~init:(Action_builder.return ()) ~f:(fun acc target ->
    acc
    >>>
    match (target : Request.t) with
    | File path -> Action_builder.path path
    | Alias a -> Alias.request a)
;;

module Target_type = struct
  type t =
    | File
    | Directory
end

module All_targets = struct
  type t = Target_type.t Path.Build.Map.t

  include Monoid.Make (struct
      type nonrec t = t

      let empty = Path.Build.Map.empty
      let combine = Path.Build.Map.union_exn
    end)
end

module Source_tree = Dune_rules.Source_tree
module Source_tree_map_reduce = Source_tree.Dir.Make_map_reduce (Memo) (All_targets)

let all_direct_targets dir =
  let open Memo.O in
  let* root =
    match dir with
    | None -> Source_tree.root ()
    | Some dir -> Source_tree.nearest_dir dir
  and* contexts = Memo.Lazy.force (Build_config.get ()).contexts in
  Context_name.Map.values contexts
  |> List.filter_map ~f:(fun (ctx, (ctx_type : Build_config.Gen_rules.Context_type.t)) ->
    match ctx_type with
    | Empty -> None
    | With_sources -> Some ctx)
  |> Memo.parallel_map ~f:(fun (ctx : Dune_engine.Build_context.t) ->
    Source_tree_map_reduce.map_reduce
      root
      ~traverse:Source_dir_status.Set.all
      ~f:(fun dir ->
        Dune_engine.Load_rules.load_dir
          ~dir:
            (Path.build
               (Path.Build.append_source ctx.build_dir (Source_tree.Dir.path dir)))
        >>| function
        | External _ | Source _ -> All_targets.empty
        | Build { rules_here; _ } ->
          All_targets.combine
            (Path.Build.Map.map rules_here.by_file_targets ~f:(fun _ -> Target_type.File))
            (Path.Build.Map.map rules_here.by_directory_targets ~f:(fun _ ->
               Target_type.Directory))
        | Build_under_directory_target _ -> All_targets.empty))
  >>| All_targets.reduce
;;

let target_hint (_setup : Dune_rules.Main.build_system) path =
  let open Memo.O in
  let sub_dir = Option.value ~default:path (Path.parent path) in
  (* CR-someday amokhov:

     We currently provide the same hint for all targets. It would be nice to
     indicate whether a hint corresponds to a file or to a directory target. *)
  let root =
    match sub_dir with
    | External e ->
      Code_error.raise "target_hint: external path" [ "path", Path.External.to_dyn e ]
    | In_source_tree d -> d
    | In_build_dir d -> Path.Build.drop_build_context_exn d
  in
  let+ candidates = all_direct_targets (Some root) >>| Path.Build.Map.keys in
  let candidates =
    if Path.is_in_build_dir path
    then List.map ~f:Path.build candidates
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
      if Path.equal (Path.parent_exn path) sub_dir
      then Some (Path.to_string path)
      else None)
  in
  let candidates = String.Set.of_list candidates |> String.Set.to_list in
  User_message.did_you_mean (Path.to_string path) ~candidates
;;

let resolve_path path ~(setup : Dune_rules.Main.build_system)
  : (Request.t list, _) result Memo.t
  =
  let open Memo.O in
  let checked = Util.check_path setup.contexts path in
  let can't_build path =
    let+ hint = target_hint setup path in
    Error hint
  in
  let as_source_dir src =
    Source_tree.find_dir src
    >>| Option.map ~f:(fun _ ->
      [ Request.Alias
          (Alias.in_dir
             ~name:Dune_engine.Alias.Name.default
             ~recursive:true
             ~contexts:setup.contexts
             path)
      ])
  in
  let matching_targets src =
    Memo.parallel_map setup.contexts ~f:(fun ctx ->
      let path = Path.append_source (Path.build (Context.build_dir ctx)) src in
      Load_rules.is_target path
      >>| function
      | Yes _ | Under_directory_target_so_cannot_say -> Some (Request.File path)
      | No -> None)
    >>| List.filter_opt
  in
  let matching_target () =
    Load_rules.is_target path
    >>| function
    | Yes _ | Under_directory_target_so_cannot_say -> Some [ Request.File path ]
    | No -> None
  in
  match checked with
  | External _ -> Memo.return (Ok [ Request.File path ])
  | In_source_dir src ->
    matching_targets src
    >>= (function
     | [] ->
       as_source_dir src
       >>= (function
        | Some res -> Memo.return (Ok res)
        | None -> can't_build path)
     | l -> Memo.return (Ok l))
  | In_build_dir (_ctx, src) ->
    matching_target ()
    >>= (function
     | Some res -> Memo.return (Ok res)
     | None ->
       as_source_dir src
       >>= (function
        | Some res -> Memo.return (Ok res)
        | None -> can't_build path))
  | In_private_context _ | In_install_dir _ ->
    matching_target ()
    >>= (function
     | Some res -> Memo.return (Ok res)
     | None -> can't_build path)
;;

let expand_path_from_root (root : Workspace_root.t) sctx sv =
  let+ s =
    let* expander =
      let dir =
        let ctx = Super_context.context sctx in
        Path.Build.relative
          (Context.build_dir ctx)
          (String.concat ~sep:Filename.dir_sep root.to_cwd)
      in
      Action_builder.of_memo (Dune_rules.Super_context.expander sctx ~dir)
    in
    Dune_rules.Expander.expand_str expander sv
  in
  root.reach_from_root_prefix ^ s
;;

let expand_path root sctx sv =
  let+ s = expand_path_from_root root sctx sv in
  Path.relative Path.root s
;;

let resolve_alias root ~recursive sv ~(setup : Dune_rules.Main.build_system) =
  match Dune_lang.String_with_vars.text_only sv with
  | Some s ->
    Ok [ Request.Alias (Alias.of_string root ~recursive s ~contexts:setup.contexts) ]
  | None -> Error [ Pp.text "alias cannot contain variables" ]
;;

let resolve_target root ~setup target =
  match (target : Dune_lang.Dep_conf.t) with
  | Alias sv as dep ->
    Action_builder.return
      (Result.map_error
         ~f:(fun hints -> dep, hints)
         (resolve_alias root ~recursive:false sv ~setup))
  | Alias_rec sv as dep ->
    Action_builder.return
      (Result.map_error
         ~f:(fun hints -> dep, hints)
         (resolve_alias root ~recursive:true sv ~setup))
  | File sv as dep ->
    let f ctx =
      let sctx =
        Dune_engine.Context_name.Map.find_exn setup.scontexts (Context.name ctx)
      in
      let* path = expand_path root sctx sv in
      Action_builder.of_memo (resolve_path path ~setup)
      >>| Result.map_error ~f:(fun hints -> dep, hints)
    in
    Action_builder.List.map setup.contexts ~f >>| Result.List.concat_map ~f:Fun.id
  | dep -> Action_builder.return (Error (dep, []))
;;

let resolve_targets
  root
  (config : Dune_config.t)
  (setup : Dune_rules.Main.build_system)
  user_targets
  =
  match user_targets with
  | [] -> Action_builder.return []
  | _ ->
    let+ targets = Action_builder.List.map user_targets ~f:(resolve_target root ~setup) in
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
;;

let resolve_targets_exn root config setup user_targets =
  resolve_targets root config setup user_targets
  >>| List.concat_map ~f:(function
    | Error (dep, hints) ->
      User_error.raise
        [ Pp.textf "Don't know how to build %s" (Arg.Dep.to_string_maybe_quoted dep) ]
        ~hints
    | Ok targets -> targets)
;;

let interpret_targets root config setup user_targets =
  let* () = Action_builder.return () in
  resolve_targets_exn root config setup user_targets >>= request
;;

type target_type = Target_type.t =
  | File
  | Directory
