open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules
module Context_type = Build_config.Context_type
module Build_only_sub_dirs = Gen_rules.Build_only_sub_dirs

module Current_rule_loc = struct
  let t = ref (fun () -> Memo.return None)
  let set f = t := f
  let get () = !t ()
end

let set_current_rule_loc = Current_rule_loc.set

module Loaded = struct
  (* CR-someday amokhov: Loaded rules are relative to the directory passed to [load_dir],
     so these maps should probably be indexed by [Filename.t]s rather than [Path.t]s. We
     could add [Filename_map.t] anchored to a specific directory like [Filename_set.t]. *)
  type rules_here =
    { by_file_targets : Rule.t Path.Build.Map.t
    ; by_directory_targets : Rule.t Path.Build.Map.t
    }

  let no_rules_here =
    { by_file_targets = Path.Build.Map.empty
    ; by_directory_targets = Path.Build.Map.empty
    }
  ;;

  type build =
    { allowed_subdirs : Path.Unspecified.w Dir_set.t
    ; rules_here : rules_here
    ; aliases : (Loc.t * Rules.Dir_rules.Alias_spec.item) list Alias.Name.Map.t
    }

  type t =
    | Source of { filenames : Filename.Array.Set.t }
    | External of { filenames : Filename.Array.Set.t }
    | Build of build
    | Build_under_directory_target of { directory_target_ancestor : Path.Build.t }

  let no_rules ~allowed_subdirs =
    Build { allowed_subdirs; rules_here = no_rules_here; aliases = Alias.Name.Map.empty }
  ;;
end

module Dir_triage = struct
  module Build_directory = struct
    (* invariant: [dir = context_name / sub_dir] *)
    type t =
      { dir : Path.Build.t
      ; context_name : Context_name.t
      ; context_type : Context_type.t
      ; sub_dir : Path.Source.t
      }

    (* It's ok to only compare and hash the [dir] field because of the
       invariant. *)
    let equal a b = Path.Build.equal a.dir b.dir
    let hash t = Path.Build.hash t.dir
    let to_dyn t = Path.Build.to_dyn t.dir

    let parent t =
      Option.map (Path.Source.parent t.sub_dir) ~f:(fun sub_dir ->
        { t with dir = Path.Build.parent_exn t.dir; sub_dir })
    ;;

    let is_build_only_root { context_type; sub_dir; _ } =
      match context_type with
      | Empty -> false
      | With_sources { build_only_sub_dirs } ->
        (match Path.Source.split_first_component sub_dir with
         | Some (name, rest) when Path.Local.is_root rest ->
           Filename.Set.mem build_only_sub_dirs name
         | _ -> false)
    ;;

    let has_workspace_source { context_type; sub_dir; _ } =
      match context_type with
      | Empty -> false
      | With_sources { build_only_sub_dirs } ->
        (match Path.Source.split_first_component sub_dir with
         | None -> true
         | Some (name, _) -> not (Filename.Set.mem build_only_sub_dirs name))
    ;;
  end

  type t =
    | Known of Loaded.t
    | Build_directory of Build_directory.t

  let empty_source = Known (Source { filenames = Filename.Array.Set.empty })
  let no_rules = Known (Loaded.no_rules ~allowed_subdirs:Dir_set.empty)
end

let get_dir_triage ~dir =
  match (dir : Path.t) with
  | In_source_tree dir ->
    let module Source_tree = (val (Build_config.get ()).source_tree) in
    Source_tree.find_dir dir
    >>| (function
     | None -> Dir_triage.empty_source
     | Some dir -> Dir_triage.Known (Source { filenames = Source_tree.Dir.filenames dir }))
  | External dir_ext ->
    let+ filenames =
      Fs_memo.dir_contents (External dir_ext)
      >>| function
      | Error (Unix.ENOENT, _, _) -> Filename.Array.Set.empty
      | Error unix_error ->
        User_warning.emit
          [ Pp.textf "Unable to read %s" (Path.to_string_maybe_quoted dir)
          ; Unix_error.Detailed.pp_reason unix_error
          ];
        Filename.Array.Set.empty
      | Ok filenames ->
        Fs_memo.Dir_contents.to_list filenames
        |> List.filter_map ~f:(fun (filename, kind) ->
          match kind with
          | Unix.S_DIR -> None
          | _ -> Some filename)
        |> Filename.Array.Set.of_list
    in
    Dir_triage.Known (External { filenames })
  | In_build_dir dir ->
    (match Dpath.Target_dir.of_target dir with
     | Regular Root ->
       let+ contexts = Memo.Lazy.force (Build_config.get ()).contexts in
       let allowed_subdirs =
         [ Path.Build.basename Dpath.Build.anonymous_actions_dir ]
         @ (Context_name.Map.keys contexts
            |> List.map ~f:(fun name ->
              Filename.of_string_exn (Context_name.to_string name)))
         |> Subdir_set.of_list
         |> Subdir_set.to_dir_set
       in
       Dir_triage.Known (Loaded.no_rules ~allowed_subdirs)
     | Anonymous_action p ->
       let build_dir = Dpath.Target_dir.build_dir p in
       Code_error.raise
         "Called get_dir_triage on an anonymous action directory"
         [ "dir", Path.Build.to_dyn build_dir ]
     | Invalid _ ->
       Memo.return @@ Dir_triage.Known (Loaded.no_rules ~allowed_subdirs:Dir_set.empty)
     | Regular (With_context (context_name, sub_dir)) ->
       let+ contexts = Memo.Lazy.force (Build_config.get ()).contexts in
       (match Context_name.Map.find contexts context_name with
        | None -> Dir_triage.no_rules
        | Some context_type ->
          Dir_triage.Build_directory { dir; context_name; context_type; sub_dir }))
;;

let describe_rule (rule : Rule.t) =
  Pp.text
  @@
  match rule.info with
  | From_dune_file loc ->
    let start = Loc.start loc in
    start.pos_fname ^ ":" ^ string_of_int start.pos_lnum
  | Internal -> "<internal location>"
  | Source_file_copy _ -> "file present in source tree"
;;

let report_rule_src_dir_conflict dir fn (rule : Rule.t) =
  let loc =
    match rule.info with
    | From_dune_file loc -> loc
    | Internal | Source_file_copy _ ->
      let dir =
        match Path.Build.drop_build_context dir with
        | None -> Path.build dir
        | Some s -> Path.source s
      in
      Loc.in_dir dir
  in
  User_error.raise
    ~loc
    [ Pp.textf
        "This rule defines a target %S whose name conflicts with a source directory in \
         the same directory."
        fn
    ]
    ~hints:
      [ Pp.textf
          "If you want Dune to generate and replace %S, add (mode promote) to the rule \
           stanza. Alternatively, you can delete %S from the source tree or change the \
           rule to generate a different target."
          fn
          fn
      ]
;;

let report_rule_conflict fn (rule' : Rule.t) (rule : Rule.t) =
  let fn = Path.build fn in
  User_error.raise
    [ Pp.textf "Multiple rules generated for %s:" (Path.to_string_maybe_quoted fn)
    ; Pp.enumerate ~f:describe_rule [ rule'; rule ]
    ]
    ~hints:
      (match rule.info, rule'.info with
       | Source_file_copy _, _ | _, Source_file_copy _ ->
         [ Pp.textf
             "rm -f %s"
             (Path.to_string_maybe_quoted (Path.drop_optional_build_context fn))
         ]
       | _ -> [])
;;

let remove_subdir_if_stale ~dir ~subdirs_to_keep fn =
  if not (Subdir_set.mem subdirs_to_keep fn)
  then (
    let path = Path.Build.relative_fname dir fn in
    let () = Rule_cache.Workspace_local.remove_subtree path in
    Path.rm_rf (Path.build path))
;;

let remove_old_artifacts
      ~dir
      ~(rules_here : Loaded.rules_here)
      ~(subdirs_to_keep : Subdir_set.t)
  =
  match Path.Untracked.readdir_unsorted_with_kinds (Path.build dir) with
  | Error _ -> ()
  | Ok files ->
    List.iter files ~f:(fun (fn, kind) ->
      let path = Path.Build.relative_fname dir fn in
      let path_is_a_target =
        Path.Build.Map.mem rules_here.by_file_targets path
        || Path.Build.Map.mem rules_here.by_directory_targets path
      in
      if not path_is_a_target
      then (
        match kind with
        | Unix.S_DIR -> remove_subdir_if_stale ~dir ~subdirs_to_keep fn
        | _ ->
          let () = Rule_cache.Workspace_local.remove_target path in
          Fpath.unlink_exn (Path.Build.to_string path)))
;;

(* We don't remove files in there as we don't know upfront if they are stale or
   not. *)
let remove_old_sub_dirs_in_anonymous_actions_dir ~dir ~(subdirs_to_keep : Subdir_set.t) =
  match Path.Untracked.readdir_unsorted_with_kinds (Path.build dir) with
  | Error _ -> ()
  | Ok files ->
    List.iter files ~f:(fun (fn, kind) ->
      match kind with
      | Unix.S_DIR -> remove_subdir_if_stale ~dir ~subdirs_to_keep fn
      | _ -> ())
;;

let no_rule_found ~loc fn =
  let+ contexts = Memo.Lazy.force (Build_config.get ()).contexts in
  let fail fn ~loc =
    User_error.raise ?loc [ Pp.textf "No rule found for %s" (Dpath.describe_target fn) ]
  in
  let hints ctx =
    let candidates =
      Context_name.Map.to_list_map contexts ~f:(fun name _ -> Context_name.to_string name)
    in
    User_message.did_you_mean (Context_name.to_string ctx) ~candidates
  in
  match Dpath.analyse_target fn with
  | Other _ -> fail fn ~loc
  | Regular (ctx, _) ->
    if Context_name.Map.mem contexts ctx
    then fail fn ~loc
    else
      User_error.raise
        [ Pp.textf
            "Trying to build %s but build context %s doesn't exist."
            (Path.Build.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Alias (ctx, fn') ->
    if Context_name.Map.mem contexts ctx
    then fail fn ~loc
    else (
      let fn = Path.append_source (Path.build (Context_name.build_dir ctx)) fn' in
      User_error.raise
        [ Pp.textf
            "Trying to build alias %s but build context %s doesn't exist."
            (Path.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx))
  | Anonymous_action _ ->
    (* We never lookup such actions by target name, so this should be
       unreachable *)
    Code_error.raise
      ?loc
      "Build_system.no_rule_found got anonymous action path"
      [ "fn", Path.Build.to_dyn fn ]
;;

module rec Load_rules : sig
  val load_dir : dir:Path.t -> Loaded.t Memo.t
  val is_under_directory_target : Path.t -> bool Memo.t

  val lookup_alias
    :  Alias.t
    -> (Loc.t * Rules.Dir_rules.Alias_spec.item) list option Memo.t
end = struct
  open Load_rules

  let copy_source_action ~src_path ~build_path : Action.Full.t Action_builder.t =
    let action =
      Action.Full.make
        (Action.copy (Path.source src_path) build_path)
        (* Sandboxing this action doesn't make much sense: if we can copy [src_path] to
           the sandbox, we might as well copy it to the build directory directly. *)
        ~sandbox:Sandbox_config.no_sandboxing
    in
    Action_builder.Expert.record_dep_on_source_file_exn
      action
      ~loc:Current_rule_loc.get
      src_path
  ;;

  let create_copy_rules ~dir ~ctx_dir ~non_target_source_filenames =
    Filename.Array.Set.to_list_map non_target_source_filenames ~f:(fun filename ->
      let src_path = Path.Source.relative_fname dir filename in
      let build_path = Path.Build.append_source ctx_dir src_path in
      Rule.make
        ~info:(Source_file_copy (Path.source src_path))
        ~targets:(Targets.File.create build_path)
        (copy_source_action ~src_path ~build_path))
  ;;

  let compile_rules ~dir ~source_dirs rules =
    let check_for_source_dir_conflict rule target =
      if Filename.Array.Set.mem source_dirs target
      then report_rule_src_dir_conflict dir (Filename.to_string target) rule
    in
    let add_targets rules ~targets rule =
      Filename.Set.fold targets ~init:rules ~f:(fun target rules ->
        check_for_source_dir_conflict rule target;
        let target = Path.Build.relative_fname rule.targets.root target in
        Path.Build.Map.update rules target ~f:(function
          | None -> Some rule
          | Some other -> Some (report_rule_conflict target other rule)))
    in
    let by_file_targets, by_directory_targets =
      List.fold_left
        rules
        ~init:(Path.Build.Map.empty, Path.Build.Map.empty)
        ~f:(fun (by_file_targets, by_directory_targets) rule ->
          assert (Path.Build.( = ) dir rule.Rule.targets.root);
          ( add_targets by_file_targets ~targets:rule.targets.files rule
          , add_targets by_directory_targets ~targets:rule.targets.dirs rule ))
    in
    (match
       ( Path.Build.Map.is_empty by_file_targets
       , Path.Build.Map.is_empty by_directory_targets )
     with
     | true, _ | _, true -> ()
     | false, false ->
       Path.Build.Map.iter2
         by_file_targets
         by_directory_targets
         ~f:(fun target rule1 rule2 ->
           match rule1, rule2 with
           | None, _ | _, None -> ()
           | Some rule1, Some rule2 -> report_rule_conflict target rule1 rule2));
    { Loaded.by_file_targets; by_directory_targets }
  ;;

  let lookup_alias alias =
    load_dir ~dir:(Path.build (Alias.dir alias))
    >>| function
    | Source _ | External _ ->
      Code_error.raise "Alias in a non-build dir" [ "alias", Alias.to_dyn alias ]
    | Build { aliases; _ } -> Alias.Name.Map.find aliases (Alias.name alias)
    | Build_under_directory_target _ -> None
  ;;

  let compute_alias_expansions ~(collected : Rules.Dir_rules.ready) ~dir =
    let+ aliases =
      let aliases = collected.aliases in
      if Alias.Name.Map.mem aliases Alias.Name.default
      then Memo.return aliases
      else
        (Build_config.get ()).implicit_default_alias dir
        >>| function
        | None -> aliases
        | Some expansion ->
          Alias.Name.Map.set
            aliases
            Alias.Name.default
            (Rules.Dir_rules.Alias_spec.singleton
               Loc.none
               (Rules.Dir_rules.Alias_spec.Deps expansion))
    in
    Alias.Name.Map.map aliases ~f:Rules.Dir_rules.Alias_spec.to_list
  ;;

  (** A directory is only allowed to be generated if its parent knows about it.
      This restriction is necessary to prevent stale artifact deletion from
      removing that directory.

      This module encodes that restriction. *)
  module Generated_directory_restrictions : sig
    type restriction =
      | Unrestricted
      | Restricted of Path.Unspecified.w Dir_set.t Memo.Lazy.t

    (** Used by the child to ask about the restrictions placed by the parent. *)
    val allowed_by_parent : dir:Path.Build.t -> restriction Memo.t
  end = struct
    type restriction =
      | Unrestricted
      | Restricted of Path.Unspecified.w Dir_set.t Memo.Lazy.t

    let source_subdirs_of_build_dir ~dir =
      let module Source_tree = (val (Build_config.get ()).source_tree) in
      let corresponding_source_dir =
        match Dpath.analyse_target dir with
        | Alias _ | Anonymous_action _ | Other _ -> Memo.return None
        | Regular (_ctx, sub_dir) -> Source_tree.find_dir sub_dir
      in
      corresponding_source_dir
      >>| function
      | None -> Filename.Array.Set.empty
      | Some dir -> Source_tree.Dir.sub_dir_names dir
    ;;

    let allowed_dirs ~dir ~subdir : restriction Memo.t =
      let+ subdirs = source_subdirs_of_build_dir ~dir in
      if Filename.Array.Set.mem subdirs subdir
      then Unrestricted
      else
        Restricted
          (Memo.Lazy.create ~name:"allowed_dirs" (fun () ->
             load_dir ~dir:(Path.build dir)
             >>| function
             | External _ | Source _ -> Dir_set.just_the_root
             | Build { allowed_subdirs; _ } -> Dir_set.descend allowed_subdirs subdir
             | Build_under_directory_target _ -> Dir_set.empty))
    ;;

    let allowed_by_parent ~dir =
      allowed_dirs ~dir:(Path.Build.parent_exn dir) ~subdir:(Path.Build.basename dir)
    ;;
  end

  module Normal = struct
    type t =
      { build_dir_only_sub_dirs : Build_only_sub_dirs.t
      ; directory_targets : Loc.t Path.Build.Map.t
      ; rules : Rules.t Memo.Lazy.t
      }

    let combine_exn r { build_dir_only_sub_dirs; directory_targets; rules } =
      { build_dir_only_sub_dirs =
          Build_only_sub_dirs.union r.build_dir_only_sub_dirs build_dir_only_sub_dirs
      ; directory_targets = Path.Build.Map.union_exn r.directory_targets directory_targets
      ; rules =
          Memo.lazy_ ~name:"union-rules" (fun () ->
            let open Memo.O in
            let+ r = Memo.Lazy.force r.rules
            and+ r' = Memo.Lazy.force rules in
            Rules.union r r')
      }
    ;;

    let check_all_directory_targets_are_descendant ~of_:dir directory_targets =
      Path.Build.Map.iteri directory_targets ~f:(fun p _loc ->
        if not (Path.Build.is_descendant p ~of_:dir)
        then
          Code_error.raise
            "[gen_rules] returned directory target in a directory that is not a \
             descendant of the directory it was called for"
            [ "dir", Path.Build.to_dyn dir; "example", Path.Build.to_dyn p ])
    ;;

    let check_all_sub_dirs_rule_dirs_are_descendant ~of_:dir build_dir_only_sub_dirs =
      Path.Build.Map.iteri build_dir_only_sub_dirs ~f:(fun p _ ->
        if not (Path.Build.is_descendant p ~of_:dir)
        then
          Code_error.raise
            "[gen_rules] returned sub-directories in a directory that is not a \
             descendant of the directory it was called for"
            [ "dir", Path.Build.to_dyn dir; "example", Path.Build.to_dyn p ])
    ;;

    let check_all_rules_are_descendant ~of_:dir rules =
      match
        Path.Build.Map.find_key (Rules.to_map rules) ~f:(fun p ->
          not (Path.Build.is_descendant p ~of_:dir))
      with
      | None -> ()
      | Some p ->
        let dir_rules = Rules.find rules (Path.build p) |> Rules.Dir_rules.consume in
        Code_error.raise
          "[gen_rules] returned rules in a directory that is not a descendant of the \
           directory it was called for"
          [ "dir", Path.Build.to_dyn dir
          ; ( "example"
            , match dir_rules with
              | { rules = r :: _; _ } ->
                Dyn.Variant
                  ( "Rule"
                  , [ Dyn.Record [ "targets", Targets.Validated.to_dyn r.targets ] ] )
              | { rules = []; aliases } ->
                (match Alias.Name.Map.choose aliases with
                 | None -> assert false
                 | Some (name, _) ->
                   Dyn.Variant
                     ( "Alias"
                     , [ Dyn.Record
                           [ "dir", Path.Build.to_dyn p; "name", Alias.Name.to_dyn name ]
                       ] )) )
          ]
    ;;

    let make_rules_gen_result
          ~of_
          { Gen_rules.Rules.build_dir_only_sub_dirs; directory_targets; rules }
      =
      check_all_directory_targets_are_descendant ~of_ directory_targets;
      check_all_sub_dirs_rule_dirs_are_descendant ~of_ build_dir_only_sub_dirs;
      let rules =
        Memo.lazy_ ~name:"check-rules-are-descendant" (fun () ->
          let+ rules = rules in
          check_all_rules_are_descendant ~of_ rules;
          rules)
      in
      { build_dir_only_sub_dirs; directory_targets; rules }
    ;;
  end

  type gen_rules_result =
    | Under_directory_target of { directory_target_ancestor : Path.Build.t }
    | Normal of Normal.t

  module rec Gen_rules : sig
    val gen_rules : Dir_triage.Build_directory.t -> gen_rules_result Memo.t
  end = struct
    let combine_gen_rules_result ~parent ~child =
      match parent with
      | Under_directory_target { directory_target_ancestor } ->
        Code_error.raise
          "rules under a directory target aren't allowed"
          [ "directory_target_ancestor", Path.Build.to_dyn directory_target_ancestor ]
      | Normal r -> Normal (Normal.combine_exn r child)
    ;;

    let call_rules_generator
          ({ Dir_triage.Build_directory.dir; context_name; context_type = _; sub_dir } as
           d)
      =
      let sub_dir_components = Path.Source.explode sub_dir |> Filename.L.to_string in
      (Build_config.get ()).rule_generator context_name ~dir sub_dir_components
      >>= function
      | Rules rules -> Memo.return @@ Normal (Normal.make_rules_gen_result ~of_:dir rules)
      | Unknown_context ->
        Code_error.raise
          "[gen_rules] did not specify rules for the context"
          [ "context_name", Context_name.to_dyn context_name ]
      | Redirect_to_parent child ->
        (match Dir_triage.Build_directory.parent d with
         | None ->
           Code_error.raise
             "[gen_rules] returned Redirect_to_parent on a root directory"
             [ "context_name", Context_name.to_dyn context_name ]
         | Some parent ->
           let child = Normal.make_rules_gen_result ~of_:dir child in
           let+ parent = Gen_rules.gen_rules parent in
           combine_gen_rules_result ~parent ~child)
    ;;

    let gen_rules_impl d =
      if Dir_triage.Build_directory.is_build_only_root d
      then call_rules_generator d
      else (
        match Dir_triage.Build_directory.parent d with
        | None -> call_rules_generator d
        | Some d' ->
          Gen_rules.gen_rules d'
          >>= (function
           | Under_directory_target _ as res -> Memo.return res
           | Normal rules ->
             if Path.Build.Map.mem rules.directory_targets d.dir
             then
               Memo.return (Under_directory_target { directory_target_ancestor = d.dir })
             else call_rules_generator d))
    ;;

    let gen_rules =
      let memo =
        Memo.create ~input:(module Dir_triage.Build_directory) "gen-rules" gen_rules_impl
      in
      fun x -> Memo.exec memo x
    ;;
  end

  let report_rule_internal_dir_conflict target_name loc =
    User_error.raise
      ~loc
      [ Pp.textf
          "This rule defines a target %S whose name conflicts with an internal directory \
           used by Dune. Please use a different name."
          (Filename.to_string target_name)
      ]
  ;;

  module Source_files_and_dirs = struct
    type t =
      { source_filenames : Filename.Array.Set.t
      ; source_dirs : Filename.Array.Set.t
      }

    let empty =
      { source_filenames = Filename.Array.Set.empty
      ; source_dirs = Filename.Array.Set.empty
      }
    ;;
  end

  let source_files_and_dirs dir =
    let+ source_filenames, source_dirs =
      let module Source_tree = (val (Build_config.get ()).source_tree) in
      Source_tree.find_dir dir
      >>| function
      | None -> Filename.Array.Set.empty, Filename.Array.Set.empty
      | Some dir -> Source_tree.Dir.filenames dir, Source_tree.Dir.sub_dir_names dir
    in
    { Source_files_and_dirs.source_filenames; source_dirs }
  ;;

  let descendants_to_keep
        ({ Dir_triage.Build_directory.dir; context_name = _; context_type; sub_dir } as
         build_dir)
        (build_dir_only_sub_dirs : Subdir_set.t)
        ~source_dirs
        rules_produced
    =
    let* allowed_by_parent =
      match context_type, Path.Source.to_string sub_dir with
      | With_sources _, ".dune" ->
        (* GROSS HACK: this is to avoid a cycle as the rules for all
           directories force the generation of ".dune/configurator". We need a
           better way to deal with such cases. *)
        Memo.return Generated_directory_restrictions.Unrestricted
      | _ ->
        if Dir_triage.Build_directory.is_build_only_root build_dir
        then Memo.return Generated_directory_restrictions.Unrestricted
        else Generated_directory_restrictions.allowed_by_parent ~dir
    in
    let* () =
      match allowed_by_parent with
      | Unrestricted -> Memo.return ()
      | Restricted restriction ->
        (match Path.Build.Map.find (Rules.to_map rules_produced) dir with
         | None -> Memo.return ()
         | Some rules ->
           let+ restriction = Memo.Lazy.force restriction in
           if not (Dir_set.here restriction)
           then
             Code_error.raise
               "Generated rules in a directory not allowed by the parent"
               [ "dir", Path.Build.to_dyn dir
               ; "rules", Rules.Dir_rules.to_dyn rules
               ; "restriction", Dir_set.to_dyn restriction
               ])
    in
    let rules_generated_in =
      Rules.to_map rules_produced
      |> Path.Build.Map.foldi ~init:Dir_set.empty ~f:(fun p _ acc ->
        match Path.Local_gen.descendant ~of_:dir p with
        | None -> acc
        | Some p -> Dir_set.union acc (Dir_set.singleton p))
    in
    let source_dirs_to_keep =
      Filename.Array.Set.fold source_dirs ~init:Dir_set.empty ~f:(fun path acc ->
        let path = Path.Local.relative_fname Path.Local.root path in
        Dir_set.union acc (Dir_set.singleton path))
    in
    let subdirs_to_keep =
      Dir_set.union (Subdir_set.to_dir_set build_dir_only_sub_dirs) source_dirs_to_keep
    in
    let+ allowed_grand_descendants_of_parent =
      match allowed_by_parent with
      | Unrestricted ->
        (* In this case the parent isn't going to be able to create any
           generated grand descendant directories. Rules that attempt to do
           so may run into the [allowed_by_parent] check or will be simply
           ignored. *)
        Memo.return Dir_set.empty
      | Restricted restriction -> Memo.Lazy.force restriction
    in
    Dir_set.union_all
      [ rules_generated_in; subdirs_to_keep; allowed_grand_descendants_of_parent ]
  ;;

  let validate_directory_targets ~dir ~real_directory_targets ~directory_targets =
    if
      not
        (Path.Build.Map.equal real_directory_targets directory_targets ~equal:(fun _ _ ->
           (* The locations should match if the declaration knows which
               rule will generate the directory, but it's not necessary
               as the rule's actual location has higher priority. *)
           true))
    then (
      let mismatched_directories =
        let error message loc =
          Dyn.record [ "message", Dyn.string message; "loc", Loc.to_dyn_hum loc ]
        in
        Path.Build.Map.merge
          real_directory_targets
          directory_targets
          ~f:(fun _ generated declared ->
            match generated, declared with
            | None, None | Some _, Some _ -> None
            | Some loc, None -> Some (error "not declared" loc)
            | None, Some loc -> Some (error "not generated" loc))
      in
      Code_error.raise
        "gen_rules returned a set of directory targets that doesn't match the set of \
         directory targets from returned rules"
        [ "dir", Path.Build.to_dyn dir
        ; "mismatched_directories", Path.Build.Map.to_dyn Fun.id mismatched_directories
        ])
  ;;

  let load_build_directory_exn
        ({ Dir_triage.Build_directory.dir; context_name; context_type; sub_dir } as
         build_dir)
    =
    (* Load all the rules *)
    Gen_rules.gen_rules build_dir
    >>= function
    | Under_directory_target { directory_target_ancestor } ->
      Memo.return (Loaded.Build_under_directory_target { directory_target_ancestor })
    | Normal { rules; build_dir_only_sub_dirs; directory_targets } ->
      let build_dir_only_sub_dirs =
        let declared =
          Path.Build.Map.find build_dir_only_sub_dirs dir
          |> Option.value ~default:Subdir_set.empty
        in
        match context_type, Path.Source.is_root sub_dir with
        | With_sources { build_only_sub_dirs }, true ->
          Subdir_set.union declared (Subdir_set.of_set build_only_sub_dirs)
        | _ -> declared
      in
      Path.Build.Map.iteri directory_targets ~f:(fun dir_target loc ->
        let name = Path.Build.basename dir_target in
        if
          Path.Build.equal (Path.Build.parent_exn dir_target) dir
          && Subdir_set.mem build_dir_only_sub_dirs name
        then report_rule_internal_dir_conflict name loc);
      let* rules_produced = Memo.Lazy.force rules in
      let rules =
        let dir = Path.build dir in
        Rules.find rules_produced dir
      in
      let collected = Rules.Dir_rules.consume rules in
      let rules = collected.rules in
      let from_workspace = Dir_triage.Build_directory.has_workspace_source build_dir in
      let* source_files_and_dirs =
        if from_workspace
        then source_files_and_dirs sub_dir
        else Memo.return Source_files_and_dirs.empty
      in
      let { Source_selection.source_filenames; source_dirs; rules } =
        if from_workspace
        then
          Source_selection.select
            ~dir
            ~source_dir:(Path.source sub_dir)
            ~build_dir_only_sub_dirs
            ~source_filenames:source_files_and_dirs.source_filenames
            ~source_dirs:source_files_and_dirs.source_dirs
            rules
        else
          { Source_selection.source_filenames = source_files_and_dirs.source_filenames
          ; source_dirs = source_files_and_dirs.source_dirs
          ; rules
          }
      in
      let copy_rules =
        let ctx_dir = Context_name.build_dir context_name in
        create_copy_rules
          ~dir:sub_dir
          ~ctx_dir
          ~non_target_source_filenames:source_filenames
      in
      let rules = rules @ copy_rules in
      let* descendants_to_keep =
        descendants_to_keep build_dir build_dir_only_sub_dirs ~source_dirs rules_produced
      in
      let rules_here = compile_rules ~dir ~source_dirs rules in
      validate_directory_targets
        ~dir
        ~real_directory_targets:(Rules.directory_targets rules_produced)
        ~directory_targets;
      (let subdirs_to_keep = Subdir_set.of_dir_set descendants_to_keep in
       remove_old_artifacts ~dir ~rules_here ~subdirs_to_keep;
       remove_old_sub_dirs_in_anonymous_actions_dir
         ~dir:
           (Path.Build.append_local
              Dpath.Build.anonymous_actions_dir
              (Path.Build.local dir))
         ~subdirs_to_keep);
      let+ aliases =
        match context_type with
        | With_sources _ -> compute_alias_expansions ~collected ~dir
        | Empty ->
          (* There are no aliases in contexts without sources *)
          Memo.return Alias.Name.Map.empty
      in
      Loaded.Build { Loaded.allowed_subdirs = descendants_to_keep; rules_here; aliases }
  ;;

  let load_dir_impl ~dir : Loaded.t Memo.t =
    Dune_trace.emit Debug (fun () -> Dune_trace.Event.load_dir dir);
    get_dir_triage ~dir
    >>= function
    | Known l -> Memo.return l
    | Build_directory x -> load_build_directory_exn x
  ;;

  let load_dir =
    let load_dir_impl dir = load_dir_impl ~dir in
    let memo =
      Memo.create_with_store
        "load-dir"
        ~store:(module Path.Table)
        ~input:(module Path)
        load_dir_impl
    in
    fun ~dir -> Memo.exec memo dir
  ;;

  let is_under_directory_target p =
    match Path.parent p with
    | None -> Memo.return false
    | Some dir ->
      get_dir_triage ~dir
      >>= (function
       | Known _ -> Memo.return false
       | Build_directory d ->
         Gen_rules.gen_rules d
         >>| (function
          | Under_directory_target _ -> true
          | Normal { directory_targets; _ } ->
            Path.Build.Map.mem directory_targets (Path.as_in_build_dir_exn p)))
  ;;
end

include Load_rules

let get_rule_internal path =
  let dir = Path.Build.parent_exn path in
  load_dir ~dir:(Path.build dir)
  >>= function
  | External _ | Source _ -> assert false
  | Build { rules_here; _ } ->
    Memo.return
      (match Path.Build.Map.find rules_here.by_file_targets path with
       | Some _ as rule -> rule
       | None -> Path.Build.Map.find rules_here.by_directory_targets path)
  | Build_under_directory_target { directory_target_ancestor } ->
    load_dir ~dir:(Path.build (Path.Build.parent_exn directory_target_ancestor))
    >>= (function
     | External _ | Source _ | Build_under_directory_target _ -> assert false
     | Build { rules_here; _ } ->
       Memo.return
         (Path.Build.Map.find rules_here.by_directory_targets directory_target_ancestor))
;;

let get_rule path =
  match Path.as_in_build_dir path with
  | None -> Memo.return None
  | Some path -> get_rule_internal path
;;

type rule_or_source =
  | Source of Digest.t
  | Rule of Path.Build.t * Rule.t

let get_rule_or_source path =
  match Path.destruct_build_dir path with
  | `Outside path ->
    let+ digest = Fs_memo.file_digest_exn ~loc:Current_rule_loc.get path in
    Source digest
  | `Inside path ->
    get_rule_internal path
    >>= (function
     | Some rule -> Memo.return (Rule (path, rule))
     | None ->
       let* loc = Current_rule_loc.get () in
       no_rule_found ~loc path)
;;

let get_alias_definition alias =
  lookup_alias alias
  >>= function
  | None ->
    let open Pp.O in
    let+ loc = Current_rule_loc.get () in
    User_error.raise ?loc [ Pp.text "No rule found for " ++ Alias.describe alias ]
  | Some x -> Memo.return x
;;

type target_type =
  | File
  | Directory

type is_target =
  | No
  | Yes of target_type
  | Under_directory_target_so_cannot_say

let is_target file =
  match Path.parent file with
  | None -> Memo.return No
  | Some dir ->
    load_dir ~dir
    >>| (function
     | External _ | Source _ -> No
     | Build { rules_here; _ } ->
       let file = Path.as_in_build_dir_exn file in
       (match Path.Build.Map.find rules_here.by_file_targets file with
        | Some _ -> Yes File
        | None ->
          (match Path.Build.Map.find rules_here.by_directory_targets file with
           | Some _ -> Yes Directory
           | None -> No))
     | Build_under_directory_target _ -> Under_directory_target_so_cannot_say)
;;
