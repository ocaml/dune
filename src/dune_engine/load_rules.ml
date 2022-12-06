open Import
open Memo.O
module Action_builder = Action_builder0
module Context_or_install = Build_config.Context_or_install

module Current_rule_loc = struct
  let t = ref (fun () -> Memo.return None)

  let set f = t := f

  let get () = !t ()
end

let set_current_rule_loc = Current_rule_loc.set

module Loaded = struct
  type rules_here =
    { by_file_targets : Rule.t Path.Build.Map.t
    ; by_directory_targets : Rule.t Path.Build.Map.t
    }

  let no_rules_here =
    { by_file_targets = Path.Build.Map.empty
    ; by_directory_targets = Path.Build.Map.empty
    }

  type build =
    { allowed_subdirs : Path.Unspecified.w Dir_set.t
    ; rules_here : rules_here
    ; aliases : (Loc.t * Rules.Dir_rules.Alias_spec.item) list Alias.Name.Map.t
    }

  type t =
    | Source of { files : Path.Source.Set.t }
    | External of { files : Path.External.Set.t }
    | Build of build
    | Build_under_directory_target of
        { directory_target_ancestor : Path.Build.t }

  let no_rules ~allowed_subdirs =
    Build
      { allowed_subdirs
      ; rules_here = no_rules_here
      ; aliases = Alias.Name.Map.empty
      }
end

module Dir_triage = struct
  module Build_directory = struct
    (* invariant: [dir = context_or_install / sub_dir] *)
    type t =
      { dir : Path.Build.t
      ; context_or_install : Context_or_install.t
      ; sub_dir : Path.Source.t
      }

    (* It's ok to only compare and hash the [dir] field because of the
       invariant. *)
    let equal a b = Path.Build.equal a.dir b.dir

    let hash t = Path.Build.hash t.dir

    let to_dyn t = Path.Build.to_dyn t.dir

    let parent t =
      Option.map (Path.Source.parent t.sub_dir) ~f:(fun sub_dir ->
          { dir = Path.Build.parent_exn t.dir
          ; context_or_install = t.context_or_install
          ; sub_dir
          })
  end

  type t =
    | Known of Loaded.t
    | Build_directory of Build_directory.t
end

let get_dir_triage ~dir =
  match Dpath.analyse_dir dir with
  | Source dir ->
    let+ files = Source_tree.files_of dir in
    Dir_triage.Known (Source { files })
  | External dir_ext ->
    let+ files =
      Fs_memo.dir_contents (External dir_ext) >>| function
      | Error (Unix.ENOENT, _, _) -> Path.External.Set.empty
      | Error unix_error ->
        User_warning.emit
          [ Pp.textf "Unable to read %s" (Path.to_string_maybe_quoted dir)
          ; Unix_error.Detailed.pp ~prefix:"Reason: " unix_error
          ];
        Path.External.Set.empty
      | Ok filenames ->
        let filenames =
          Fs_cache.Dir_contents.to_list filenames
          |> List.filter_map ~f:(fun (name, kind) ->
                 match kind with
                 | Unix.S_DIR -> None
                 | _ -> Some name)
        in
        Path.External.Set.of_listing ~dir:dir_ext ~filenames
    in
    Dir_triage.Known (External { files })
  | Build (Regular Root) ->
    let+ contexts = Memo.Lazy.force (Build_config.get ()).contexts in
    let allowed_subdirs =
      Subdir_set.to_dir_set
        (Subdir_set.of_list
           (([ Dpath.Build.anonymous_actions_dir; Dpath.Build.install_dir ]
            |> List.map ~f:Path.Build.basename)
           @ (Context_name.Map.keys contexts
             |> List.map ~f:Context_name.to_string)))
    in
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs)
  | Build (Install Root) ->
    let+ contexts = Memo.Lazy.force (Build_config.get ()).contexts in
    let allowed_subdirs =
      Context_name.Map.keys contexts
      |> List.map ~f:Context_name.to_string
      |> Subdir_set.of_list |> Subdir_set.to_dir_set
    in
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs)
  | Build (Anonymous_action p) ->
    let build_dir = Dpath.Target_dir.build_dir p in
    Code_error.raise "Called get_dir_triage on an anonymous action directory"
      [ ("dir", Path.Build.to_dyn build_dir) ]
  | Build (Invalid _) ->
    Memo.return
    @@ Dir_triage.Known (Loaded.no_rules ~allowed_subdirs:Dir_set.empty)
  | Build (Install (With_context (context_name, sub_dir))) ->
    (* In this branch, [dir] is in the build directory. *)
    let dir = Path.as_in_build_dir_exn dir in
    let context_or_install = Context_or_install.Install context_name in
    Memo.return
      (Dir_triage.Build_directory { dir; context_or_install; sub_dir })
  | Build (Regular (With_context (context_name, sub_dir))) ->
    (* In this branch, [dir] is in the build directory. *)
    let dir = Path.as_in_build_dir_exn dir in
    let context_or_install = Context_or_install.Context context_name in
    Memo.return
      (Dir_triage.Build_directory { dir; context_or_install; sub_dir })

let describe_rule (rule : Rule.t) =
  match rule.info with
  | From_dune_file { start; _ } ->
    start.pos_fname ^ ":" ^ string_of_int start.pos_lnum
  | Internal -> "<internal location>"
  | Source_file_copy _ -> "file present in source tree"

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
  User_error.raise ~loc
    [ Pp.textf
        "This rule defines a target %S whose name conflicts with a source \
         directory in the same directory."
        (Path.Build.basename fn)
    ]
    ~hints:
      [ Pp.textf
          "If you want Dune to generate and replace %S, add (mode promote) to \
           the rule stanza. Alternatively, you can delete %S from the source \
           tree or change the rule to generate a different target."
          (Path.Build.basename fn) (Path.Build.basename fn)
      ]

let report_rule_conflict fn (rule' : Rule.t) (rule : Rule.t) =
  let fn = Path.build fn in
  User_error.raise
    [ Pp.textf "Multiple rules generated for %s:"
        (Path.to_string_maybe_quoted fn)
    ; Pp.textf "- %s" (describe_rule rule')
    ; Pp.textf "- %s" (describe_rule rule)
    ]
    ~hints:
      (match (rule.info, rule'.info) with
      | Source_file_copy _, _ | _, Source_file_copy _ ->
        [ Pp.textf "rm -f %s"
            (Path.to_string_maybe_quoted (Path.drop_optional_build_context fn))
        ]
      | _ -> [])

let remove_old_artifacts ~dir ~(rules_here : Loaded.rules_here)
    ~(subdirs_to_keep : Subdir_set.t) =
  match Path.Untracked.readdir_unsorted_with_kinds (Path.build dir) with
  | Error _ -> ()
  | Ok files ->
    List.iter files ~f:(fun (fn, kind) ->
        let path = Path.Build.relative dir fn in
        let path_is_a_target =
          Path.Build.Map.mem rules_here.by_file_targets path
          || Path.Build.Map.mem rules_here.by_directory_targets path
        in
        if not path_is_a_target then
          match kind with
          | Unix.S_DIR -> (
            match subdirs_to_keep with
            | All -> ()
            | These set ->
              if not (String.Set.mem set fn) then Path.rm_rf (Path.build path))
          | _ -> Path.unlink (Path.build path))

(* We don't remove files in there as we don't know upfront if they are stale or
   not. *)
let remove_old_sub_dirs_in_anonymous_actions_dir ~dir
    ~(subdirs_to_keep : Subdir_set.t) =
  match Path.Untracked.readdir_unsorted_with_kinds (Path.build dir) with
  | Error _ -> ()
  | Ok files ->
    List.iter files ~f:(fun (fn, kind) ->
        let path = Path.Build.relative dir fn in
        match kind with
        | Unix.S_DIR -> (
          match subdirs_to_keep with
          | All -> ()
          | These set ->
            if not (String.Set.mem set fn) then Path.rm_rf (Path.build path))
        | _ -> ())

let no_rule_found ~loc fn =
  let+ contexts = Memo.Lazy.force (Build_config.get ()).contexts in
  let fail fn ~loc =
    User_error.raise ?loc
      [ Pp.textf "No rule found for %s" (Dpath.describe_target fn) ]
  in
  let hints ctx =
    let candidates =
      Context_name.Map.keys contexts |> List.map ~f:Context_name.to_string
    in
    User_message.did_you_mean (Context_name.to_string ctx) ~candidates
  in
  match Dpath.analyse_target fn with
  | Other _ -> fail fn ~loc
  | Regular (ctx, _) ->
    if Context_name.Map.mem contexts ctx then fail fn ~loc
    else
      User_error.raise
        [ Pp.textf "Trying to build %s but build context %s doesn't exist."
            (Path.Build.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Install (ctx, _) ->
    if Context_name.Map.mem contexts ctx then fail fn ~loc
    else
      User_error.raise
        [ Pp.textf
            "Trying to build %s for install but build context %s doesn't exist."
            (Path.Build.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Alias (ctx, fn') ->
    if Context_name.Map.mem contexts ctx then fail fn ~loc
    else
      let fn =
        Path.append_source (Path.build (Context_name.build_dir ctx)) fn'
      in
      User_error.raise
        [ Pp.textf
            "Trying to build alias %s but build context %s doesn't exist."
            (Path.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Anonymous_action _ ->
    (* We never lookup such actions by target name, so this should be
       unreachable *)
    Code_error.raise ?loc "Build_system.no_rule_found got anonymous action path"
      [ ("fn", Path.Build.to_dyn fn) ]

let source_or_external_file_digest path =
  let report_user_error details =
    let+ loc = Current_rule_loc.get () in
    User_error.raise ?loc
      ([ Pp.textf "File unavailable: %s"
           (Path.Outside_build_dir.to_string_maybe_quoted path)
       ]
      @ details)
  in
  Fs_memo.file_digest path >>= function
  | Ok digest -> Memo.return digest
  | No_such_file -> report_user_error []
  | Broken_symlink -> report_user_error [ Pp.text "Broken symbolic link" ]
  | Cyclic_symlink -> report_user_error [ Pp.text "Cyclic symbolic link" ]
  | Unexpected_kind st_kind ->
    report_user_error
      [ Pp.textf "This is not a regular file (%s)" (File_kind.to_string st_kind)
      ]
  | Unix_error unix_error ->
    report_user_error [ Unix_error.Detailed.pp ~prefix:"Reason: " unix_error ]
  | Error exn -> report_user_error [ Pp.textf "%s" (Printexc.to_string exn) ]

let eval_source_file : type a. a Action_builder.eval_mode -> Path.t -> a Memo.t
    =
 fun mode path ->
  let path_outside_build_dir = Path.as_outside_build_dir_exn path in
  match mode with
  | Lazy -> Memo.return ()
  | Eager ->
    let+ d = source_or_external_file_digest path_outside_build_dir in
    Dep.Fact.file path d

module rec Load_rules : sig
  val load_dir : dir:Path.t -> Loaded.t Memo.t

  val is_under_directory_target : Path.t -> bool Memo.t

  val lookup_alias :
    Alias.t -> (Loc.t * Rules.Dir_rules.Alias_spec.item) list option Memo.t

  val alias_exists : Alias.t -> bool Memo.t
end = struct
  open Load_rules

  let create_copy_rules ~ctx_dir ~non_target_source_files =
    Path.Source.Set.to_list_map non_target_source_files ~f:(fun path ->
        let ctx_path = Path.Build.append_source ctx_dir path in
        let build =
          Action_builder.of_thunk
            { f =
                (fun mode ->
                  let path = Path.source path in
                  let+ fact = eval_source_file mode path in
                  ( Action.Full.make
                      (Action.copy path ctx_path)
                      (* There's an [assert false] in [prepare_managed_paths]
                         that blows up if we try to sandbox this. *)
                      ~sandbox:Sandbox_config.no_sandboxing
                  , Dep.Map.singleton (Dep.file path) fact ))
            }
        in
        Rule.make ~context:None ~info:(Source_file_copy path)
          ~targets:(Targets.File.create ctx_path)
          build)

  let compile_rules ~dir ~source_dirs rules =
    let file_targets, directory_targets =
      let check_for_source_dir_conflict rule target =
        if String.Set.mem source_dirs (Path.Build.basename target) then
          report_rule_src_dir_conflict dir target rule
      in
      List.map rules ~f:(fun rule ->
          assert (Path.Build.( = ) dir rule.Rule.dir);
          ( Path.Build.Set.to_list_map rule.targets.files ~f:(fun target ->
                check_for_source_dir_conflict rule target;
                (target, rule))
          , Path.Build.Set.to_list_map rule.targets.dirs ~f:(fun target ->
                check_for_source_dir_conflict rule target;
                (target, rule)) ))
      |> List.unzip
    in
    let by_file_targets =
      List.concat file_targets
      |> Path.Build.Map.of_list_reducei ~f:report_rule_conflict
    in
    let by_directory_targets =
      List.concat directory_targets
      |> Path.Build.Map.of_list_reducei ~f:report_rule_conflict
    in
    Path.Build.Map.iter2 by_file_targets by_directory_targets
      ~f:(fun target rule1 rule2 ->
        match (rule1, rule2) with
        | None, _ | _, None -> ()
        | Some rule1, Some rule2 -> report_rule_conflict target rule1 rule2);
    { Loaded.by_file_targets; by_directory_targets }

  let lookup_alias alias =
    load_dir ~dir:(Path.build (Alias.dir alias)) >>| function
    | Source _ | External _ ->
      Code_error.raise "Alias in a non-build dir"
        [ ("alias", Alias.to_dyn alias) ]
    | Build { aliases; _ } -> Alias.Name.Map.find aliases (Alias.name alias)
    | Build_under_directory_target _ -> None

  let alias_exists alias =
    lookup_alias alias >>| function
    | None -> false
    | Some _ -> true

  let compute_alias_expansions ~(collected : Rules.Dir_rules.ready) ~dir =
    let aliases = collected.aliases in
    let+ aliases =
      if Alias.Name.Map.mem aliases Alias.Name.default then Memo.return aliases
      else
        (Build_config.get ()).implicit_default_alias dir >>| function
        | None -> aliases
        | Some expansion ->
          Alias.Name.Map.set aliases Alias.Name.default
            { expansions =
                Appendable_list.singleton
                  (Loc.none, Rules.Dir_rules.Alias_spec.Deps expansion)
            }
    in
    Alias.Name.Map.map aliases
      ~f:(fun { Rules.Dir_rules.Alias_spec.expansions } ->
        Appendable_list.to_list expansions)

  let filter_out_fallback_rules ~to_copy rules =
    List.filter rules ~f:(fun (rule : Rule.t) ->
        match rule.mode with
        | Standard | Promote _ | Ignore_source_files -> true
        | Fallback ->
          let source_files_for_targets =
            if not (Path.Build.Set.is_empty rule.targets.dirs) then
              Code_error.raise "Unexpected directory target in a Fallback rule"
                [ ("targets", Targets.Validated.to_dyn rule.targets) ];
            Path.Build.Set.to_list_map
              rule.targets.files
              (* All targets are in a directory of a build context since there
                 are source files to copy, so this call can't fail. *)
              ~f:Path.Build.drop_build_context_exn
            |> Path.Source.Set.of_list
          in
          if Path.Source.Set.is_subset source_files_for_targets ~of_:to_copy
          then (* All targets are present *)
            false
          else if
            Path.Source.Set.is_empty
              (Path.Source.Set.inter source_files_for_targets to_copy)
          then (* No target is present *)
            true
          else
            let absent_targets =
              Path.Source.Set.diff source_files_for_targets to_copy
            in
            let present_targets =
              Path.Source.Set.diff source_files_for_targets absent_targets
            in
            User_error.raise ~loc:(Rule.loc rule)
              [ Pp.text
                  "Some of the targets of this fallback rule are present in \
                   the source tree, and some are not. This is not allowed. \
                   Either none of the targets must be present in the source \
                   tree, either they must all be."
              ; Pp.nop
              ; Pp.text "The following targets are present:"
              ; Pp.enumerate ~f:Path.pp
                  (Path.set_of_source_paths present_targets |> Path.Set.to_list)
              ; Pp.nop
              ; Pp.text "The following targets are not:"
              ; Pp.enumerate ~f:Path.pp
                  (Path.set_of_source_paths absent_targets |> Path.Set.to_list)
              ])

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

    let corresponding_source_dir ~dir =
      match Dpath.analyse_target dir with
      | Install _ | Alias _ | Anonymous_action _ | Other _ -> Memo.return None
      | Regular (_ctx, sub_dir) -> Source_tree.find_dir sub_dir

    let source_subdirs_of_build_dir ~dir =
      corresponding_source_dir ~dir >>| function
      | None -> String.Set.empty
      | Some dir -> Source_tree.Dir.sub_dir_names dir

    let allowed_dirs ~dir ~subdir : restriction Memo.t =
      let+ subdirs = source_subdirs_of_build_dir ~dir in
      if String.Set.mem subdirs subdir then Unrestricted
      else
        Restricted
          (Memo.Lazy.create ~name:"allowed_dirs" (fun () ->
               load_dir ~dir:(Path.build dir) >>| function
               | External _ | Source _ -> Dir_set.just_the_root
               | Build { allowed_subdirs; _ } ->
                 Dir_set.descend allowed_subdirs subdir
               | Build_under_directory_target _ -> Dir_set.empty))

    let allowed_by_parent ~dir =
      allowed_dirs
        ~dir:(Path.Build.parent_exn dir)
        ~subdir:(Path.Build.basename dir)
  end

  type gen_rules_result =
    | Under_directory_target of { directory_target_ancestor : Path.Build.t }
    | Normal of
        { build_dir_only_sub_dirs : Subdir_set.t
        ; directory_targets : Loc.t Path.Build.Map.t
        ; rules : Rules.t Memo.Lazy.t
        }

  module rec Gen_rules : sig
    val gen_rules : Dir_triage.Build_directory.t -> gen_rules_result Memo.t
  end = struct
    let check_all_directory_targets_are_descendant ~of_:dir directory_targets =
      Path.Build.Map.iteri directory_targets ~f:(fun p _loc ->
          if not (Path.Build.is_descendant p ~of_:dir) then
            Code_error.raise
              "[gen_rules] returned directory target in a directory that is \
               not a descendant of the directory it was called for"
              [ ("dir", Path.Build.to_dyn dir)
              ; ("example", Path.Build.to_dyn p)
              ])

    let check_all_rules_are_descendant ~of_:dir rules =
      match
        Path.Build.Map.find_key (Rules.to_map rules) ~f:(fun p ->
            not (Path.Build.is_descendant p ~of_:dir))
      with
      | None -> ()
      | Some p ->
        let dir_rules =
          Rules.find rules (Path.build p) |> Rules.Dir_rules.consume
        in
        Code_error.raise
          "[gen_rules] returned rules in a directory that is not a descendant \
           of the directory it was called for"
          [ ("dir", Path.Build.to_dyn dir)
          ; ( "example"
            , match dir_rules with
              | { rules = r :: _; _ } ->
                Dyn.Variant
                  ( "Rule"
                  , [ Dyn.Record
                        [ ("targets", Targets.Validated.to_dyn r.targets) ]
                    ] )
              | { rules = []; aliases } -> (
                match Alias.Name.Map.choose aliases with
                | None -> assert false
                | Some (name, _) ->
                  Dyn.Variant
                    ( "Alias"
                    , [ Dyn.Record
                          [ ("dir", Path.Build.to_dyn p)
                          ; ("name", Alias.Name.to_dyn name)
                          ]
                      ] )) )
          ]

    let call_rules_generator
        ({ Dir_triage.Build_directory.dir; context_or_install; sub_dir } as d) =
      let (module RG : Build_config.Rule_generator) =
        (Build_config.get ()).rule_generator
      in
      let sub_dir_components = Path.Source.explode sub_dir in
      RG.gen_rules context_or_install ~dir sub_dir_components >>= function
      | Rules { build_dir_only_sub_dirs; directory_targets; rules } ->
        check_all_directory_targets_are_descendant ~of_:dir directory_targets;
        let rules =
          Memo.lazy_ (fun () ->
              let+ rules = rules in
              check_all_rules_are_descendant ~of_:dir rules;
              rules)
        in
        Memo.return
          (Normal { build_dir_only_sub_dirs; directory_targets; rules })
      | Unknown_context_or_install ->
        Code_error.raise "[gen_rules] did not specify rules for the context"
          [ ("context_or_install", Context_or_install.to_dyn context_or_install)
          ]
      | Redirect_to_parent -> (
        match Dir_triage.Build_directory.parent d with
        | None ->
          Code_error.raise
            "[gen_rules] returned Redirect_to_parent on a root directory"
            [ ( "context_or_install"
              , Context_or_install.to_dyn context_or_install )
            ]
        | Some d' -> Gen_rules.gen_rules d')

    let gen_rules_impl d =
      match Dir_triage.Build_directory.parent d with
      | None -> call_rules_generator d
      | Some d' -> (
        Gen_rules.gen_rules d' >>= function
        | Under_directory_target _ as res -> Memo.return res
        | Normal rules ->
          if Path.Build.Map.mem rules.directory_targets d.dir then
            Memo.return
              (Under_directory_target { directory_target_ancestor = d.dir })
          else call_rules_generator d)

    let gen_rules =
      let memo =
        Memo.create
          ~input:(module Dir_triage.Build_directory)
          "gen-rules" gen_rules_impl
      in
      fun x -> Memo.exec memo x
  end

  let report_rule_internal_dir_conflict target_name loc =
    User_error.raise ~loc
      [ Pp.textf
          "This rule defines a target %S whose name conflicts with an internal \
           directory used by Dune. Please use a different name."
          target_name
      ]

  let load_build_directory_exn
      ({ Dir_triage.Build_directory.dir; context_or_install; sub_dir } as
      build_dir) =
    (* Load all the rules *)
    let (module RG : Build_config.Rule_generator) =
      (Build_config.get ()).rule_generator
    in
    Gen_rules.gen_rules build_dir >>= function
    | Under_directory_target { directory_target_ancestor } ->
      Memo.return
        (Loaded.Build_under_directory_target { directory_target_ancestor })
    | Normal { rules; build_dir_only_sub_dirs; directory_targets } ->
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
      (* Compute the set of sources and targets promoted to the source tree that
         must not be copied to the build directory. *)
      let source_files_to_ignore, source_dirnames_to_ignore =
        List.fold_left rules ~init:(Path.Build.Set.empty, String.Set.empty)
          ~f:(fun (acc_files, acc_dirnames) { Rule.targets; mode; loc; _ } ->
            let target_filenames =
              Path.Build.Set.to_list_map ~f:Path.Build.basename targets.files
              |> String.Set.of_list
            in
            let target_dirnames =
              Path.Build.Set.to_list_map ~f:Path.Build.basename targets.dirs
              |> String.Set.of_list
            in
            (* Check if this rule defines any file targets that conflict with
               internal Dune directories listed in [build_dir_only_sub_dirs]. We
               don't check directory targets as these are already checked
               earlier. *)
            (match
               String.Set.choose
                 (Subdir_set.inter_set build_dir_only_sub_dirs target_filenames)
             with
            | None -> ()
            | Some target_name ->
              report_rule_internal_dir_conflict target_name loc);
            match mode with
            | Ignore_source_files ->
              ( Path.Build.Set.union acc_files targets.files
              , String.Set.union acc_dirnames target_dirnames )
            | Promote { only; _ } ->
              (* Note that the [only] predicate applies to the files inside the
                 directory targets rather than to directory names themselves. *)
              let target_files =
                match only with
                | None -> targets.files
                | Some pred ->
                  let is_promoted file =
                    Predicate.test pred
                      (Path.reach (Path.build file) ~from:(Path.build dir))
                  in
                  Path.Build.Set.filter targets.files ~f:is_promoted
              in
              ( Path.Build.Set.union acc_files target_files
              , String.Set.union acc_dirnames target_dirnames )
            | Standard | Fallback -> (acc_files, acc_dirnames))
      in
      (* Take into account the source files *)
      let* to_copy, source_dirs =
        match context_or_install with
        | Install _ -> Memo.return (None, String.Set.empty)
        | Context context_name ->
          let+ files, subdirs =
            Source_tree.find_dir sub_dir >>| function
            | None -> (Path.Source.Set.empty, String.Set.empty)
            | Some dir ->
              (Source_tree.Dir.file_paths dir, Source_tree.Dir.sub_dir_names dir)
          in
          let files =
            let source_files_to_ignore =
              Path.Build.Set.to_list_map ~f:Path.Build.drop_build_context_exn
                source_files_to_ignore
              |> Path.Source.Set.of_list
            in
            let source_files_to_ignore =
              Target_promotion.delete_stale_dot_merlin_file ~dir
                ~source_files_to_ignore
            in
            Path.Source.Set.diff files source_files_to_ignore
          in
          let subdirs = String.Set.diff subdirs source_dirnames_to_ignore in
          if Path.Source.Set.is_empty files then (None, subdirs)
          else
            let ctx_path = Context_name.build_dir context_name in
            (Some (ctx_path, files), subdirs)
      in
      (* Filter out fallback rules *)
      let rules =
        match to_copy with
        | None ->
          (* If there are no source files to copy, fallback rules are
             automatically kept *)
          rules
        | Some (_, to_copy) -> filter_out_fallback_rules ~to_copy rules
      in
      (* Compile the rules and cleanup stale artifacts *)
      let rules =
        (match to_copy with
        | None -> []
        | Some (ctx_dir, source_files) ->
          create_copy_rules ~ctx_dir ~non_target_source_files:source_files)
        @ rules
      in
      let* allowed_by_parent =
        match (context_or_install, Path.Source.to_string sub_dir) with
        | Context _, ".dune" ->
          (* GROSS HACK: this is to avoid a cycle as the rules for all
             directories force the generation of ".dune/configurator". We need a
             better way to deal with such cases. *)
          Memo.return Generated_directory_restrictions.Unrestricted
        | _ -> Generated_directory_restrictions.allowed_by_parent ~dir
      in
      let* () =
        match allowed_by_parent with
        | Unrestricted -> Memo.return ()
        | Restricted restriction -> (
          match Path.Build.Map.find (Rules.to_map rules_produced) dir with
          | None -> Memo.return ()
          | Some rules ->
            let+ restriction = Memo.Lazy.force restriction in
            if not (Dir_set.here restriction) then
              Code_error.raise
                "Generated rules in a directory not allowed by the parent"
                [ ("dir", Path.Build.to_dyn dir)
                ; ("rules", Rules.Dir_rules.to_dyn rules)
                ])
      in
      let* descendants_to_keep =
        let rules_generated_in =
          Rules.to_map rules_produced
          |> Path.Build.Map.foldi ~init:Dir_set.empty ~f:(fun p _ acc ->
                 match Path.Local_gen.descendant ~of_:dir p with
                 | None -> acc
                 | Some p -> Dir_set.union acc (Dir_set.singleton p))
        in
        let subdirs_to_keep =
          match build_dir_only_sub_dirs with
          | All -> Subdir_set.All
          | These set -> These (String.Set.union source_dirs set)
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
          [ rules_generated_in
          ; Subdir_set.to_dir_set subdirs_to_keep
          ; allowed_grand_descendants_of_parent
          ]
      in
      let subdirs_to_keep = Subdir_set.of_dir_set descendants_to_keep in
      let rules_here = compile_rules ~dir ~source_dirs rules in
      let () =
        let real_directory_targets = Rules.directory_targets rules_produced in
        if
          not
            (Path.Build.Map.equal real_directory_targets directory_targets
               ~equal:(fun _ _ ->
                 (* The locations should match if the declaration knows which
                    rule will generate the directory, but it's not necessary
                    as the rule's actual location has higher priority. *)
                 true))
        then
          let mismatched_directories =
            let error message loc =
              Dyn.record
                [ ("message", Dyn.string message); ("loc", Loc.to_dyn_hum loc) ]
            in
            Path.Build.Map.merge real_directory_targets directory_targets
              ~f:(fun _ generated declared ->
                match (generated, declared) with
                | None, None | Some _, Some _ -> None
                | Some loc, None -> Some (error "not declared" loc)
                | None, Some loc -> Some (error "not generated" loc))
          in
          Code_error.raise
            "gen_rules returned a set of directory targets that doesn't match \
             the set of directory targets from returned rules"
            [ ("dir", Path.Build.to_dyn dir)
            ; ( "mismatched_directories"
              , Path.Build.Map.to_dyn Fun.id mismatched_directories )
            ]
      in
      remove_old_artifacts ~dir ~rules_here ~subdirs_to_keep;
      remove_old_sub_dirs_in_anonymous_actions_dir
        ~dir:
          (Path.Build.append_local Dpath.Build.anonymous_actions_dir
             (Path.Build.local dir))
        ~subdirs_to_keep;
      let+ aliases =
        match context_or_install with
        | Context _ -> compute_alias_expansions ~collected ~dir
        | Install _ ->
          (* There are no aliases in the [_build/install] directory *)
          Memo.return Alias.Name.Map.empty
      in
      Loaded.Build
        { Loaded.allowed_subdirs = descendants_to_keep; rules_here; aliases }

  let load_dir_impl ~dir : Loaded.t Memo.t =
    if !Clflags.debug_load_dir then
      Console.print_user_message
        (User_message.make
           [ Pp.textf "Loading build directory %s" (Path.to_string dir) ]);
    get_dir_triage ~dir >>= function
    | Known l -> Memo.return l
    | Build_directory x -> load_build_directory_exn x

  let load_dir =
    let load_dir_impl dir = load_dir_impl ~dir in
    let memo = Memo.create "load-dir" ~input:(module Path) load_dir_impl in
    fun ~dir -> Memo.exec memo dir

  let is_under_directory_target p =
    match Path.parent p with
    | None -> Memo.return false
    | Some dir -> (
      get_dir_triage ~dir >>= function
      | Known _ -> Memo.return false
      | Build_directory d -> (
        Gen_rules.gen_rules d >>| function
        | Under_directory_target _ -> true
        | Normal { directory_targets; _ } ->
          Path.Build.Map.mem directory_targets (Path.as_in_build_dir_exn p)))
end

include Load_rules

let get_rule_internal path =
  let dir = Path.Build.parent_exn path in
  load_dir ~dir:(Path.build dir) >>= function
  | External _ | Source _ -> assert false
  | Build { rules_here; _ } -> (
    match Path.Build.Map.find rules_here.by_file_targets path with
    | Some _ as rule -> Memo.return rule
    | None ->
      Memo.return (Path.Build.Map.find rules_here.by_directory_targets path))
  | Build_under_directory_target { directory_target_ancestor } -> (
    load_dir ~dir:(Path.build (Path.Build.parent_exn directory_target_ancestor))
    >>= function
    | External _ | Source _ | Build_under_directory_target _ -> assert false
    | Build { rules_here; _ } ->
      Memo.return
        (Path.Build.Map.find rules_here.by_directory_targets
           directory_target_ancestor))

let get_rule path =
  match Path.as_in_build_dir path with
  | None -> Memo.return None
  | Some path -> get_rule_internal path

type rule_or_source =
  | Source of Digest.t
  | Rule of Path.Build.t * Rule.t

let get_rule_or_source path =
  match Path.destruct_build_dir path with
  | `Outside path ->
    let+ d = source_or_external_file_digest path in
    Source d
  | `Inside path -> (
    get_rule_internal path >>= function
    | Some rule -> Memo.return (Rule (path, rule))
    | None ->
      let* loc = Current_rule_loc.get () in
      no_rule_found ~loc path)

type target_type =
  | File
  | Directory

module All_targets = struct
  type t = target_type Path.Build.Map.t

  include Monoid.Make (struct
    type nonrec t = t

    let empty = Path.Build.Map.empty

    let combine = Path.Build.Map.union_exn
  end)
end

module Source_tree_map_reduce =
  Source_tree.Dir.Make_map_reduce (Memo) (All_targets)

let all_direct_targets dir =
  let* root =
    match dir with
    | None -> Source_tree.root ()
    | Some dir -> Source_tree.nearest_dir dir
  and* contexts = Memo.Lazy.force (Build_config.get ()).contexts in
  Memo.parallel_map (Context_name.Map.values contexts) ~f:(fun ctx ->
      Source_tree_map_reduce.map_reduce root ~traverse:Sub_dirs.Status.Set.all
        ~f:(fun dir ->
          load_dir
            ~dir:
              (Path.build
                 (Path.Build.append_source ctx.build_dir
                    (Source_tree.Dir.path dir)))
          >>| function
          | External _ | Source _ -> All_targets.empty
          | Build { rules_here; _ } ->
            All_targets.combine
              (Path.Build.Map.map rules_here.by_file_targets ~f:(fun _ -> File))
              (Path.Build.Map.map rules_here.by_directory_targets ~f:(fun _ ->
                   Directory))
          | Build_under_directory_target _ -> All_targets.empty))
  >>| All_targets.reduce

let get_alias_definition alias =
  lookup_alias alias >>= function
  | None ->
    let open Pp.O in
    let+ loc = Current_rule_loc.get () in
    User_error.raise ?loc
      [ Pp.text "No rule found for " ++ Alias.describe alias ]
  | Some x -> Memo.return x

type is_target =
  | No
  | Yes of target_type
  | Under_directory_target_so_cannot_say

let is_target file =
  match Path.parent file with
  | None -> Memo.return No
  | Some dir -> (
    load_dir ~dir >>| function
    | External _ | Source _ -> No
    | Build { rules_here; _ } -> (
      let file = Path.as_in_build_dir_exn file in
      match Path.Build.Map.find rules_here.by_file_targets file with
      | Some _ -> Yes File
      | None -> (
        match Path.Build.Map.find rules_here.by_directory_targets file with
        | Some _ -> Yes Directory
        | None -> No))
    | Build_under_directory_target _ -> Under_directory_target_so_cannot_say)
