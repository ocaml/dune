open Import
open Memo.O

module Alias_rules = struct
  let add sctx ~alias ~loc build =
    let dir = Alias.dir alias in
    Super_context.add_alias_action sctx alias ~dir ~loc build
  ;;

  let add_empty sctx ~loc ~alias =
    let action = Action_builder.return (Action.Full.make Action.empty) in
    add sctx ~loc ~alias action
  ;;
end

let check_filename =
  let not_in_dir ~kind ~error_loc s =
    User_error.raise
      ~loc:error_loc
      [ (match kind with
         | Targets_spec.Kind.File ->
           Pp.textf "%S does not denote a file in the current directory." s
         | Directory -> Pp.textf "Directory targets must have exactly one path component.")
      ]
  in
  fun ~kind ~error_loc ~dir -> function
    | Value.String ("." | "..") ->
      User_error.raise ~loc:error_loc [ Pp.text "'.' and '..' are not valid targets" ]
    | String s ->
      if Filename.dirname s <> Filename.current_dir_name
      then not_in_dir ~kind ~error_loc s;
      Path.Build.relative ~error_loc dir s
    | Path p ->
      (match Option.equal Path.equal (Path.parent p) (Some (Path.build dir)) with
       | true -> Path.as_in_build_dir_exn p
       | false -> not_in_dir ~kind ~error_loc (Path.to_string p))
    | Dir p -> not_in_dir ~kind ~error_loc (Path.to_string p)
;;

type rule_kind =
  | Aliases_only of Alias.Name.t list
  | Aliases_with_targets of Alias.Name.t list * Path.Build.t
  | No_alias

let rule_kind ~(rule : Rule_conf.t) ~(action : _ Action_builder.With_targets.t) =
  match rule.aliases with
  | [] -> No_alias
  | aliases ->
    (match Targets.head action.targets with
     | None -> Aliases_only aliases
     | Some target -> Aliases_with_targets (aliases, target))
;;

let interpret_and_add_locks ~expander locks action =
  let open Action_builder.O in
  Expander.expand_locks expander locks
  >>= function
  | [] -> action
  | locks -> Action_builder.map action ~f:(Action.Full.add_locks locks)
;;

let add_user_rule
  sctx
  ~dir
  ~(rule : Rule_conf.t)
  ~(action : _ Action_builder.With_targets.t)
  ~expander
  =
  let action =
    let build = interpret_and_add_locks ~expander rule.locks action.build in
    { action with Action_builder.With_targets.build }
  in
  Super_context.add_rule_get_targets sctx ~dir ~mode:rule.mode ~loc:rule.loc action
;;

let user_rule sctx ?extra_bindings ~dir ~expander (rule : Rule_conf.t) =
  Expander.eval_blang expander rule.enabled_if
  >>= function
  | false ->
    let+ () =
      Memo.parallel_iter rule.aliases ~f:(fun alias ->
        let alias = Alias.make ~dir alias in
        Alias_rules.add_empty sctx ~loc:rule.loc ~alias)
    in
    None
  | true ->
    let* targets =
      match rule.targets with
      | Infer -> Memo.return Targets_spec.Infer
      | Static { targets; multiplicity } ->
        let+ targets =
          Memo.List.concat_map targets ~f:(fun (target, kind) ->
            (match multiplicity with
             | One ->
               Expander.No_deps.expand expander ~mode:Single target >>| List.singleton
             | Multiple -> Expander.No_deps.expand expander ~mode:Many target)
            >>|
            let error_loc = String_with_vars.loc target in
            List.map ~f:(fun value -> check_filename ~kind ~dir ~error_loc value, kind))
        in
        Targets_spec.Static { multiplicity; targets }
    in
    let expander =
      match extra_bindings with
      | None -> expander
      | Some bindings -> Expander.add_bindings expander ~bindings
    in
    let* action =
      let+ (action : _ Action_builder.With_targets.t) =
        let chdir = Expander.dir expander in
        Action_unexpanded.expand
          (snd rule.action)
          ~loc:(fst rule.action)
          ~chdir
          ~expander
          ~deps:rule.deps
          ~targets
          ~targets_dir:dir
      in
      if rule.patch_back_source_tree
      then
        Action_builder.With_targets.map action ~f:(fun action ->
          (* Here we expect that [action.sandbox] is [Sandbox_config.default]
             because the parsing of [rule] stanzas forbids having both a
             sandboxing setting in [deps] and a [patch_back_source_tree] field
             at the same time.

             If we didn't have this restriction and [action.sandbox] was
             something that didn't permit [Some Patch_back_source_tree], Dune
             would crash in a way that would be difficult for the user to
             understand. *)
          Action.Full.add_sandbox Sandbox_mode.Set.patch_back_source_tree_only action)
      else action
    in
    (match rule_kind ~rule ~action with
     | No_alias ->
       let+ targets = add_user_rule sctx ~dir ~rule ~action ~expander in
       Some targets
     | Aliases_with_targets (aliases, alias_target) ->
       let+ () =
         Memo.parallel_iter aliases ~f:(fun alias ->
           let alias = Alias.make ~dir alias in
           Rules.Produce.Alias.add_deps
             alias
             (Action_builder.path (Path.build alias_target)))
       and+ targets = add_user_rule sctx ~dir ~rule ~action ~expander in
       Some targets
     | Aliases_only aliases ->
       let+ () =
         let action = interpret_and_add_locks ~expander rule.locks action.build in
         Memo.parallel_iter aliases ~f:(fun alias ->
           let alias = Alias.make ~dir alias in
           Alias_rules.add sctx ~alias ~loc:rule.loc action)
       in
       None)
;;

let copy_files sctx ~dir ~expander ~src_dir (def : Copy_files.t) =
  let loc = String_with_vars.loc def.files in
  let* glob_in_src =
    let+ src_glob = Expander.No_deps.expand_str expander def.files in
    if Filename.is_relative src_glob
    then Path.relative (Path.source src_dir) src_glob ~error_loc:loc
    else (
      let since = 2, 7 in
      if def.syntax_version < since
      then
        Dune_lang.Syntax.Error.since
          loc
          Stanza.syntax
          since
          ~what:(sprintf "%s is an absolute path. This" src_glob);
      Path.external_ (Path.External.of_string src_glob))
  in
  let since = 1, 3 in
  if def.syntax_version < since
     && not (Path.is_descendant glob_in_src ~of_:(Path.source src_dir))
  then
    Dune_lang.Syntax.Error.since
      loc
      Stanza.syntax
      since
      ~what:
        (sprintf
           "%s is not a sub-directory of %s. This"
           (Path.to_string_maybe_quoted glob_in_src)
           (Path.Source.to_string_maybe_quoted src_dir));
  let src_in_src = Path.parent_exn glob_in_src in
  let glob = Path.basename glob_in_src |> Glob.of_string_exn loc in
  let src_in_build =
    match Path.as_in_source_tree src_in_src with
    | None -> src_in_src
    | Some src_in_src ->
      let context = Super_context.context sctx in
      Path.Build.append_source (Context.build_dir context) src_in_src |> Path.build
  in
  let* exists_or_generated =
    match src_in_src with
    | In_build_dir _ -> assert false
    | External ext -> Fs_memo.dir_exists (External ext)
    | In_source_tree src_in_src ->
      Source_tree.find_dir src_in_src
      >>= (function
       | Some _ -> Memo.return true
       | None -> Load_rules.is_under_directory_target src_in_build)
  in
  if not exists_or_generated
  then
    User_error.raise
      ~loc
      [ Pp.textf "Cannot find directory: %s" (Path.to_string src_in_src) ];
  if Path.equal src_in_src (Path.source src_dir)
  then
    User_error.raise
      ~loc
      [ Pp.textf
          "Cannot copy files onto themselves. The format is <dir>/<glob> where <dir> is \
           not the current directory."
      ];
  (* add rules *)
  let* only_sources = Expander.eval_blang expander def.only_sources in
  let* files =
    let dir =
      match only_sources with
      | true -> src_in_src
      | false -> src_in_build
    in
    Build_system.eval_pred (File_selector.of_glob ~dir glob)
  in
  (* CR-someday amokhov: We currently traverse the set [files] twice: first, to
     add the corresponding rules, and then to convert the files to [targets]. To
     do only one traversal we need [Memo.parallel_map_set]. *)
  let* () =
    Memo.parallel_iter_set
      (module Filename.Set)
      (Filename_set.filenames files)
      ~f:(fun basename ->
        let file_src = Path.relative src_in_build basename in
        let file_dst = Path.Build.relative dir basename in
        let context = Super_context.context sctx in
        Super_context.add_rule
          sctx
          ~loc
          ~dir
          ~mode:def.mode
          ((if def.add_line_directive
            then Copy_line_directive.builder context
            else Action_builder.copy)
             ~src:file_src
             ~dst:file_dst))
  in
  let targets =
    Filename.Set.to_list_map (Filename_set.filenames files) ~f:(fun basename ->
      let file_dst = Path.Build.relative dir basename in
      Path.build file_dst)
    |> Path.Set.of_list
  in
  let+ () =
    Memo.Option.iter def.alias ~f:(fun alias ->
      let alias = Alias.make alias ~dir in
      Rules.Produce.Alias.add_deps alias (Action_builder.path_set targets))
  in
  targets
;;

let copy_files sctx ~dir ~expander ~src_dir (def : Copy_files.t) =
  Expander.eval_blang expander def.enabled_if
  >>= function
  | true -> copy_files sctx ~dir ~expander ~src_dir def
  | false -> Memo.return Path.Set.empty
;;

let alias sctx ?extra_bindings ~dir ~expander (alias_conf : Alias_conf.t) =
  let alias = Alias.make ~dir alias_conf.name in
  let loc = alias_conf.loc in
  Expander.eval_blang expander alias_conf.enabled_if
  >>= function
  | false -> Alias_rules.add_empty sctx ~loc ~alias
  | true ->
    (match alias_conf.action with
     | None ->
       let builder, _expander, _sandbox = Dep_conf_eval.named ~expander alias_conf.deps in
       Rules.Produce.Alias.add_deps alias ~loc builder
     | Some (action_loc, action) ->
       let action =
         let expander =
           match extra_bindings with
           | None -> expander
           | Some bindings -> Expander.add_bindings expander ~bindings
         in
         let chdir = Expander.dir expander in
         Action_unexpanded.expand_no_targets
           action
           ~loc:action_loc
           ~expander
           ~chdir
           ~deps:alias_conf.deps
           ~what:"aliases"
       in
       interpret_and_add_locks ~expander alias_conf.locks action
       |> Alias_rules.add sctx ~loc ~alias)
;;
