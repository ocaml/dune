open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules

let action_builder_with_dir_targets ~directory_targets =
  let targets =
    Targets.create
      ~files:Path.Build.Set.empty
      ~dirs:(Path.Build.Set.of_list directory_targets)
  in
  Action_builder.with_targets ~targets
;;

let copy_lock_dir ~target ~lock_dir ~deps ~files =
  let open Action_builder.O in
  Action_builder.deps deps
  >>> (Path.Set.to_list_map files ~f:(fun src ->
         let suffix = Path.drop_prefix_exn src ~prefix:(Path.source lock_dir) in
         let dst = Path.Build.append_local target suffix in
         let parent = Path.Build.parent_exn dst in
         Action.progn [ Action.mkdir parent; Action.copy src dst ])
       |> Action.concurrent
       |> Action.Full.make
       |> Action_builder.return)
  |> action_builder_with_dir_targets ~directory_targets:[ target ]
;;

let setup_copy_rules ~dir:target ~lock_dir =
  let+ deps, files = Source_deps.files (Path.source lock_dir) in
  let directory_targets, rules =
    match Path.Set.is_empty files with
    | true -> Path.Build.Map.empty, Rules.empty
    | false ->
      let directory_targets = Path.Build.Map.singleton target Loc.none in
      let { Action_builder.With_targets.build; targets } =
        copy_lock_dir ~target ~lock_dir ~deps ~files
      in
      let rule = Rule.make ~targets build in
      directory_targets, Rules.of_rules [ rule ]
  in
  Gen_rules.make ~directory_targets (Memo.return rules)
;;

let setup_lock_rules (workspace : Workspace.t) ~dir ~lock_dir =
  let dir = Path.Build.append_local dir lock_dir in
  let lock_dir = Path.Source.append_local workspace.dir lock_dir in
  setup_copy_rules ~dir ~lock_dir
;;

let setup_dev_tool_lock_rules ~dir dev_tool =
  let package_name = Dune_pkg.Dev_tool.package_name dev_tool in
  let dev_tool_name = Dune_lang.Package_name.to_string package_name in
  let dir = Path.Build.relative dir dev_tool_name in
  let lock_dir = Lock_dir.dev_tool_source_lock_dir dev_tool in
  setup_copy_rules ~dir ~lock_dir
;;

let lock_dirs_of_workspace (workspace : Workspace.t) =
  let module Set = Path.Source.Set in
  let+ lock_dirs_from_ctx =
    Memo.List.map workspace.contexts ~f:(function
      | Opam _ | Default { lock_dir = None; _ } -> Memo.return None
      | Default { lock_dir = Some selection; _ } ->
        let+ path = Lock_dir.select_lock_dir selection in
        Some path)
    >>| List.filter_opt
  in
  match lock_dirs_from_ctx, workspace.lock_dirs with
  | [], [] -> Set.singleton Lock_dir.default_source_path
  | lock_dirs_from_ctx, lock_dirs_from_toplevel ->
    let lock_paths_from_toplevel =
      List.map lock_dirs_from_toplevel ~f:(fun (lock_dir : Workspace.Lock_dir.t) ->
        lock_dir.path)
    in
    Set.union (Set.of_list lock_paths_from_toplevel) (Set.of_list lock_dirs_from_ctx)
;;

let setup_rules ~components ~dir =
  let empty = Gen_rules.rules_here Gen_rules.Rules.empty in
  match components with
  | [ ".lock" ] ->
    let* workspace = Workspace.workspace () in
    lock_dirs_of_workspace workspace
    >>| Path.Source.Set.to_list
    >>= Memo.List.fold_left ~init:empty ~f:(fun rules lock_dir_path ->
      let lock_dir = Path.Source.to_local lock_dir_path in
      let+ lock_rule = setup_lock_rules workspace ~dir ~lock_dir in
      Gen_rules.combine rules lock_rule)
  | [ ".dev-tool-locks" ] ->
    Memo.List.fold_left Dune_pkg.Dev_tool.all ~init:empty ~f:(fun rules dev_tool ->
      let+ dev_tool_rules = setup_dev_tool_lock_rules ~dir dev_tool in
      Gen_rules.combine rules dev_tool_rules)
  | [] ->
    let sub_dirs = [ ".lock"; ".dev-tool-locks" ] in
    let build_dir_only_sub_dirs =
      Gen_rules.Build_only_sub_dirs.singleton ~dir @@ Subdir_set.of_list sub_dirs
    in
    Memo.return @@ Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty)
  | _ -> Memo.return empty
;;
