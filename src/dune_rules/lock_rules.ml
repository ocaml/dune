open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules

let rule ?loc { Action_builder.With_targets.build; targets } =
  Rule.make ~info:(Rule.Info.of_loc_opt loc) ~targets build |> Rules.Produce.rule
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
  |> Action_builder.with_no_targets
  |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
;;

let setup_copy_rules ~dir:target ~lock_dir =
  let+ deps, files = Source_deps.files (Path.source lock_dir) in
  let directory_targets, rules =
    match Path.Set.is_empty files with
    | true -> Path.Build.Map.empty, Memo.return Rules.empty
    | false ->
      let directory_targets = Path.Build.Map.singleton target Loc.none in
      let rules =
        Rules.collect_unit (fun () ->
          let copy_rule = copy_lock_dir ~target ~lock_dir ~deps ~files in
          rule ~loc:Loc.none copy_rule)
      in
      directory_targets, rules
  in
  Gen_rules.make ~directory_targets rules
;;

let setup_lock_rules ~dir ~lock_dir =
  let* workspace = Workspace.workspace () in
  let dir = Path.Build.relative dir lock_dir in
  let lock_dir = Path.Source.relative workspace.dir lock_dir in
  setup_copy_rules ~dir ~lock_dir
;;

let setup_dev_tool_lock_rules ~dir dev_tool =
  let package_name = Dune_pkg.Dev_tool.package_name dev_tool in
  let dev_tool_name = Dune_lang.Package_name.to_string package_name in
  let dir = Path.Build.relative dir dev_tool_name in
  let lock_dir = Lock_dir.dev_tool_source_lock_dir dev_tool in
  setup_copy_rules ~dir ~lock_dir
;;

let setup_rules ~components ~dir =
  let empty = Gen_rules.rules_here Gen_rules.Rules.empty in
  match components with
  | [ ".lock" ] ->
    (* TODO enable other lock dirs too, by reading them from the workspace *)
    setup_lock_rules ~dir ~lock_dir:"dune.lock"
  | [ ".dev-tool-locks" ] ->
    Memo.List.fold_left
      Dune_pkg.Dev_tool.all
      ~f:(fun rules dev_tool ->
        let+ dev_tool_rules = setup_dev_tool_lock_rules ~dir dev_tool in
        Gen_rules.combine rules dev_tool_rules)
      ~init:empty
  | [] ->
    let sub_dirs = [ ".lock"; ".dev-tool-locks" ] in
    let build_dir_only_sub_dirs =
      Gen_rules.Build_only_sub_dirs.singleton ~dir @@ Subdir_set.of_list sub_dirs
    in
    Memo.return @@ Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty)
  | _ -> Memo.return empty
;;
