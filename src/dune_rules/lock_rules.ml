open Import
open Memo.O

module Spec = struct
  type ('path, 'target) t =
    { target : 'target
    ; lock_dir : string
    ; projects : Dune_project.t list
    }

  let name = "lock"
  let version = 1
  let bimap t _ g = { t with target = g t.target }
  let is_useful_to ~memoize = memoize

  let encode { target; lock_dir; projects = _ } _encode_path encode_target : Sexp.t =
    Sexp.List [ encode_target target; Sexp.Atom lock_dir ]
  ;;

  let action { target; lock_dir; projects } ~ectx:_ ~eenv:_ =
    let open Fiber.O in
    let+ () = Fiber.return () in
    Printf.eprintf
      "Running ACTION, target is %s, our lock_dir is %S\n"
      (Path.Build.to_string target)
      lock_dir;
    let path = Path.build target in
    Path.mkdir_p path;
    let content =
      projects
      |> List.map ~f:(fun project ->
        let pkgs = Dune_project.packages project in
        Dune_lang.Package_name.Map.to_dyn Package.to_dyn pkgs
      |>
        Dyn.to_string)
      |> String.concat ~sep:"\n"
    in
    Io.write_file ~binary:true (Path.relative path "lock.dune") content
  ;;
end

module A = Action_ext.Make (Spec)

let action ~projects ~target ~lock_dir = A.action { Spec.target; lock_dir; projects }

let lock ~projects ~target ~lock_dir =
  action ~projects ~target ~lock_dir
  |> Action.Full.make ~can_go_in_shared_cache:false
  |> Action_builder.With_targets.return
  |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
;;

module Gen_rules = Build_config.Gen_rules

let rule ?loc { Action_builder.With_targets.build; targets } =
  (* TODO this ignores the workspace file *)
  Rule.make ~info:(Rule.Info.of_loc_opt loc) ~targets build |> Rules.Produce.rule
;;

let setup_lock_rules ~dir ~lock_dir ~projects : Gen_rules.result =
  let target = Path.Build.relative dir "content" in
  let gen_rules projects lock_dir =
    let lock_rule = lock ~projects ~target ~lock_dir in
    rule ~loc:Loc.none lock_rule
  in
  let rules =
    Rules.collect_unit (fun () ->
      (* deref Memo to create dependency on project *)
      let* projects = projects in
      gen_rules projects lock_dir)
  in
  let directory_targets = Path.Build.Map.singleton target Loc.none in
  Gen_rules.make ~directory_targets rules
;;

let setup_rules ~components ~dir =
  let projects = Dune_load.projects () in
  match components with
  | [ ".lock" ] ->
    Gen_rules.make
      ~build_dir_only_sub_dirs:
        (Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
      (Memo.return Rules.empty)
    |> Memo.return
  | [ ".lock"; lock_dir ] -> Memo.return @@ setup_lock_rules ~dir ~lock_dir ~projects
  | [] ->
    let sub_dirs = [ ".lock" ] in
    let build_dir_only_sub_dirs =
      Gen_rules.Build_only_sub_dirs.singleton ~dir @@ Subdir_set.of_list sub_dirs
    in
    Memo.return @@ Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty)
  | _ -> Memo.return @@ Gen_rules.rules_here Gen_rules.Rules.empty
;;

let setup_tmp_lock_alias =
  fun ~dir ctx_name ->
  let alias = Alias.make ~dir Alias0.pkg_lock in
  let rule =
    Rules.collect_unit (fun () ->
      (* careful, need to point to a file that will be created by the rule *)
      let path =
        let ( / ) = Path.Build.relative in
        (* TODO get lock dir name instead of hardcoding `dune.lock` *)
        Private_context.t.build_dir
        / Context_name.to_string ctx_name
        / ".lock"
        / "dune.lock"
        / "content"
      in
      let deps = Action_builder.path (Path.build path) in
      Rules.Produce.Alias.add_deps alias deps)
  in
  Gen_rules.rules_for ~dir ~allowed_subdirs:Filename.Set.empty rule
  |> Gen_rules.rules_here
;;
