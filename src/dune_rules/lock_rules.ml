open Import
open Memo.O

module Spec = struct
  type ('path, 'target) t =
    { target : 'target
    ; lock_dir : string
    ; packages : Dune_pkg.Local_package.t Package.Name.Map.t
    }

  let name = "lock"
  let version = 1
  let bimap t _ g = { t with target = g t.target }
  let is_useful_to ~memoize = memoize

  let encode { target; lock_dir; packages } _encode_path encode_target : Sexp.t =
    let packages : Sexp.t =
      match Dune_pkg.Package_universe.dependency_digest packages with
      | None -> Atom "no packages"
      | Some hash ->
        List [ Atom "hash"; Atom (Dune_pkg.Local_package.Dependency_hash.to_string hash) ]
    in
    Sexp.List [ encode_target target; Sexp.Atom lock_dir; packages ]
  ;;

  let action { target; lock_dir; packages } ~ectx:_ ~eenv:_ =
    let open Fiber.O in
    let+ () = Fiber.return () in
    Printf.eprintf
      "Running ACTION, target is %s, our lock_dir is %S\n"
      (Path.Build.to_string target)
      lock_dir;
    let path = Path.build target in
    Path.mkdir_p path;
    let content =
      Package.Name.Map.values packages
      |> List.map ~f:(fun (s : Dune_pkg.Local_package.t) ->
        s.name |> Package.Name.to_dyn |> Dyn.to_string)
      |> String.concat ~sep:"\n"
    in
    Io.write_file ~binary:true (Path.relative path "lock.dune") content
  ;;
end

module A = Action_ext.Make (Spec)

let action ~packages ~target ~lock_dir = A.action { Spec.target; lock_dir; packages }

let lock ~packages ~target ~lock_dir =
  action ~packages ~target ~lock_dir
  |> Action.Full.make ~can_go_in_shared_cache:false
  |> Action_builder.With_targets.return
  |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
;;

module Gen_rules = Build_config.Gen_rules

let rule ?loc { Action_builder.With_targets.build; targets } =
  (* TODO this ignores the workspace file *)
  Rule.make ~info:(Rule.Info.of_loc_opt loc) ~targets build |> Rules.Produce.rule
;;

let setup_lock_rules ~dir ~lock_dir : Gen_rules.result =
  let target = Path.Build.relative dir "content" in
  let rules =
    Rules.collect_unit (fun () ->
      let* packages =
        Dune_load.packages ()
        >>| Dune_lang.Package.Name.Map.map ~f:Dune_pkg.Local_package.of_package
      in
      let lock_rule = lock ~packages ~target ~lock_dir in
      rule ~loc:Loc.none lock_rule)
  in
  let directory_targets = Path.Build.Map.singleton target Loc.none in
  Gen_rules.make ~directory_targets rules
;;

let setup_rules ~components ~dir =
  match components with
  | [ ".lock" ] ->
    Gen_rules.make
      ~build_dir_only_sub_dirs:
        (Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
      (Memo.return Rules.empty)
    |> Memo.return
  | [ ".lock"; lock_dir ] -> Memo.return @@ setup_lock_rules ~dir ~lock_dir
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
        (* TODO get lock dir name instead of hardcoding `dune.lock` *)
        Path.Build.L.relative
          Private_context.t.build_dir
          [ Context_name.to_string ctx_name; ".lock"; "dune.lock"; "content" ]
      in
      let deps = Action_builder.path (Path.build path) in
      Rules.Produce.Alias.add_deps alias deps)
  in
  Gen_rules.rules_for ~dir ~allowed_subdirs:Filename.Set.empty rule
  |> Gen_rules.rules_here
;;
