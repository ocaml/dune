open Import
open Memo.O

module Spec = struct
  type ('path, 'target) t =
    { target : 'target
    ; lock_dir : string
    }

  let name = "lock"
  let version = 1
  let bimap t _ g = { t with target = g t.target }
  let is_useful_to ~memoize = memoize

  let encode { target; lock_dir } _encode_path encode_target : Sexp.t =
    Sexp.List [ encode_target target; Sexp.Atom lock_dir ]
  ;;

  let action { target; lock_dir } ~ectx:_ ~eenv:_ =
    let open Fiber.O in
    let+ () = Fiber.return () in
    Printf.eprintf
      "Our ACTION target is %s, our lock_dir is %S\n"
      (Path.Build.to_string target)
      lock_dir;
    let path = Path.build target in
    Path.mkdir_p path;
    Io.write_file ~binary:true (Path.relative path "lock.dune") "Hello I exist";
    ()
  ;;
end

module A = Action_ext.Make (Spec)

let action ~target ~lock_dir = A.action { Spec.target; lock_dir }

let lock ~target ~lock_dir =
  action ~target ~lock_dir
  |> Action.Full.make ~can_go_in_shared_cache:true
  |> Action_builder.With_targets.return
  |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
;;

module Gen_rules = Build_config.Gen_rules

let lock_rule ~target lock_dir = lock ~target ~lock_dir |> Memo.return

let rule ?loc { Action_builder.With_targets.build; targets } =
  (* TODO this ignores the workspace file *)
  Rule.make ~info:(Rule.Info.of_loc_opt loc) ~targets build |> Rules.Produce.rule
;;

let setup_lock_rules ctx_name ~lock_dir : Gen_rules.result =
  let target =
    let ( / ) = Path.Build.relative in
    Private_context.t.build_dir
    / Context_name.to_string ctx_name
    / ".lock"
    / lock_dir
    / "content"
  in
  let gen_rules lock_dir =
    let* lock_rule = lock_rule ~target lock_dir in
    rule ~loc:Loc.none lock_rule
  in
  let rules = Rules.collect_unit (fun () -> gen_rules lock_dir) in
  let directory_targets = Path.Build.Map.singleton target Loc.none in
  Gen_rules.make ~directory_targets rules
;;

let setup_rules ~components ~dir ctx =
  match components with
  | [ ".lock" ] ->
    Gen_rules.make
      ~build_dir_only_sub_dirs:
        (Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
      (Memo.return Rules.empty)
    |> Memo.return
  | [ ".lock"; lock_dir ] -> Memo.return @@ setup_lock_rules ctx ~lock_dir
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
