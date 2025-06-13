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
      "Running ACTION, target is %s, our lock_dir is %S\n"
      (Path.Build.to_string target)
      lock_dir;
    let path = Path.build target in
    Path.mkdir_p path;
    let t = Unix.localtime @@ Unix.gettimeofday () in
    let content =
      sprintf
        "Created on %d-%0.2d-%0.2d %0.2d:%0.2d:%0.2d"
        (t.tm_year + 1900)
        (t.tm_mon + 1)
        t.tm_mday
        t.tm_hour
        t.tm_min
        t.tm_sec
    in
    Io.write_file ~binary:true (Path.relative path "lock.dune") content;
    ()
  ;;
end

module A = Action_ext.Make (Spec)

let action ~target ~lock_dir = A.action { Spec.target; lock_dir }

let lock ~target ~lock_dir =
  action ~target ~lock_dir
  |> Action.Full.make ~can_go_in_shared_cache:false
  |> Action_builder.With_targets.return
  |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
;;

module Gen_rules = Build_config.Gen_rules

let rule ?loc { Action_builder.With_targets.build; targets } =
  (* TODO this ignores the workspace file *)
  Rule.make ~info:(Rule.Info.of_loc_opt loc) ~targets build |> Rules.Produce.rule
;;

let setup_lock_rules ctx_name ~lock_dir : Gen_rules.result =
  let sctx = Super_context.find_exn ctx_name in
  let target =
    let ( / ) = Path.Build.relative in
    Private_context.t.build_dir
    / Context_name.to_string ctx_name
    / ".lock"
    / lock_dir
    / "content"
  in
  let gen_rules lock_dir =
    let lock_rule = lock ~target ~lock_dir in
    rule ~loc:Loc.none lock_rule
  in
  let rules =
    Rules.collect_unit (fun () ->
      (* deref Memo to create dependency on project *)
      let* _sctx = sctx in
      gen_rules lock_dir)
  in
  let directory_targets = Path.Build.Map.singleton target Loc.none in
  Gen_rules.make ~directory_targets rules
;;

let setup_rules ~components ~dir ctx_name =
  match components with
  | [ ".lock" ] ->
    Gen_rules.make
      ~build_dir_only_sub_dirs:
        (Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
      (Memo.return Rules.empty)
    |> Memo.return
  | [ ".lock"; lock_dir ] -> Memo.return @@ setup_lock_rules ctx_name ~lock_dir
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
