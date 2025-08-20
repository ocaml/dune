open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules

let rule ?loc { Action_builder.With_targets.build; targets } =
  Rule.make ~info:(Rule.Info.of_loc_opt loc) ~targets build |> Rules.Produce.rule
;;

module Copy = struct
  module Spec = struct
    type ('path, 'target) t =
      { target : 'target
      ; lock_dir : Path.Source.t
      ; contents : Fs_cache.Dir_contents.t
      }

    let name = "copy-lock-dir"
    let version = 1
    let bimap t _ g = { t with target = g t.target }
    let is_useful_to ~memoize = memoize

    let encode { target; lock_dir; contents } _encode_path encode_target : Sexp.t =
      let contents : Sexp.t list =
        contents
        |> Fs_cache.Dir_contents.to_list
        |> List.map ~f:(fun (filename, filekind) ->
          Sexp.List
            [ Atom filename; Atom (filekind |> File_kind.to_dyn |> Dyn.to_string) ])
      in
      List [ encode_target target; Atom (Path.Source.to_string lock_dir); List contents ]
    ;;

    let action { target; lock_dir; contents } ~ectx:_ ~eenv:_ =
      let open Fiber.O in
      let+ () = Fiber.return () in
      Path.mkdir_p (Path.build target);
      Fs_cache.Dir_contents.iter contents ~f:(fun (filename, _file_kind) ->
        let src = Path.Source.relative lock_dir filename in
        let dst = Path.Build.relative target filename in
        Io.copy_file ~src:(Path.source src) ~dst:(Path.build dst) ())
    ;;
  end

  module A = Action_ext.Make (Spec)
end

let copy_lock_dir ~target ~lock_dir ~contents =
  Copy.A.action { Copy.Spec.target; lock_dir; contents }
  |> Action.Full.make ~can_go_in_shared_cache:false
  |> Action_builder.With_targets.return
  |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
;;

let setup_copy_rules ~dir ~lock_dir =
  let target = Path.Build.relative dir "content" in
  let rules =
    Rules.collect_unit (fun () ->
      let* dir_contents =
        Fs_memo.dir_contents (Path.Outside_build_dir.In_source_dir lock_dir)
      in
      let contents =
        match dir_contents with
        | Ok dir_contents -> dir_contents
        | Error unix_error ->
          User_error.raise
            [ Pp.text "Failed to read lock directory from source tree"
            ; Pp.text (Unix_error.Detailed.to_string_hum unix_error)
            ]
      in
      let copy_rule = copy_lock_dir ~target ~lock_dir ~contents in
      rule ~loc:Loc.none copy_rule)
  in
  let directory_targets = Path.Build.Map.singleton target Loc.none in
  Gen_rules.make ~directory_targets rules
;;

let setup_lock_rules ~dir ~lock_dir =
  let+ workspace = Workspace.workspace () in
  let lock_dir = Path.Source.relative workspace.dir lock_dir in
  setup_copy_rules ~dir ~lock_dir
;;

let setup_rules ~components ~dir =
  match components with
  | [ ".lock" ] ->
    Gen_rules.make
      ~build_dir_only_sub_dirs:
        (Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
      (Memo.return Rules.empty)
    |> Memo.return
  | [ ".lock"; lock_dir ] -> setup_lock_rules ~dir ~lock_dir
  | [] ->
    let sub_dirs = [ ".lock" ] in
    let build_dir_only_sub_dirs =
      Gen_rules.Build_only_sub_dirs.singleton ~dir @@ Subdir_set.of_list sub_dirs
    in
    Memo.return @@ Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty)
  | _ -> Memo.return @@ Gen_rules.rules_here Gen_rules.Rules.empty
;;
