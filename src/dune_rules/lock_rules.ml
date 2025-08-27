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
      ; files : Path.Source.Set.t
      }

    let name = "copy-lock-dir"
    let version = 1
    let bimap t _ g = { t with target = g t.target }
    let is_useful_to ~memoize = memoize

    let encode { target; lock_dir; files } _encode_path encode_target : Sexp.t =
      let contents : Sexp.t list =
        files
        |> Path.Source.Set.to_list
        |> List.sort ~compare:Path.Source.compare
        |> List.map ~f:(fun p -> Sexp.Atom (Path.Source.to_string p))
      in
      List [ encode_target target; Atom (Path.Source.to_string lock_dir); List contents ]
    ;;

    let action { target; lock_dir = _; files } ~ectx:_ ~eenv:_ =
      let open Fiber.O in
      let+ () = Fiber.return () in
      Path.mkdir_p (Path.build target);
      Path.Source.Set.iter files ~f:(fun src ->
        let dst =
          match Path.Source.explode src with
          | [] ->
            Code_error.raise
              "Somehow the path is unexpected"
              [ "src", Path.Source.to_dyn src ]
          | _ :: components -> Path.Build.L.relative target components
        in
        let parent = Path.Build.parent_exn dst in
        (* Printf.eprintf "Source to copy %S => %S (parent %S)\n" (Path.Source.to_string src) (Path.Build.to_string dst) (Path.Build.to_string parent); *)
        Path.mkdir_p (Path.build parent);
        Io.copy_file ~src:(Path.source src) ~dst:(Path.build dst) ())
    ;;
  end

  module A = Action_ext.Make (Spec)
end

let copy_lock_dir ~target ~lock_dir ~files =
  Copy.A.action { Copy.Spec.target; lock_dir; files }
  |> Action.Full.make ~can_go_in_shared_cache:false
  |> Action_builder.With_targets.return
  |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
;;

let setup_copy_rules ~dir:target ~lock_dir =
  let+ _deps, file_set = Source_deps.files (Path.source lock_dir) in
  let directory_targets, rules =
    match Path.Set.is_empty file_set with
    | true -> Path.Build.Map.empty, Memo.return Rules.empty
    | false ->
      let directory_targets = Path.Build.Map.singleton target Loc.none in
      let rules =
        Rules.collect_unit (fun () ->
          let files =
            Path.Set.fold
              ~init:Path.Source.Set.empty
              ~f:(fun e acc ->
                match (e : Path.t) with
                | In_source_tree p -> Path.Source.Set.add acc p
                | In_build_dir b ->
                  Code_error.raise
                    "Source deps returned build paths"
                    [ "path", Path.Build.to_dyn b ]
                | External e ->
                  Code_error.raise
                    "Source deps returned external path"
                    [ "path", Path.External.to_dyn e ])
              file_set
          in
          let copy_rule = copy_lock_dir ~target ~lock_dir ~files in
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

let setup_rules ~components ~dir =
  match components with
  | [ ".lock" ] ->
    (* TODO enable other lock dirs too, by reading them from the workspace *)
    setup_lock_rules ~dir ~lock_dir:"dune.lock"
  | [] ->
    let sub_dirs = [ ".lock" ] in
    let build_dir_only_sub_dirs =
      Gen_rules.Build_only_sub_dirs.singleton ~dir @@ Subdir_set.of_list sub_dirs
    in
    Memo.return @@ Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty)
  | _ -> Memo.return @@ Gen_rules.rules_here Gen_rules.Rules.empty
;;
