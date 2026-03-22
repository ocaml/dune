open Import
module Diff = Dune_util.Action.Diff

let compare_file_paths = function
  | Diff.Mode.Binary -> Io.compare_files
  | Text -> Io.compare_text_files
;;

type kind =
  | Missing
  | File
  | Directory

type file_diff =
  { source_file : Path.Source.t
  ; file1 : Path.t
  ; file2 : Path.t
  }

type change =
  | File_diff of file_diff
  | Message of User_message.Style.t Pp.t list

type promotion =
  | Promote_file of
      { source_file : Path.Source.t
      ; correction_file : Path.Build.t
      }
  | Delete of [ `File | `Directory ] * Path.Source.t
  | Create_directory of Path.Source.t

type plan =
  { changes : change list
  ; promotions : promotion list
  }

let empty_plan = { changes = []; promotions = [] }
let add_change plan change = { plan with changes = change :: plan.changes }
let add_promotion plan promotion = { plan with promotions = promotion :: plan.promotions }

let remove_intermediate_target path =
  match Fpath.unlink (Path.Build.to_string path) with
  | Success | Does_not_exist -> ()
  | Is_a_directory -> Path.rm_rf (Path.build path)
  | Error e ->
    User_error.raise
      [ Pp.textf
          "Failed to remove intermediate target %s"
          (Path.Build.to_string_maybe_quoted path)
      ; Exn.pp e
      ]
;;

let is_copied_from_source_tree file =
  match Path.extract_build_context_dir_maybe_sandboxed file with
  | None -> false
  | Some (_, file) ->
    (* CR-someday rgrinberg: isn't this racy? *)
    Fpath.exists (Path.to_string (Path.source file))
;;

let in_source_or_target file =
  is_copied_from_source_tree file || not (Fpath.exists (Path.to_string file))
;;

let source_root file =
  Path.as_in_build_dir_exn file |> Path.Build.drop_build_context_maybe_sandboxed_exn
;;

let promotion source_file =
  { User_message.Diff_annot.in_source = source_file
  ; in_build = Diff_promotion.File.in_staging_area source_file
  }
;;

let run_change loc ~patch_back (mode : Diff.Mode.t) = function
  | Message messages -> User_error.raise ~loc messages
  | File_diff { source_file; file1; file2 } ->
    (match mode with
     | Text ->
       Print_diff.print
         ~patch_back
         (promotion source_file)
         file1
         file2
         ~skip_trailing_cr:Sys.win32
     | Binary ->
       User_error.raise
         ~promotion:(promotion source_file)
         ~loc
         [ Pp.textf
             "Files %s and %s differ."
             (Path.to_string_maybe_quoted file1)
             (Path.to_string_maybe_quoted file2)
         ])
;;

let register_promotions how promotions =
  List.iter promotions ~f:(function
    | Promote_file { source_file; correction_file } ->
      Diff_promotion.register_intermediate how ~source_file ~correction_file
    | Delete (what, source_file) -> Diff_promotion.register_delete what source_file
    | Create_directory source_dir -> Diff_promotion.register_create_directory source_dir)
;;

let kind_of_path ~loc path =
  match Path.Untracked.stat path with
  | Error ((ENOENT | ENOTDIR), _, _) -> Missing
  | Ok { st_kind = S_REG; _ } -> File
  | Ok { st_kind = S_DIR; _ } -> Directory
  | Ok { st_kind; _ } ->
    User_error.raise
      ~loc
      [ Pp.textf
          "Unsupported path kind %S in directory diff for %s"
          (File_kind.to_string_hum st_kind)
          (Path.to_string_maybe_quoted path)
      ]
  | Error e ->
    User_error.raise
      ~loc
      [ Pp.textf "Unable to stat %s" (Path.to_string_maybe_quoted path)
      ; Unix_error.Detailed.pp_reason e
      ]
;;

let list_directory ~loc path =
  match Path.Untracked.readdir_unsorted_with_kinds path with
  | Ok entries ->
    List.sort entries ~compare:(fun (x, _) (y, _) -> Filename.compare x y)
    |> List.map ~f:fst
  | Error e ->
    User_error.raise
      ~loc
      [ Pp.textf "Unable to read directory %s" (Path.to_string_maybe_quoted path)
      ; Unix_error.Detailed.pp_reason e
      ]
;;

type tree_diff =
  { loc : Loc.t
  ; mode : Diff.Mode.t
  ; source_root : Path.Source.t
  ; source_root_path : Path.t
  ; target_dir : Path.Build.t
  }

let plan_tree_diff ({ mode; source_root; _ } as t) =
  let source_file rel = Path.Source.append_local source_root rel in
  let source_path rel =
    if rel = Path.Local.root then t.source_root_path else Path.source (source_file rel)
  in
  let target_path rel = Path.build (Path.Build.append_local t.target_dir rel) in
  let kind_of_source rel = kind_of_path ~loc:t.loc (source_path rel) in
  let kind_of_target rel = kind_of_path ~loc:t.loc (target_path rel) in
  let list_target_directory rel = list_directory ~loc:t.loc (target_path rel) in
  let path_name rel = Path.Source.to_string_maybe_quoted (source_file rel) in
  let add_message plan message = add_change plan (Message [ message ]) in
  let add_promote_file plan rel =
    add_promotion
      plan
      (Promote_file
         { source_file = source_file rel
         ; correction_file = Path.Build.append_local t.target_dir rel
         })
  in
  let add_file_diff plan rel =
    let source_file = source_file rel in
    let plan = add_promote_file plan rel in
    let change =
      File_diff { source_file; file1 = source_path rel; file2 = target_path rel }
    in
    add_change plan change
  in
  let add_delete plan what rel =
    let plan = add_promotion plan (Delete (what, source_file rel)) in
    let what_text =
      match what with
      | `File -> "File"
      | `Directory -> "Directory"
    in
    add_message plan (Pp.textf "%s %s should be deleted" what_text (path_name rel))
  in
  let add_create_directory ~announce plan rel =
    let plan = add_promotion plan (Create_directory (source_file rel)) in
    if announce
    then add_message plan (Pp.textf "Directory %s should be created" (path_name rel))
    else plan
  in
  let rec collect_target_only rel target_kind plan =
    match target_kind with
    | Missing -> plan
    | File -> add_promote_file plan rel
    | Directory ->
      list_target_directory rel
      |> List.fold_left
           ~init:(add_create_directory ~announce:false plan rel)
           ~f:(fun plan name ->
             let rel = Path.Local.relative rel name in
             collect_target_only rel (kind_of_target rel) plan)
  and loop rel source_kind target_kind plan =
    match source_kind, target_kind with
    | Missing, Missing -> plan
    | File, File ->
      (match compare_file_paths mode (source_path rel) (target_path rel) with
       | Eq -> plan
       | _ -> add_file_diff plan rel)
    | Missing, File -> add_file_diff plan rel
    | Directory, File ->
      let plan = add_promote_file plan rel in
      add_message
        plan
        (Pp.textf "Directory %s should be replaced with a file" (path_name rel))
    | File, Missing -> add_delete plan `File rel
    | Directory, Missing -> add_delete plan `Directory rel
    | Missing, Directory ->
      let entries = list_target_directory rel in
      let plan = add_create_directory ~announce:(List.is_empty entries) plan rel in
      if List.is_empty entries
      then plan
      else
        List.fold_left entries ~init:plan ~f:(fun plan name ->
          let rel = Path.Local.relative rel name in
          loop rel Missing (kind_of_target rel) plan)
    | File, Directory ->
      list_target_directory rel
      |> List.fold_left
           ~init:
             (let plan = add_create_directory ~announce:false plan rel in
              add_message
                plan
                (Pp.textf "File %s should be replaced with a directory" (path_name rel)))
           ~f:(fun plan name ->
             let rel = Path.Local.relative rel name in
             collect_target_only rel (kind_of_target rel) plan)
    | Directory, Directory ->
      let rec merge source_entries target_entries plan =
        match source_entries, target_entries with
        | [], [] -> plan
        | name :: source_entries, [] ->
          let rel = Path.Local.relative rel name in
          let plan = loop rel (kind_of_source rel) Missing plan in
          merge source_entries [] plan
        | [], name :: target_entries ->
          let rel = Path.Local.relative rel name in
          let plan = loop rel Missing (kind_of_target rel) plan in
          merge [] target_entries plan
        | source_name :: source_entries, target_name :: target_entries ->
          (match Filename.compare source_name target_name with
           | Lt ->
             let rel = Path.Local.relative rel source_name in
             let plan = loop rel (kind_of_source rel) Missing plan in
             merge source_entries (target_name :: target_entries) plan
           | Eq ->
             let rel = Path.Local.relative rel source_name in
             let plan = loop rel (kind_of_source rel) (kind_of_target rel) plan in
             merge source_entries target_entries plan
           | Gt ->
             let rel = Path.Local.relative rel target_name in
             let plan = loop rel Missing (kind_of_target rel) plan in
             merge (source_name :: source_entries) target_entries plan)
      in
      merge (list_directory ~loc:t.loc (source_path rel)) (list_target_directory rel) plan
  in
  let { changes; promotions } =
    loop
      Path.Local.root
      (kind_of_source Path.Local.root)
      (kind_of_target Path.Local.root)
      empty_plan
  in
  { changes = List.rev changes; promotions = List.rev promotions }
;;

let plan_diff loc { Diff.optional; file1; file2; mode } =
  let source_kind = kind_of_path ~loc file1 in
  let target_kind = kind_of_path ~loc (Path.build file2) in
  let source_root_path =
    match source_kind, target_kind with
    | Directory, _ | _, Directory -> Path.source (source_root file1)
    | _ -> file1
  in
  let tree_diff =
    { loc; mode; source_root = source_root file1; source_root_path; target_dir = file2 }
  in
  if optional && target_kind = Missing then empty_plan else plan_tree_diff tree_diff
;;

let exec_plan
      loc
      ~patch_back
      { Diff.optional; mode; file1; file2 }
      ({ changes; promotions } : plan)
  =
  match changes with
  | [] ->
    if optional then remove_intermediate_target file2;
    Fiber.return ()
  | changes ->
    let in_source_or_target = in_source_or_target file1 in
    let target_is_copied_from_source_tree =
      is_copied_from_source_tree (Path.build file2)
    in
    Fiber.finalize
      (fun () -> Fiber.parallel_iter changes ~f:(run_change loc ~patch_back mode))
      ~finally:(fun () ->
        (match optional with
         | false ->
           if in_source_or_target && not target_is_copied_from_source_tree
           then register_promotions `Copy promotions
         | true ->
           if in_source_or_target
           then register_promotions `Move promotions
           else remove_intermediate_target file2);
        Fiber.return ())
;;

let exec loc ~patch_back diff = exec_plan loc ~patch_back diff (plan_diff loc diff)
