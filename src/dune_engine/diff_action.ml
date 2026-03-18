open Import
module Diff = Dune_util.Action.Diff

let compare_file_paths = function
  | Diff.Mode.Binary -> Io.compare_files
  | Text -> Io.compare_text_files
;;

let compare_files { Diff.optional; mode; file1; file2 } =
  let file2_exists = lazy (Fpath.exists (Path.Build.to_string file2)) in
  if optional && not (Lazy.force file2_exists)
  then
    (* Small optimization to avoid stat'ing file1 *)
    `Eq true
  else (
    let file1_exists = Fpath.exists (Path.to_string file1) in
    match file1_exists, Lazy.force file2_exists with
    | true, true -> `Eq (compare_file_paths mode file1 (Path.build file2) = Eq)
    | false, false -> `Eq true
    | true, false -> if optional then `Eq true else `Delete
    | false, true -> `Eq false)
;;

type kind =
  | Missing
  | File
  | Directory

type file_diff =
  { source_file : Path.Source.t
  ; file1 : Path.t
  ; file2 : Path.t
  ; kind : Diff.Mode.t
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

let run_change loc ~patch_back = function
  | Message messages -> User_error.raise ~loc messages
  | File_diff { source_file; file1; file2; kind } ->
    (match kind with
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
  | Ok { Unix.st_kind = S_REG; _ } -> File
  | Ok { Unix.st_kind = S_DIR; _ } -> Directory
  | Ok { Unix.st_kind; _ } ->
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

let exec_directory loc ~patch_back { Diff.optional; mode; file1; file2 } =
  let source_root = source_root file1 in
  let source_file rel =
    if String.equal rel "" then source_root else Path.Source.relative source_root rel
  in
  let source_path rel = Path.source (source_file rel) in
  let target_path rel =
    let path = if String.equal rel "" then file2 else Path.Build.relative file2 rel in
    Path.build path
  in
  let target_file rel =
    if String.equal rel "" then file2 else Path.Build.relative file2 rel
  in
  let path_name rel = Path.Source.to_string_maybe_quoted (source_file rel) in
  let append_rel rel name = if String.equal rel "" then name else rel ^ "/" ^ name in
  let promotions = ref [] in
  let changes = ref [] in
  let add_message message = changes := Message [ message ] :: !changes in
  let add_promote_file rel =
    promotions
    := Promote_file { source_file = source_file rel; correction_file = target_file rel }
       :: !promotions
  in
  let add_file_diff rel =
    let source_file = source_file rel in
    add_promote_file rel;
    changes
    := File_diff
         { source_file; file1 = source_path rel; file2 = target_path rel; kind = mode }
       :: !changes
  in
  let add_delete what rel =
    promotions := Delete (what, source_file rel) :: !promotions;
    let what =
      match what with
      | `File -> "File"
      | `Directory -> "Directory"
    in
    add_message (Pp.textf "%s %s should be deleted" what (path_name rel))
  in
  let add_create_directory rel =
    promotions := Create_directory (source_file rel) :: !promotions
  in
  let source_kind_of rel = kind_of_path ~loc (source_path rel) in
  let target_kind_of rel = kind_of_path ~loc (target_path rel) in
  let rec collect_target_only rel target_kind =
    match target_kind with
    | Missing -> ()
    | File -> add_promote_file rel
    | Directory ->
      add_create_directory rel;
      List.iter
        (list_directory ~loc (target_path rel))
        ~f:(fun name ->
          let rel = append_rel rel name in
          collect_target_only rel (target_kind_of rel))
  in
  let rec loop rel source_kind target_kind =
    match source_kind, target_kind with
    | Missing, Missing -> ()
    | File, File ->
      if compare_file_paths mode (source_path rel) (target_path rel) <> Eq
      then add_file_diff rel
    | Missing, File -> add_file_diff rel
    | Directory, File ->
      add_promote_file rel;
      add_message (Pp.textf "Directory %s should be replaced with a file" (path_name rel))
    | File, Missing -> add_delete `File rel
    | Directory, Missing -> add_delete `Directory rel
    | Missing, Directory ->
      let entries = list_directory ~loc (target_path rel) in
      add_create_directory rel;
      if List.is_empty entries
      then add_message (Pp.textf "Directory %s should be created" (path_name rel))
      else
        List.iter entries ~f:(fun name ->
          let rel = append_rel rel name in
          loop rel Missing (target_kind_of rel))
    | File, Directory ->
      add_create_directory rel;
      add_message (Pp.textf "File %s should be replaced with a directory" (path_name rel));
      list_directory ~loc (target_path rel)
      |> List.iter ~f:(fun name ->
        let rel = append_rel rel name in
        collect_target_only rel (target_kind_of rel))
    | Directory, Directory ->
      let rec merge source_entries target_entries =
        match source_entries, target_entries with
        | [], [] -> ()
        | name :: source_entries, [] ->
          let rel = append_rel rel name in
          loop rel (source_kind_of rel) Missing;
          merge source_entries []
        | [], name :: target_entries ->
          let rel = append_rel rel name in
          loop rel Missing (target_kind_of rel);
          merge [] target_entries
        | source_name :: source_entries, target_name :: target_entries ->
          (match Filename.compare source_name target_name with
           | Lt ->
             let rel = append_rel rel source_name in
             loop rel (source_kind_of rel) Missing;
             merge source_entries (target_name :: target_entries)
           | Eq ->
             let rel = append_rel rel source_name in
             loop rel (source_kind_of rel) (target_kind_of rel);
             merge source_entries target_entries
           | Gt ->
             let rel = append_rel rel target_name in
             loop rel Missing (target_kind_of rel);
             merge (source_name :: source_entries) target_entries)
      in
      merge
        (list_directory ~loc (source_path rel))
        (list_directory ~loc (target_path rel))
  in
  let source_kind = source_kind_of "" in
  let target_kind = target_kind_of "" in
  if optional && target_kind = Missing
  then Fiber.return ()
  else (
    loop "" source_kind target_kind;
    match List.rev !changes with
    | [] ->
      if optional then remove_intermediate_target file2;
      Fiber.return ()
    | changes ->
      let promotions = List.rev !promotions in
      let in_source_or_target = in_source_or_target file1 in
      let target_is_copied_from_source_tree =
        is_copied_from_source_tree (Path.build file2)
      in
      Fiber.finalize
        (fun () -> Fiber.parallel_iter changes ~f:(run_change loc ~patch_back))
        ~finally:(fun () ->
          (match optional with
           | false ->
             if in_source_or_target && not target_is_copied_from_source_tree
             then register_promotions `Copy promotions
           | true ->
             if in_source_or_target
             then register_promotions `Move promotions
             else remove_intermediate_target file2);
          Fiber.return ()))
;;

let exec_file loc ~patch_back ({ Diff.optional; file1; file2; mode } as diff) =
  let remove_intermediate_file () =
    if optional
    then (
      try Fpath.unlink_exn (Path.Build.to_string file2) with
      | Unix.Unix_error (ENOENT, _, _) -> ())
  in
  match compare_files diff with
  | `Eq true ->
    remove_intermediate_file ();
    Fiber.return ()
  | `Eq false | `Delete ->
    (* CR-soon rgrinberg: handle deletion *)
    let in_source_or_target = in_source_or_target file1 in
    let source_file = source_root file1 in
    Fiber.finalize
      (fun () ->
         if mode = Binary
         then
           User_error.raise
             ~promotion:(promotion source_file)
             ~loc
             [ Pp.textf
                 "Files %s and %s differ."
                 (Path.to_string_maybe_quoted file1)
                 (Path.to_string_maybe_quoted (Path.build file2))
             ]
         else
           Print_diff.print
             ~patch_back
             (promotion source_file)
             file1
             (Path.build file2)
             ~skip_trailing_cr:(mode = Text && Sys.win32))
      ~finally:(fun () ->
        let register how =
          Diff_promotion.register_intermediate how ~source_file ~correction_file:file2
        in
        (match optional with
         | false ->
           (* Promote if in the source tree or not a target. The second case
              means that the diffing have been done with the empty file *)
           if in_source_or_target && not (is_copied_from_source_tree (Path.build file2))
           then register `Copy
         | true ->
           if in_source_or_target then register `Move else remove_intermediate_file ());
        Fiber.return ())
;;

let exec loc ~patch_back ({ Diff.file1; file2; _ } as diff) =
  match kind_of_path ~loc file1, kind_of_path ~loc (Path.build file2) with
  | Directory, _ | _, Directory -> exec_directory loc ~patch_back diff
  | _ -> exec_file loc ~patch_back diff
;;
