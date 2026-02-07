open Import
module Diff = Dune_util.Action.Diff

let compare_files = function
  | Diff.Mode.Binary -> Io.compare_files
  | Text -> Io.compare_text_files
;;

let diff_eq_files { Diff.optional; mode; file1; file2 } =
  let file1 = if Fpath.exists (Path.to_string file1) then file1 else Dev_null.path in
  let file2 = Path.build file2 in
  (optional && not (Fpath.exists (Path.to_string file2)))
  || compare_files mode file1 file2 = Eq
;;

let exec loc ({ Diff.optional; file1; file2; mode } as diff) =
  let remove_intermediate_file () =
    if optional
    then (
      try Fpath.unlink_exn (Path.Build.to_string file2) with
      | Unix.Unix_error (ENOENT, _, _) -> ())
  in
  if diff_eq_files diff
  then (
    remove_intermediate_file ();
    Fiber.return ())
  else (
    let is_copied_from_source_tree file =
      match Path.extract_build_context_dir_maybe_sandboxed file with
      | None -> false
      | Some (_, file) ->
        (* CR-someday rgrinberg: isn't this racy? *)
        Fpath.exists (Path.to_string (Path.source file))
    in
    let in_source_or_target =
      is_copied_from_source_tree file1 || not (Fpath.exists (Path.to_string file1))
    in
    let source_file =
      snd (Option.value_exn (Path.extract_build_context_dir_maybe_sandboxed file1))
    in
    Fiber.finalize
      (fun () ->
         let annots =
           User_message.Annots.singleton
             Diff_promotion.Annot.annot
             { Diff_promotion.Annot.in_source = source_file
             ; in_build = Diff_promotion.File.in_staging_area source_file
             }
         in
         if mode = Binary
         then
           User_error.raise
             ~annots
             ~loc
             [ Pp.textf
                 "Files %s and %s differ."
                 (Path.to_string_maybe_quoted file1)
                 (Path.to_string_maybe_quoted (Path.build file2))
             ]
         else
           Print_diff.print
             annots
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
        Fiber.return ()))
;;
