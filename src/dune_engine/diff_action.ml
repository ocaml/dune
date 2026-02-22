open Import
module Diff = Dune_util.Action.Diff

let compare_files = function
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
    | true, true -> `Eq (compare_files mode file1 (Path.build file2) = Eq)
    | false, false -> `Eq true
    | true, false -> if optional then `Eq true else `Delete
    | false, true -> `Eq false)
;;

let exec loc ~patch_back ({ Diff.optional; file1; file2; mode } as diff) =
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
      Path.as_in_build_dir_exn file1 |> Path.Build.drop_build_context_maybe_sandboxed_exn
    in
    Fiber.finalize
      (fun () ->
         let promotion =
           { User_message.Diff_annot.in_source = source_file
           ; in_build = Diff_promotion.File.in_staging_area source_file
           }
         in
         if mode = Binary
         then
           User_error.raise
             ~promotion
             ~loc
             [ Pp.textf
                 "Files %s and %s differ."
                 (Path.to_string_maybe_quoted file1)
                 (Path.to_string_maybe_quoted (Path.build file2))
             ]
         else
           Print_diff.print
             ~patch_back
             promotion
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
