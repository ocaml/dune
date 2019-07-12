open! Stdune

module Make
    (Src : Action_intf.Ast)
    (Dst : Action_intf.Ast)
= struct
  let map_one_step f (t : Src.t) ~dir ~f_program ~f_string ~f_path : Dst.t =
    match t with
    | Run (prog, args) ->
      Run (f_program ~dir prog, List.map args ~f:(f_string ~dir))
    | Chdir (fn, t) ->
      Chdir (f_path ~dir fn, f t ~dir:fn ~f_program ~f_string ~f_path)
    | Setenv (var, value, t) ->
      Setenv (f_string ~dir var, f_string ~dir value, f t ~dir ~f_program ~f_string ~f_path)
    | Redirect (outputs, fn, t) ->
      Redirect (outputs, f_path ~dir fn, f t ~dir ~f_program ~f_string ~f_path)
    | Ignore (outputs, t) ->
      Ignore (outputs, f t ~dir ~f_program ~f_string ~f_path)
    | Progn l -> Progn (List.map l ~f:(fun t -> f t ~dir ~f_program ~f_string ~f_path))
    | Echo xs -> Echo (List.map xs ~f:(f_string ~dir))
    | Cat x -> Cat (f_path ~dir x)
    | Copy (x, y) -> Copy (f_path ~dir x, f_path ~dir y)
    | Symlink (x, y) ->
      Symlink (f_path ~dir x, f_path ~dir y)
    | Copy_and_add_line_directive (x, y) ->
      Copy_and_add_line_directive (f_path ~dir x, f_path ~dir y)
    | System x -> System (f_string ~dir x)
    | Bash x -> Bash (f_string ~dir x)
    | Write_file (x, y) -> Write_file (f_path ~dir x, f_string ~dir y)
    | Rename (x, y) -> Rename (f_path ~dir x, f_path ~dir y)
    | Remove_tree x -> Remove_tree (f_path ~dir x)
    | Mkdir x -> Mkdir (f_path ~dir x)
    | Digest_files x -> Digest_files (List.map x ~f:(f_path ~dir))
    | Diff ({ file1; file2 ; _ } as diff) ->
      Diff { diff with
             file1 = f_path ~dir file1
           ; file2 = f_path ~dir file2
           }
    | Merge_files_into (sources, extras, target) ->
      Merge_files_into
        (List.map sources ~f:(f_path ~dir),
         List.map extras ~f:(f_string ~dir),
         f_path ~dir target)

  let rec map t ~dir ~f_program ~f_string ~f_path =
    map_one_step map t ~dir ~f_program ~f_string ~f_path
end
