open Import

let dev_files =
  [ Ml_kind.cmt_ext Impl; Ml_kind.cmt_ext Intf; Cm_kind.ext Cmi ]
  |> List.map ~f:(String.drop_prefix_if_exists ~prefix:".")
  |> Glob.matching_extensions
;;

let add_obj_dir ~obj_dir mode =
  let dir_glob =
    let dir =
      Path.build
        (match mode with
         | Lib_mode.Melange -> Obj_dir.melange_dir obj_dir
         | Ocaml _ -> Obj_dir.byte_dir obj_dir)
    in
    File_selector.of_glob ~dir dev_files
  in
  Rules.Produce.Alias.add_deps
    (Alias.make Alias0.check ~dir:(Obj_dir.dir obj_dir))
    (Action_builder.paths_matching_unit ~loc:(Loc.of_pos __POS__) dir_glob)
;;

let add_files ~dir files =
  let alias = Alias.make Alias0.check ~dir in
  let files = Path.Set.of_list files in
  Rules.Produce.Alias.add_deps alias (Action_builder.path_set files)
;;

let add_cycle_check ~dir modules =
  let alias = Alias.make Alias0.check ~dir in
  Rules.Produce.Alias.add_deps alias (Action_builder.ignore modules)
;;
