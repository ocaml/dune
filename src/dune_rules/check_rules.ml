open Import

let dev_files =
  [ Ml_kind.cmt_ext Impl; Ml_kind.cmt_ext Intf; Cm_kind.ext Cmi ]
  |> List.map ~f:(String.drop_prefix_if_exists ~prefix:".")
  |> Glob.matching_extensions
;;

let add_obj_dir sctx ~obj_dir mode =
  if Super_context.context sctx |> Context.merlin
  then (
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
      (Action_builder.paths_matching_unit ~loc:(Loc.of_pos __POS__) dir_glob))
  else Memo.return ()
;;

let add_files sctx ~dir files =
  if Super_context.context sctx |> Context.merlin
  then (
    let alias = Alias.make Alias0.check ~dir in
    (let open Action_builder.O in
     files >>| Dep.Set.of_files >>= Action_builder.deps)
    |> Rules.Produce.Alias.add_deps alias)
  else Memo.return ()
;;

let add_cycle_check sctx ~dir modules =
  if Super_context.context sctx |> Context.merlin
  then (
    let alias = Alias.make Alias0.check ~dir in
    Rules.Produce.Alias.add_deps alias (Action_builder.ignore modules))
  else Memo.return ()
;;
