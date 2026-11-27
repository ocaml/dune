open Import

type t =
  { dir : Path.Build.t
  ; path_to_root : Filename.t list
  ; files : Filename.Array.Set.t
  ; source_dir : Source_tree.Dir.t option
  ; stanzas : Stanza.t list
  }

let copy_rule ~src ~dst =
  let { Action_builder.With_targets.build; targets } = Action_builder.copy ~src ~dst in
  Rule.make ~targets build
;;

let rules ~dir source_dir =
  let source_copy_rule filename =
    let file = Source_tree.Dir.file source_dir filename in
    copy_rule
      ~src:(Source_tree_file.File.backing_path file)
      ~dst:(Path.Build.relative_fname dir filename)
  in
  Source_tree.Dir.filenames source_dir
  |> Filename.Array.Set.to_list
  |> Filename.Map.of_list_map_exn ~f:(fun filename -> filename, source_copy_rule filename)
;;
