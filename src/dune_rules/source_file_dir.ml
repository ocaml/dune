open Import

type t =
  { dir : Path.Build.t
  ; path_to_root : Filename.t list
  ; files : Filename.Array.Set.t
  ; source_dir : Source_tree.Rules.Dir.t option
  ; stanzas : Stanza.t list
  }

let materialize ~dst (source : Source_tree.Rules.File.materialization) =
  match source with
  | Copy src -> Action_builder.copy ~src ~dst
  | Write { contents; executable; dependencies } ->
    let perm =
      if executable
      then Dune_lang.Action.File_perm.Executable
      else Dune_lang.Action.File_perm.Normal
    in
    let { Action_builder.With_targets.build; targets } =
      Action_builder.write_file ~perm dst contents
    in
    let build =
      let open Action_builder.O in
      let* () = Action_builder.paths dependencies in
      build
    in
    { Action_builder.With_targets.build; targets }
;;

let rules ~dir source_dir =
  let open Memo.O in
  let source_copy_rule filename =
    let file =
      Source_tree.Rules.Dir.file
        source_dir
        (Path.Local.relative_fname Path.Local.root filename)
    in
    let* materialization = Source_tree.Rules.File.materialization file in
    let dst = Path.Build.relative_fname dir filename in
    match materialization with
    | None ->
      Code_error.raise
        "Source file selected without materialization"
        [ "file", Source_tree.Rules.File.source_path file |> Source_path.to_dyn ]
    | Some (Copy (In_source_tree src)) ->
      Load_rules.source_file_copy ~src ~dst |> Memo.return
    | Some source ->
      let { Action_builder.With_targets.build; targets } = materialize ~dst source in
      let path =
        match source with
        | Copy path -> path
        | Write _ -> Source_tree.Rules.File.path file
      in
      Rule.make ~info:(Rule.Info.Source_file_copy path) ~targets build |> Memo.return
  in
  Source_tree.Rules.Dir.filenames source_dir
  |> Filename.Array.Set.to_list
  |> Filename.Map.of_list_map_exn ~f:(fun filename -> filename, source_copy_rule filename)
;;
