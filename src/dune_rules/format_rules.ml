open Import

let add_diff sctx loc alias ~dir ~input ~output =
  let open Action_builder.O in
  let action = Action.Chdir (Path.build dir, Action.diff input output) in
  Super_context.add_alias_action sctx alias ~dir ~loc:(Some loc)
    (Action_builder.paths [ input; Path.build output ]
    >>> Action_builder.return (Action.Full.make action))

let rec subdirs_until_root dir =
  match Path.parent dir with
  | None -> [ dir ]
  | Some d -> dir :: subdirs_until_root d

let depend_on_files ~named dir =
  subdirs_until_root dir
  |> List.concat_map ~f:(fun dir -> List.map named ~f:(Path.relative dir))
  |> Action_builder.paths_existing

let formatted_dir_basename = ".formatted"

let action =
  let module Spec = struct
    type ('path, 'target) t = Dune_lang.Syntax.Version.t * 'path * 'target

    let name = "format-dune-file"

    let version = 1

    let bimap (ver, src, dst) f g = (ver, f src, g dst)

    let is_useful_to ~distribute:_ ~memoize = memoize

    let encode (version, src, dst) path target : Dune_lang.t =
      List
        [ Dune_lang.atom_or_quoted_string "format-dune-file"
        ; Dune_lang.Syntax.Version.encode version
        ; path src
        ; target dst
        ]

    let action (version, src, dst) ~ectx:_ ~eenv:_ =
      Dune_lang.Format.format_action ~version ~src ~dst;
      Fiber.return ()
  end in
  fun ~version (src : Path.t) (dst : Path.Build.t) ->
    let module M :
      Action.Ext.Instance with type path = Path.t and type target = Path.Build.t =
    struct
      type path = Path.t

      type target = Path.Build.t

      module Spec = Spec

      let v = (version, src, dst)
    end in
    Action.Extension (module M)

let gen_rules_output sctx (config : Format_config.t) ~version ~dialects
    ~expander ~output_dir =
  assert (formatted_dir_basename = Path.Build.basename output_dir);
  let loc = Format_config.loc config in
  let dir = Path.Build.parent_exn output_dir in
  let source_dir = Path.Build.drop_build_context_exn dir in
  let alias_formatted = Alias.fmt ~dir:output_dir in
  let depend_on_files named = depend_on_files ~named (Path.build dir) in
  let open Memo.O in
  let setup_formatting file =
    let input_basename = Path.Source.basename file in
    let input = Path.Build.relative dir input_basename in
    let output = Path.Build.relative output_dir input_basename in
    let formatter =
      let ext = Path.Source.extension file in
      let open Option.O in
      let* dialect, kind = Dialect.DB.find_by_extension dialects ext in
      let* () =
        Option.some_if
          (Format_config.includes config (Dialect (Dialect.name dialect)))
          ()
      in
      let+ loc, action, extra_deps =
        match Dialect.format dialect kind with
        | Some _ as action -> action
        | None -> (
          match Dialect.preprocess dialect kind with
          | None -> Dialect.format Dialect.ocaml kind
          | Some _ -> None)
      in
      let extra_deps =
        match extra_deps with
        | [] -> Action_builder.return ()
        | extra_deps -> depend_on_files extra_deps
      in
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets extra_deps
      >>> Preprocessing.action_for_pp_with_target
            ~sandbox:Sandbox_config.default ~loc ~expander ~action ~src:input
            ~target:output
    in
    Memo.Option.iter formatter ~f:(fun action ->
        Super_context.add_rule sctx ~mode:Standard ~loc ~dir action
        >>> add_diff sctx loc alias_formatted ~dir ~input:(Path.build input)
              ~output)
  in
  let* () =
    Source_tree.files_of source_dir
    >>= Memo.parallel_iter_set (module Path.Source.Set) ~f:setup_formatting
  in
  let* () =
    match Format_config.includes config Dune with
    | false -> Memo.return ()
    | true -> (
      Source_tree.find_dir source_dir >>= function
      | None -> Memo.return ()
      | Some source_dir -> (
        match Source_tree.Dir.dune_file source_dir with
        | None -> Memo.return ()
        | Some f ->
          let path = Source_tree.Dune_file.path f in
          let input_basename = Path.Source.basename path in
          let input = Path.Build.relative dir input_basename in
          let output = Path.Build.relative output_dir input_basename in
          Super_context.add_rule sctx ~mode:Standard ~loc ~dir
            (Action_builder.with_file_targets ~file_targets:[ output ]
            @@
            let open Action_builder.O in
            let input = Path.build input in
            let+ () = Action_builder.path input in
            Action.Full.make (action ~version input output))
          >>> add_diff sctx loc alias_formatted ~dir ~input:(Path.build input)
                ~output))
  in
  Rules.Produce.Alias.add_deps alias_formatted (Action_builder.return ())

let gen_rules sctx ~output_dir =
  let open Memo.O in
  let dir = Path.Build.parent_exn output_dir in
  let* config = Super_context.format_config sctx ~dir in
  Memo.when_
    (not (Format_config.is_empty config))
    (fun () ->
      let* expander = Super_context.expander sctx ~dir in
      let* scope = Scope.DB.find_by_dir output_dir in
      let project = Scope.project scope in
      let dialects = Dune_project.dialects project in
      let version = Dune_project.dune_version project in
      gen_rules_output sctx config ~version ~dialects ~expander ~output_dir)

let setup_alias sctx ~dir =
  let open Memo.O in
  let* config = Super_context.format_config sctx ~dir in
  Memo.when_
    (not (Format_config.is_empty config))
    (fun () ->
      let output_dir = Path.Build.relative dir formatted_dir_basename in
      let alias = Alias.fmt ~dir in
      let alias_formatted = Alias.fmt ~dir:output_dir in
      Rules.Produce.Alias.add_deps alias
        (Action_builder.dep (Dep.alias alias_formatted)))
