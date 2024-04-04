open Import

let add_diff sctx loc alias ~dir ~input ~output =
  let open Action_builder.O in
  let action = Action.Chdir (Path.build dir, Action.diff input output) in
  Super_context.add_alias_action
    sctx
    alias
    ~dir
    ~loc
    (Action_builder.paths [ input; Path.build output ]
     >>> Action_builder.return (Action.Full.make action))
;;

let rec subdirs_until_root dir =
  match Path.parent dir with
  | None -> [ dir ]
  | Some d -> dir :: subdirs_until_root d
;;

let depend_on_files ~named dir =
  subdirs_until_root dir
  |> List.concat_map ~f:(fun dir -> List.map named ~f:(Path.relative dir))
  |> Action_builder.paths_existing
;;

let formatted_dir_basename = ".formatted"

let action =
  let module Spec = struct
    type ('path, 'target) t = Dune_lang.Syntax.Version.t * 'path * 'target

    let name = "format-dune-file"
    let version = 1
    let bimap (ver, src, dst) f g = ver, f src, g dst
    let is_useful_to ~memoize = memoize

    let encode (version, src, dst) path target : Dune_lang.t =
      List
        [ Dune_lang.atom_or_quoted_string "format-dune-file"
        ; Dune_lang.Syntax.Version.encode version
        ; path src
        ; target dst
        ]
    ;;

    let action (version, src, dst) ~ectx:_ ~eenv:_ =
      Dune_lang.Format.format_action ~version ~src ~dst;
      Fiber.return ()
    ;;
  end
  in
  fun ~version (src : Path.t) (dst : Path.Build.t) ->
    let module M :
      Action.Ext.Instance with type path = Path.t and type target = Path.Build.t = struct
      type path = Path.t
      type target = Path.Build.t

      module Spec = Spec

      let v = version, src, dst
    end
    in
    Action.Extension (module M)
;;

module Alias = struct
  let fmt ~dir = Alias.make Alias0.fmt ~dir
end

let gen_rules_output
  sctx
  (config : Format_config.t)
  ~version
  ~dialects
  ~expander
  ~output_dir
  =
  assert (formatted_dir_basename = Path.Build.basename output_dir);
  let loc = Format_config.loc config in
  let dir = Path.Build.parent_exn output_dir in
  let alias_formatted = Alias.fmt ~dir:output_dir in
  let open Memo.O in
  let setup_formatting file =
    let input_basename = Path.Source.basename file in
    let input = Path.Build.relative dir input_basename in
    let output = Path.Build.relative output_dir input_basename in
    (let open Option.O in
     let* dialect, kind =
       Path.Source.extension file |> Dialect.DB.find_by_extension dialects
     in
     let* () =
       Option.some_if (Format_config.includes config (Dialect (Dialect.name dialect))) ()
     in
     let+ loc, action, extra_deps =
       match Dialect.format dialect kind with
       | Some _ as action -> action
       | None ->
         (match Dialect.preprocess dialect kind with
          | None -> Dialect.format Dialect.ocaml kind
          | Some _ -> None)
     in
     let extra_deps =
       match extra_deps with
       | [] -> Action_builder.return ()
       | extra_deps -> depend_on_files ~named:extra_deps (Path.build dir)
     in
     let open Action_builder.With_targets.O in
     Action_builder.with_no_targets extra_deps
     >>> Pp_spec_rules.action_for_pp_with_target
           ~sandbox:Sandbox_config.default
           ~loc
           ~expander
           ~action
           ~src:input
           ~target:output)
    |> Memo.Option.iter ~f:(fun action ->
      Super_context.add_rule sctx ~mode:Standard ~loc ~dir action
      >>> add_diff sctx loc alias_formatted ~dir ~input:(Path.build input) ~output)
  in
  let* source_dir = Source_tree.find_dir (Path.Build.drop_build_context_exn dir) in
  let* () =
    Memo.Option.iter source_dir ~f:(fun source_dir ->
      Source_tree.Dir.filenames source_dir
      |> Filename.Set.to_seq
      |> Memo.parallel_iter_seq ~f:(fun file ->
        Path.Source.relative (Source_tree.Dir.path source_dir) file |> setup_formatting))
  and* () =
    match Format_config.includes config Dune with
    | false -> Memo.return ()
    | true ->
      Memo.Option.iter source_dir ~f:(fun source_dir ->
        Source_tree.Dir.dune_file source_dir
        |> Memo.Option.iter ~f:(fun f ->
          Dune_file0.path f
          |> Memo.Option.iter ~f:(fun path ->
            let input_basename = Path.Source.basename path in
            let input = Path.build (Path.Build.relative dir input_basename) in
            let output = Path.Build.relative output_dir input_basename in
            (let open Action_builder.O in
             let+ () = Action_builder.path input in
             Action.Full.make (action ~version input output))
            |> Action_builder.with_file_targets ~file_targets:[ output ]
            |> Super_context.add_rule sctx ~mode:Standard ~loc ~dir
            >>> add_diff sctx loc alias_formatted ~dir ~input ~output)))
  in
  Rules.Produce.Alias.add_deps alias_formatted (Action_builder.return ())
;;

let format_config ~dir =
  let open Memo.O in
  let+ value =
    Env_stanza_db.value_opt ~dir ~f:(fun (t : Dune_env.config) ->
      Memo.return t.format_config)
  and+ default =
    (* we always force the default for error checking *)
    Path.Build.drop_build_context_exn dir
    |> Source_tree.nearest_dir
    >>| Source_tree.Dir.project
    >>| Dune_project.format_config
  in
  Option.value value ~default
;;

let with_config ~dir f =
  let open Memo.O in
  let* config = format_config ~dir in
  if Format_config.is_empty config
  then
    (* CR-rgrinberg: this [is_empty] check is weird. We should use [None]
       to represent that no settings have been set. *)
    Memo.return ()
  else f config
;;

let gen_rules sctx ~output_dir =
  let open Memo.O in
  let dir = Path.Build.parent_exn output_dir in
  with_config ~dir (fun config ->
    let* expander = Super_context.expander sctx ~dir in
    let* project = Dune_load.find_project ~dir in
    let dialects = Dune_project.dialects project in
    let version = Dune_project.dune_version project in
    gen_rules_output sctx config ~version ~dialects ~expander ~output_dir)
;;

let setup_alias ~dir =
  with_config ~dir (fun (_ : Format_config.t) ->
    let output_dir = Path.Build.relative dir formatted_dir_basename in
    let alias = Alias.fmt ~dir in
    let alias_formatted = Alias.fmt ~dir:output_dir in
    Rules.Produce.Alias.add_deps alias (Action_builder.dep (Dep.alias alias_formatted)))
;;
