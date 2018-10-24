open Import

let flag_of_kind : Ml_kind.t -> _ =
  function
  | Impl -> "--impl"
  | Intf -> "--intf"

let config_includes (config : Dune_file.Auto_format.t) s =
  match config.enabled_for with
  | Default -> true
  | Only set -> List.mem s ~set

let add_diff sctx loc alias ~dir input output =
  let module SC = Super_context in
  let open Build.O in
  let action = Action.diff input output in
  SC.add_alias_action sctx alias ~loc:(Some loc) ~locks:[] ~stamp:input
    (Build.paths [input; output]
     >>>
     Build.action
       ~dir
       ~targets:[]
       action)

let gen_rules sctx (config : Dune_file.Auto_format.t) ~dir =
  let loc = config.loc in
  let files =
    File_tree.files_of
      (Super_context.file_tree sctx)
      (Path.drop_build_context_exn dir)
  in
  let subdir = ".formatted" in
  let output_dir = Path.relative dir subdir in
  let alias = Build_system.Alias.make "fmt" ~dir in
  let alias_formatted = Build_system.Alias.make "fmt" ~dir:output_dir in
  let resolve_program = Super_context.resolve_program sctx ~loc:(Some loc) in
  let setup_formatting file (arrows_acc, extra_deps_acc) =
    let input_basename = Path.basename file in
    let input = Path.relative dir input_basename in
    let output = Path.relative output_dir input_basename in

    let ocaml kind =
      if config_includes config Ocaml then
        let exe = resolve_program "ocamlformat" in
        let args =
          let open Arg_spec in
          [ A (flag_of_kind kind)
          ; Dep input
          ; A "--name"
          ; Path file
          ; A "-o"
          ; Target output
          ]
        in
        Some (Build.run ~dir exe args)
      else
        None
    in

    let formatter =
      match Path.extension file with
      | ".ml" -> ocaml Impl
      | ".mli" -> ocaml Intf
      | ".re"
      | ".rei" when config_includes config Reason ->
        let exe = resolve_program "refmt" in
        let args = [Arg_spec.Dep input] in
        Some (Build.run ~dir ~stdout_to:output exe args)
      | _ -> None
    in

    let new_extra_deps_acc =
      if String.equal input_basename ".ocamlformat" then
        input::extra_deps_acc
      else
        extra_deps_acc
    in

    let new_arrows_acc =
      match formatter with
      | None -> arrows_acc
      | Some arr -> (arr, input, output)::arrows_acc
    in

    (new_arrows_acc, new_extra_deps_acc)
  in
  Super_context.on_load_dir sctx ~dir:output_dir ~f:(fun () ->
    let arrows, extra_deps =
      Path.Set.fold files ~init:([], []) ~f:setup_formatting
    in
    List.iter
      arrows
      ~f:(fun (format_arr, input, output) ->
        let open Build.O in
        let arr = Build.paths extra_deps >>> format_arr in
        Super_context.add_rule sctx ~mode:Standard ~loc arr;
        add_diff sctx loc alias_formatted ~dir input output));
  Super_context.add_alias_deps sctx alias
    (Path.Set.singleton (Build_system.Alias.stamp_file alias_formatted));
  Super_context.add_alias_deps sctx alias_formatted Path.Set.empty
