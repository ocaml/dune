open Import
open Memo.O

let private_obj_dir (ctx : Context.t) src =
  let src =
    Path.Build.append_source (Path.Build.relative (Context.build_dir ctx) ".topmod") src
  in
  Obj_dir.for_pp ~dir:src
;;

let drop_rules f =
  let+ res, _ = Memo.Implicit_output.collect Dune_engine.Rules.implicit_output f in
  res
;;

let find_module sctx src =
  let src =
    Path.Build.append_source (Context.build_dir (Super_context.context sctx)) src
  in
  let dir = Path.Build.parent_exn src in
  let module_name =
    let open Option.O in
    let* name =
      let fname = Path.Build.basename src in
      let name = Filename.remove_extension fname in
      if String.equal fname name then None else Some name
    in
    Module_name.of_string_opt name
  in
  match module_name with
  | None -> Memo.return None
  | Some module_name ->
    let* dir_contents = drop_rules @@ fun () -> Dir_contents.get sctx ~dir in
    let* ocaml = Dir_contents.ocaml dir_contents
    and* scope = Scope.DB.find_by_dir dir in
    Ml_sources.find_origin ocaml ~libs:(Scope.libs scope) [ module_name ]
    >>= (function
     | None -> Memo.return None
     | Some origin ->
       let* expander = Super_context.expander sctx ~dir in
       let+ cctx, merlin =
         drop_rules
         @@ fun () ->
         match origin with
         | Executables exes ->
           Exe_rules.rules ~sctx ~dir ~dir_contents ~scope ~expander exes
         | Library lib -> Lib_rules.rules lib ~sctx ~dir_contents ~dir ~expander ~scope
         | Melange mel ->
           Melange_rules.setup_emit_cmj_rules
             ~sctx
             ~dir_contents
             ~dir
             ~expander
             ~scope
             mel
       in
       let module_ =
         let modules = Compilation_context.modules cctx in
         match Modules.With_vlib.find modules module_name with
         | Some m -> m
         | None ->
           User_error.raise
             [ Pp.textf
                 "Could not find module corresponding to source file %s"
                 (Path.Build.to_string_maybe_quoted src)
             ]
       in
       Some (module_, cctx, merlin, origin))
;;

let module_deps cctx module_ =
  let dep_graph =
    let dg = Compilation_context.dep_graphs cctx in
    Ocaml.Ml_kind.Dict.get dg Impl
  in
  let action = Dep_graph.deps_of dep_graph module_ in
  let+ graph, _ = Action_builder.evaluate_and_collect_facts action in
  graph
;;

let gen_rules sctx ~dir:rules_dir ~comps =
  let src = Path.Source.L.relative Path.Source.root comps in
  let* mod_ = find_module sctx src in
  match mod_ with
  | None -> Memo.return ()
  | Some (module_, cctx, _merlin, _) ->
    let module_ = Module.set_source module_ Intf None in
    let* () =
      let* module_deps = module_deps cctx module_ in
      let files =
        let obj_dir = Compilation_context.obj_dir cctx in
        List.filter_map module_deps ~f:(fun module_ ->
          Obj_dir.Module.cm_file obj_dir module_ ~kind:(Ocaml Cmi))
      in
      Memo.parallel_iter files ~f:(fun file ->
        let src = Path.build file in
        let dst = Path.Build.relative rules_dir (Path.Build.basename file) in
        Super_context.add_rule sctx ~dir:rules_dir (Action_builder.symlink ~src ~dst))
    in
    let cctx =
      let obj_dir = private_obj_dir (Super_context.context sctx) src in
      Compilation_context.set_obj_dir cctx obj_dir
      |> Compilation_context.without_bin_annot
      |> Compilation_context.set_modes
           ~modes:
             { Lib_mode.Map.melange = false; ocaml = { byte = true; native = false } }
    in
    Module_compilation.build_module ~force_write_cmi:true cctx module_
;;
