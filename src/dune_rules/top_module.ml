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
  match
    let open Option.O in
    let* name =
      let fname = Path.Build.basename src in
      let name = Filename.remove_extension fname in
      if String.equal fname name then None else Some name
    in
    Module_name.of_string_opt name
  with
  | None -> Memo.return None
  | Some module_name ->
    let dir = Path.Build.parent_exn src in
    let* dir_contents = drop_rules @@ fun () -> Dir_contents.get sctx ~dir in
    let* ocaml = Dir_contents.ocaml dir_contents
    and* scope = Dir_contents.dir dir_contents |> Scope.DB.find_by_dir in
    Ml_sources.find_origin ocaml ~libs:(Scope.libs scope) [ module_name ]
    >>= (function
     | None -> Memo.return None
     | Some origin ->
       let* expander = Super_context.expander sctx ~dir in
       let+ cctx, merlin =
         drop_rules
         @@ fun () ->
         match origin with
         | Executables exes -> Exe_rules.rules ~sctx ~dir_contents ~scope ~expander exes
         | Tests tests -> Exe_rules.rules ~sctx ~dir_contents ~scope ~expander tests.exes
         | Library lib -> Lib_rules.rules lib ~sctx ~dir_contents ~expander ~scope
         | Melange mel ->
           Melange_rules.setup_emit_cmj_rules ~sctx ~dir_contents ~expander ~scope mel
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
  let+ graph, _ =
    let dep_graph =
      let dg = Compilation_context.dep_graphs cctx in
      Ocaml.Ml_kind.Dict.get dg Impl
    in
    Dep_graph.deps_of dep_graph module_ |> Action_builder.evaluate_and_collect_facts
  in
  graph
;;

let gen_rules sctx ~dir:rules_dir ~comps =
  let src = Path.Source.L.relative Path.Source.root comps in
  find_module sctx src
  >>= function
  | None -> Memo.return ()
  | Some (module_, cctx, _merlin, _) ->
    let module_ = Module.set_source module_ ~ml_kind:Intf None in
    let* () =
      (let obj_dir = Compilation_context.obj_dir cctx in
       module_deps cctx module_
       >>| List.filter_map ~f:(Obj_dir.Module.cm_file obj_dir ~kind:(Ocaml Cmi)))
      >>= Memo.parallel_iter ~f:(fun file ->
        (let src = Path.build file in
         let dst = Path.Build.relative rules_dir (Path.Build.basename file) in
         Action_builder.symlink ~src ~dst)
        |> Super_context.add_rule sctx ~dir:rules_dir)
    in
    let cctx =
      private_obj_dir (Super_context.context sctx) src
      |> Compilation_context.set_obj_dir cctx
      |> Compilation_context.without_bin_annot
      |> Compilation_context.set_modes
           ~modes:
             { Lib_mode.Map.melange = false; ocaml = { byte = true; native = false } }
    in
    Module_compilation.build_module ~force_write_cmi:true cctx module_
;;
