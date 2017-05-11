open Import
open Jbuild_types
open Build.O

module SC = Super_context

let ( ++ ) = Path.relative

let get_odoc sctx = SC.resolve_program sctx "odoc" ~hint:"opam install odoc"

let lib_odoc_all ~dir (lib : Library.t)  =
  Alias.file (Alias.lib_odoc_all ~dir lib.name)

let lib_dependencies (libs : Lib.t list) =
  List.filter_map libs ~f:(function
    | External _ -> None
    | Internal (dir, lib) -> Some (lib_odoc_all ~dir lib))

let module_deps (m : Module.t) ~dir ~dep_graph ~modules =
  Build.dyn_paths
    (dep_graph
     >>^ fun graph ->
     List.map (Utils.find_deps ~dir graph m.name)
       ~f:(fun name ->
         let m = Utils.find_module ~dir modules name in
         Module.odoc_file m ~dir))

let compile_module sctx (m : Module.t) ~odoc ~dir ~includes ~dep_graph ~modules
      ~lib_public_name =
  let context = SC.context sctx in
  let odoc_file = Module.odoc_file m ~dir in
  SC.add_rule sctx
    (module_deps m ~dir ~dep_graph ~modules
     >>>
     includes
     >>>
     Build.run ~context ~dir odoc ~extra_targets:[odoc_file]
       [ A "compile"
       ; Dyn (fun x -> x)
       ; A "-I"; Path dir
       ; As ["--pkg"; lib_public_name]
       ; Dep (Module.cmti_file m ~dir)
       ]);
  (m, odoc_file)

let to_html sctx (m : Module.t) odoc_file ~doc_dir ~odoc ~dir ~includes
      ~dep_graph ~modules ~lib_public_name =
  let context = SC.context sctx in
  let html_file =
    doc_dir ++ lib_public_name ++ m.name ++ "index.html"
  in
  SC.add_rule sctx
    (module_deps m ~dir ~dep_graph ~modules
     >>>
     includes
     >>>
     Build.run ~context ~dir odoc ~extra_targets:[html_file]
       [ A "html"
       ; Dyn (fun x -> x)
       ; A "-I"; Path dir
       ; A "-o"; Path doc_dir
       ; Dep odoc_file
       ]);
  html_file

let doc_dir = Path.of_string "_build/doc"

let css_file sctx =
  let context = SC.context sctx in
  doc_dir ++ context.name ++ "odoc.css"

let setup_library_rules sctx (lib : Library.t) ~dir ~modules ~requires
      ~(dep_graph:Ocamldep.dep_graph) =
  Option.iter lib.public ~f:(fun public ->
    let context = SC.context sctx in
    let dep_graph =
      (* Use the dependency graph given by ocamldep. However, when a module has no .mli,
         use the dependencies for the .ml *)
      Build.fanout dep_graph.intf dep_graph.impl
      >>^ fun (intf, impl) ->
      String_map.merge intf impl ~f:(fun _ intf impl ->
        match intf, impl with
        | Some _, _    -> intf
        | None, Some _ -> impl
        | None, None -> assert false)
    in
    let odoc = get_odoc sctx in
    let includes =
      requires
      >>>
      Build.dyn_paths (Build.arr lib_dependencies)
      >>^ Lib.include_flags
    in
    let odoc_files =
      List.map (String_map.values modules)
        ~f:(compile_module sctx ~odoc ~dir ~includes ~dep_graph ~modules
              ~lib_public_name:public.name)
    in
    let aliases = SC.aliases sctx in
    Alias.add_deps aliases (Alias.lib_odoc_all ~dir lib.name)
      (List.map odoc_files ~f:snd);
    let doc_dir = doc_dir ++ context.name in
    let odoc_files =
      if lib.wrapped then
        let main_module_name = String.capitalize_ascii lib.name in
        List.filter odoc_files ~f:(fun (m, _) -> m.Module.name = main_module_name)
      else
        odoc_files
    in
    let html_files =
      List.map odoc_files ~f:(fun (m, odoc_file) ->
        to_html sctx m odoc_file ~doc_dir ~odoc ~dir ~includes ~dep_graph ~modules
          ~lib_public_name:public.name)
    in
    Alias.add_deps aliases (Alias.doc ~dir) (css_file sctx :: html_files))

let setup_css_rule sctx =
  let context = SC.context sctx in
  let doc_dir = doc_dir ++ context.name in
  SC.add_rule sctx
    (Build.run ~context
       ~dir:context.build_dir
       ~extra_targets:[doc_dir ++ "odoc.css"]
       (get_odoc sctx)
       [ A "css"; A "-o"; Path doc_dir ]);

