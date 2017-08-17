open Import
open Jbuild
open Build.O

module SC = Super_context

let ( ++ ) = Path.relative

let get_odoc sctx = SC.resolve_program sctx "odoc" ~hint:"opam install odoc"
let odoc_ext = ".odoc"

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
      ~lib_public_name ~(lib : Library.t) =
  let context = SC.context sctx in
  let html_dir = doc_dir ++ lib_public_name ++ String.capitalize_ascii m.obj_name in
  let html_file = html_dir ++ "index.html" in
  SC.add_rule sctx
    (SC.Libs.static_file_deps (dir, lib) ~ext:odoc_ext
     >>>
     includes
     >>>
     Build.progn
       [ Build.remove_tree html_dir
       ; Build.mkdir html_dir
       ; Build.run ~context ~dir odoc ~extra_targets:[html_file]
           [ A "html"
           ; Dyn (fun x -> x)
           ; A "-I"; Path dir
           ; A "-o"; Path doc_dir
           ; Dep odoc_file
           ]
       ; Build.create_file (html_dir ++ Config.jbuilder_keep_fname)
       ]
    );
  html_file

let lib_index sctx ~odoc ~dir ~(lib : Library.t) ~lib_public_name ~doc_dir ~modules
      ~includes =
  let context = SC.context sctx in
  let generated_index_mld = dir ++ sprintf "%s-generated.mld" lib.name in
  let source_index_mld = dir ++ sprintf "%s.mld" lib.name in
  let header = {|{%html:<nav><a href="..">Up</a></nav>%}|} in
  SC.add_rule sctx
    (Build.if_file_exists source_index_mld
       ~then_:(Build.contents source_index_mld
               >>^ fun s -> sprintf "%s\n%s" header s)
       ~else_:(Build.arr (fun () ->
         (if lib.wrapped then
            sprintf
              "%s\n\
               {1 Library %s}\n\
               The entry point for this library is module {!module:%s}."
              header
              lib_public_name
              (String.capitalize_ascii lib.name)
          else
            sprintf
              "%s\n\
               {1 Library %s}\n\
               This library exposes the following toplevel modules: {!modules:%s}."
              header
              lib_public_name
              (String_map.keys modules |> String.concat ~sep:" "))))
     >>>
     Build.write_file_dyn generated_index_mld);
  let html_file =
    doc_dir ++ lib_public_name ++ "index.html"
  in
  SC.add_rule sctx
    (SC.Libs.static_file_deps (dir, lib) ~ext:odoc_ext
     >>>
     includes
     >>>
     Build.run ~context ~dir odoc ~extra_targets:[html_file]
       [ A "html"
       ; Dyn (fun x -> x)
       ; A "-I"; Path dir
       ; A "-o"; Path doc_dir
       ; A "--index-for"; A lib_public_name
       ; Dep generated_index_mld
       ]);
  html_file

let doc_dir ~context = Path.relative context.Context.build_dir "_doc"

let css_file ~doc_dir = doc_dir ++ "odoc.css"

let setup_library_rules sctx (lib : Library.t) ~dir ~modules ~requires
      ~(dep_graph:Ocamldep.dep_graph) =
  Option.iter lib.public ~f:(fun public ->
    let context = SC.context sctx in
    let dep_graph =
      Build.memoize "odoc deps"
        ((* Use the dependency graph given by ocamldep. However, when a module has no
            .mli, use the dependencies for the .ml *)
          Build.fanout dep_graph.intf dep_graph.impl
          >>^ fun (intf, impl) ->
          String_map.merge intf impl ~f:(fun _ intf impl ->
            match intf, impl with
            | Some _, _    -> intf
            | None, Some _ -> impl
            | None, None -> assert false))
    in
    let odoc = get_odoc sctx in
    let includes =
      Build.memoize "includes"
        (requires
         >>>
         SC.Libs.file_deps sctx ~ext:odoc_ext
         >>^ Lib.include_flags)
    in
    let modules_and_odoc_files =
      List.map (String_map.values modules)
        ~f:(compile_module sctx ~odoc ~dir ~includes ~dep_graph ~modules
              ~lib_public_name:public.name)
    in
    SC.Libs.setup_file_deps_alias sctx ~ext:odoc_ext (dir, lib)
      (List.map modules_and_odoc_files ~f:snd);
    let doc_dir = doc_dir ~context in
    (*
    let modules_and_odoc_files =
      if lib.wrapped then
        let main_module_name = String.capitalize_ascii lib.name in
        List.filter modules_and_odoc_files
          ~f:(fun (m, _) -> m.Module.name = main_module_name)
      else
        modules_and_odoc_files
       in*)
    let html_files =
      List.map modules_and_odoc_files ~f:(fun (m, odoc_file) ->
        to_html sctx m odoc_file ~doc_dir ~odoc ~dir ~includes ~lib
          ~lib_public_name:public.name)
    in
    let lib_index_html =
      lib_index sctx ~dir ~lib ~lib_public_name:public.name ~doc_dir
        ~modules ~includes ~odoc
    in
    Alias.add_deps (SC.aliases sctx) (Alias.doc ~dir)
      (css_file ~doc_dir
       :: lib_index_html
       :: html_files))

let setup_css_rule sctx =
  let context = SC.context sctx in
  let doc_dir = doc_dir ~context in
  SC.add_rule sctx
    (Build.run ~context
       ~dir:context.build_dir
       ~extra_targets:[css_file ~doc_dir]
       (get_odoc sctx)
       [ A "css"; A "-o"; Path doc_dir ]);
