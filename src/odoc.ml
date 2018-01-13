open Import
open Jbuild
open Build.O

module SC = Super_context

let ( ++ ) = Path.relative

let get_odoc sctx = SC.resolve_program sctx "odoc" ~hint:"opam install odoc"
let odoc_ext = ".odoc"

module Mld = struct
  type t = {
    name : string; (** source file name without the extension. *)
    odoc_input_name : string;
    odoc_file_name  : string;
  }

  let odoc_file ~dir {odoc_file_name; _} =
    Path.relative dir odoc_file_name

  let odoc_input ~dir {odoc_input_name; _} =
    Path.relative dir odoc_input_name
end

module Module_or_mld = struct
  type t =
    | Mld of Mld.t
    | Module of Module.t

  let odoc_file ~dir = function
    | Mld m -> Mld.odoc_file ~dir m
    | Module m -> Module.odoc_file ~dir m

  let odoc_input ~dir = function
    | Mld m -> Mld.odoc_input ~dir m
    | Module m -> Module.cmti_file m ~dir
end

let module_or_mld_deps (m : Module_or_mld.t) ~dir ~dep_graph ~modules =
  Build.dyn_paths
    (dep_graph
     >>^ fun graph ->
     match m with
     | Mld _ -> []
     | Module m ->
       List.map (Utils.find_deps ~dir graph m.name)
         ~f:(fun name ->
           let m = Utils.find_module ~dir modules name in
           Module.odoc_file m ~dir))

let compile sctx (m : Module_or_mld.t) ~odoc ~dir ~includes ~dep_graph
      ~modules ~lib_unique_name =
  let context = SC.context sctx in
  let odoc_file = Module_or_mld.odoc_file m ~dir in
  SC.add_rule sctx
    (module_or_mld_deps m ~dir ~dep_graph ~modules
     >>>
     includes
     >>>
     Build.run ~context ~dir odoc ~extra_targets:[odoc_file]
       [ A "compile"
       ; A "-I"; Path dir
       ; Dyn (fun x -> x)
       ; As ["--pkg"; lib_unique_name]
       ; A "-o"; Path odoc_file
       ; Dep (Module_or_mld.odoc_input m ~dir)
       ]);
  (m, odoc_file)

let to_html sctx (m : Module_or_mld.t) odoc_file ~doc_dir ~odoc ~dir ~includes
      ~lib_unique_name ~(lib : Library.t) =
  let context = SC.context sctx in
  let to_remove, html_dir, html_file, jbuilder_keep =
    match m with
    | Mld m ->
      let html_dir = doc_dir ++ lib_unique_name in
      let html_file = html_dir ++ (m.Mld.name ^ ".html") in
      html_file, html_dir, html_file, []
    | Module m ->
      let html_dir = doc_dir ++ lib_unique_name ++ String.capitalize_ascii m.obj_name in
      let html_file = html_dir ++ "index.html" in
      let jbuilder_keep =
       Build.create_file (html_dir ++ Config.jbuilder_keep_fname)
      in
      html_dir, html_dir, html_file, [jbuilder_keep]
  in
  SC.add_rule sctx
    (SC.Libs.static_file_deps (dir, lib) ~ext:odoc_ext
     >>>
     includes
     >>>
     Build.progn (
       Build.remove_tree to_remove
       :: Build.mkdir html_dir
       :: Build.run ~context ~dir odoc ~extra_targets:[html_file]
            [ A "html"
            ; A "-I"; Path dir
            ; Dyn (fun x -> x)
            ; A "-o"; Path doc_dir
            ; Dep odoc_file
            ]
       :: jbuilder_keep
     )
    );
  html_file

let all_mld_files sctx ~(lib : Library.t) ~lib_name ~modules ~dir files =
  let all_files =
    if List.mem "index.mld" ~set:files then files else "index.mld" :: files
  in
  List.map all_files ~f:(fun file ->
    let name = Filename.chop_extension file in
    let odoc_input_name = sprintf "%s-generated.mld" name in
    let odoc_file_name = sprintf "page-%s%s" name odoc_ext in
    let generated_mld = dir ++ odoc_input_name in
    let source_mld = dir ++ file in
    SC.add_rule sctx
      (Build.if_file_exists source_mld
         ~then_:(Build.contents source_mld)
         ~else_:(Build.arr (fun () ->
           (if lib.wrapped then
              sprintf
                "{1 Library %s}\n\
                 The entry point for this library is module {!module:%s}."
                lib_name
                (String.capitalize_ascii lib.name)
            else
              sprintf
                "{1 Library %s}\n\
                 This library exposes the following toplevel modules: {!modules:%s}."
                lib_name
                (String_map.keys modules |> String.concat ~sep:" "))))
       >>>
       Build.write_file_dyn generated_mld);
    { Mld. name; odoc_file_name; odoc_input_name }
  )

let doc_dir ~context = Path.relative context.Context.build_dir "_doc"

let css_file ~doc_dir = doc_dir ++ "odoc.css"

let toplevel_index ~doc_dir = doc_dir ++ "index.html"

let setup_library_rules sctx (lib : Library.t) ~dir ~modules ~mld_files
      ~requires ~(dep_graph:Ocamldep.dep_graph) =
  let lib_unique_name = SC.unique_library_name sctx (Internal (dir, lib)) in
  let lib_name = Library.best_name lib in
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
  let mld_files =
    all_mld_files sctx ~dir ~lib ~lib_name ~modules mld_files
  in
  let mld_and_odoc_files =
    List.map mld_files ~f:(fun m ->
      compile sctx ~odoc ~dir ~includes ~dep_graph ~modules
        ~lib_unique_name (Mld m))
  in
  let modules_and_odoc_files =
    List.map (String_map.values modules) ~f:(fun m ->
      compile sctx ~odoc ~dir ~includes ~dep_graph ~modules
        ~lib_unique_name (Module m))
  in
  let inputs_and_odoc_files = modules_and_odoc_files @ mld_and_odoc_files in
  SC.Libs.setup_file_deps_alias sctx ~ext:odoc_ext (dir, lib)
    (List.map inputs_and_odoc_files ~f:snd);
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
    List.map inputs_and_odoc_files ~f:(fun (m, odoc_file) ->
      to_html sctx m odoc_file ~doc_dir ~odoc ~dir ~includes ~lib
        ~lib_unique_name)
  in
  SC.add_alias_deps sctx (Build_system.Alias.doc ~dir)
    (css_file ~doc_dir
     :: toplevel_index ~doc_dir
     :: html_files)

let setup_css_rule sctx =
  let context = SC.context sctx in
  let doc_dir = doc_dir ~context in
  SC.add_rule sctx
    (Build.run ~context
       ~dir:context.build_dir
       ~extra_targets:[css_file ~doc_dir]
       (get_odoc sctx)
       [ A "css"; A "-o"; Path doc_dir ])

let sp = Printf.sprintf

let setup_toplevel_index_rule sctx =
  let list_items =
    Super_context.stanzas_to_consider_for_install sctx
    |> List.filter_map ~f:(fun (_path, stanza) ->
      match stanza with
      | Stanza.Library
          {Library.kind = Library.Kind.Normal; public = Some public_info; _} ->
        let name = public_info.name in
        let link = sp {|<a href="%s/index.html">%s</a>|} name name in
        let version_suffix =
          match public_info.package.Package.version_from_opam_file with
          | None ->
            ""
          | Some v ->
            sp {| <span class="version">%s</span>|} v
        in
        Some (sp "<li>%s%s</li>" link version_suffix)

      | _ ->
        None)
  in
  let list_items = String.concat ~sep:"\n    " list_items in
  let html =
    sp {|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>index</title>
    <link rel="stylesheet" href="./odoc.css"/>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
  </head>
  <body>
    <div class="by-name">
    <h2>OCaml package documentation</h2>
    <ol>
    %s
    </ol>
 </body>
 </html>
|} list_items
  in
  let context = SC.context sctx in
  let doc_dir = doc_dir ~context in
  SC.add_rule sctx @@ Build.write_file (toplevel_index ~doc_dir) html

let gen_rules sctx ~dir rest =
  match rest with
  | [] ->
    setup_css_rule sctx;
    setup_toplevel_index_rule sctx
  | lib :: _ ->
    match Lib_db.find (SC.libs sctx) ~from:dir lib with
    | None | Some (External _) -> ()
    | Some (Internal (dir, _)) -> SC.load_dir sctx ~dir
