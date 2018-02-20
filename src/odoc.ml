open Import
open Jbuild
open Build.O

module SC = Super_context

let ( ++ ) = Path.relative

let get_odoc sctx = SC.resolve_program sctx "odoc" ~hint:"opam install odoc"
let odoc_ext = ".odoc"

module Mld : sig
  type t
  val create : name:string -> t

  val odoc_file : doc_dir:Path.t -> t -> Path.t
  val odoc_input : doc_dir:Path.t -> t -> Path.t

  val html_filename : t -> string
end = struct
  type t = string (** source file name without the extension. *)

  let create ~name = name

  let odoc_file ~doc_dir t =
    Path.relative doc_dir (sprintf "page-%s%s" t odoc_ext)

  let odoc_input ~doc_dir t =
    Path.relative doc_dir (sprintf "%s-generated.mld" t)

  let html_filename t =
    sprintf "%s.html" t
end

module Module_or_mld = struct
  type t =
    | Mld of Mld.t
    | Module of Module.t

  let odoc_file ~doc_dir = function
    | Mld m -> Mld.odoc_file ~doc_dir m
    | Module m -> Module.odoc_file ~doc_dir m

  let odoc_input ~obj_dir ~doc_dir = function
    | Mld m -> Mld.odoc_input ~doc_dir m
    | Module m -> Module.cmti_file m ~obj_dir

  let html_dir ~doc_dir = function
    | Mld _ -> doc_dir
    | Module m -> doc_dir ++ String.capitalize_ascii m.obj_name

  let html_file ~doc_dir t =
    match t with
    | Mld m -> html_dir ~doc_dir t ++ Mld.html_filename m
    | Module _ -> html_dir ~doc_dir t ++ "index.html"
end

let module_or_mld_deps (m : Module_or_mld.t) ~doc_dir
      ~(dep_graphs:Ocamldep.Dep_graphs.t) =
  match m with
  | Mld _ ->
    Build.arr (fun x -> x)
  | Module m ->
    Build.dyn_paths
      ((match m.intf with
         | Some _ ->
           Ocamldep.Dep_graph.deps_of dep_graphs.intf m
         | None ->
           (* When a module has no .mli, use the dependencies for the .ml *)
           Ocamldep.Dep_graph.deps_of dep_graphs.impl m)
       >>^ List.map ~f:(Module.odoc_file ~doc_dir))

let compile sctx (m : Module_or_mld.t) ~odoc ~dir ~obj_dir ~includes ~dep_graphs
      ~doc_dir ~lib_unique_name =
  let context = SC.context sctx in
  let odoc_file = Module_or_mld.odoc_file m ~doc_dir in
  SC.add_rule sctx
    (module_or_mld_deps m ~doc_dir ~dep_graphs
     >>>
     includes
     >>>
     Build.run ~context ~dir:doc_dir odoc
       [ A "compile"
       ; A "-I"; Path dir
       ; Dyn (fun x -> x)
       ; As ["--pkg"; lib_unique_name]
       ; A "-o"; Target odoc_file
       ; Dep (Module_or_mld.odoc_input m ~obj_dir ~doc_dir)
       ]);
  (m, odoc_file)

let to_html sctx (m : Module_or_mld.t) odoc_file ~doc_dir ~odoc ~dir ~includes
      ~(lib : Library.t) =
  let context = SC.context sctx in
  let html_dir = Module_or_mld.html_dir ~doc_dir m in
  let html_file = Module_or_mld.html_file ~doc_dir m in
  let to_remove, jbuilder_keep =
    match m with
    | Mld _ -> html_file, []
    | Module _ ->
      let jbuilder_keep =
        Build.create_file (html_dir ++ Config.jbuilder_keep_fname) in
      html_dir, [jbuilder_keep]
  in
  SC.add_rule sctx
    (SC.Doc.static_deps sctx lib
     >>>
     includes
     >>>
     Build.progn (
       Build.remove_tree to_remove
       :: Build.mkdir html_dir
       :: Build.run ~context ~dir odoc ~extra_targets:[html_file]
            [ A "html"
            ; A "-I"; Path doc_dir
            ; Dyn (fun x -> x)
            ; A "-o"; Path (Path.parent doc_dir)
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
  let doc_dir = SC.Doc.dir sctx lib in
  List.map all_files ~f:(fun file ->
    let name = Filename.chop_extension file in
    let mld = Mld.create ~name in
    let generated_mld = Mld.odoc_input ~doc_dir mld in
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
    mld
  )

let css_file ~doc_dir = doc_dir ++ "odoc.css"

let toplevel_index ~doc_dir = doc_dir ++ "index.html"

let setup_library_rules sctx (lib : Library.t) ~dir ~scope ~modules ~mld_files
      ~requires ~(dep_graphs:Ocamldep.Dep_graph.t Ml_kind.Dict.t) =
  let doc_dir = SC.Doc.dir sctx lib in
  let obj_dir, lib_unique_name =
    let obj_dir, name, status =
      match Lib.DB.find (Scope.libs scope) lib.name with
      | Error Not_found -> assert false
      | Error (Hidden { name; info; _ }) ->
        (info.obj_dir, name, info.status)
      | Ok lib ->
        (Lib.obj_dir lib, Lib.name lib, Lib.status lib)
    in
    let name =
      match status with
      | Installed -> assert false
      | Public    -> name
      | Private scope_name ->
        sprintf "%s@%s" name (Scope_info.Name.to_string scope_name)
    in
    (obj_dir, name)
  in
  let lib_name = Library.best_name lib in
  let odoc = get_odoc sctx in
  let includes =
    let ctx = SC.context sctx in
    Build.memoize "includes"
      (requires
       >>> SC.Doc.deps sctx
       >>^ Lib.L.include_flags ~stdlib_dir:ctx.stdlib_dir)
  in
  let mld_files =
    all_mld_files sctx ~dir ~lib ~lib_name ~modules mld_files
  in
  let mld_and_odoc_files =
    List.map mld_files ~f:(fun m ->
      compile sctx ~odoc ~dir ~obj_dir ~includes ~dep_graphs
        ~doc_dir ~lib_unique_name (Mld m))
  in
  let modules_and_odoc_files =
    List.map (String_map.values modules) ~f:(fun m ->
      compile sctx ~odoc ~dir ~obj_dir ~includes ~dep_graphs
        ~doc_dir ~lib_unique_name (Module m))
  in
  let inputs_and_odoc_files = modules_and_odoc_files @ mld_and_odoc_files in
  SC.Doc.setup_deps sctx lib (List.map inputs_and_odoc_files ~f:snd);
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
      to_html sctx m odoc_file ~doc_dir ~odoc ~dir ~includes ~lib)
  in
  let doc_root = SC.Doc.root sctx in
  let alias =
    match lib.public with
    | None -> Build_system.Alias.private_doc ~dir
    | Some _ -> Build_system.Alias.doc ~dir in
  SC.add_alias_deps sctx alias
    (css_file ~doc_dir:doc_root
     :: toplevel_index ~doc_dir:doc_root
     :: html_files)

let setup_css_rule sctx =
  let context = SC.context sctx in
  let doc_dir = SC.Doc.root sctx in
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
    |> List.filter_map ~f:(fun (_path, _scope, stanza) ->
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
  let doc_dir = SC.Doc.root sctx in
  SC.add_rule sctx @@ Build.write_file (toplevel_index ~doc_dir) html

let gen_rules sctx ~dir:_ rest =
  match rest with
  | [] ->
    setup_css_rule sctx;
    setup_toplevel_index_rule sctx
  | lib :: _ ->
    let lib, lib_db =
      match String.rsplit2 lib ~on:'@' with
      | None ->
        (lib, SC.public_libs sctx)
      | Some (lib, name) ->
        (lib,
         Scope.libs
           (SC.find_scope_by_name sctx (Scope_info.Name.of_string name)))
    in
    match Lib.DB.find lib_db lib with
    | Error _ -> ()
    | Ok lib  -> SC.load_dir sctx ~dir:(Lib.src_dir lib)
