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

  val odoc_file : odoc_dir:Path.t -> t -> Path.t
  val odoc_input : mld_dir:Path.t -> t -> Path.t

  val html_filename : t -> string
end = struct
  type t = string (** source file name without the extension. *)

  let create ~name = name

  let odoc_file ~odoc_dir t =
    Path.relative odoc_dir (sprintf "page-%s%s" t odoc_ext)

  let odoc_input ~mld_dir t =
    Path.relative mld_dir (sprintf "%s.mld" t)

  let html_filename t =
    sprintf "%s.html" t
end

module Module_or_mld = struct
  type t =
    | Mld of Mld.t
    | Module of Module.t

  let html_dir ~doc_dir = function
    | Mld _ -> doc_dir
    | Module m -> doc_dir ++ String.capitalize m.obj_name

  let html_file ~doc_dir t =
    match t with
    | Mld m -> html_dir ~doc_dir t ++ Mld.html_filename m
    | Module _ -> html_dir ~doc_dir t ++ "index.html"
end

let module_deps (m : Module.t) ~doc_dir
      ~(dep_graphs:Ocamldep.Dep_graphs.t) =
  Build.dyn_paths
    ((match m.intf with
       | Some _ ->
         Ocamldep.Dep_graph.deps_of dep_graphs.intf m
       | None ->
         (* When a module has no .mli, use the dependencies for the .ml *)
         Ocamldep.Dep_graph.deps_of dep_graphs.impl m)
     >>^ List.map ~f:(Module.odoc_file ~doc_dir))

let compile_mld sctx (m : Mld.t) ~odoc ~pkg ~odoc_dir ~mld_dir =
  let context = SC.context sctx in
  let odoc_file = Mld.odoc_file m ~odoc_dir in
  SC.add_rule sctx
    (Build.run ~context ~dir:odoc_dir odoc
       [ A "compile"
       (* ; A "-I"; Path odoc_dir *)
       ; As ["--pkg"; pkg]
       ; A "-o"; Target odoc_file
       ; Dep (Mld.odoc_input m ~mld_dir)
       ]);
  (Module_or_mld.Mld m, odoc_file)

let compile_module sctx (m : Module.t) ~odoc ~dir ~obj_dir ~includes ~dep_graphs
      ~doc_dir ~lib_unique_name =
  let context = SC.context sctx in
  let odoc_file = Module.odoc_file m ~doc_dir in
  SC.add_rule sctx
    (module_deps m ~doc_dir ~dep_graphs
     >>>
     includes
     >>>
     Build.run ~context ~dir:doc_dir odoc
       [ A "compile"
       ; A "-I"; Path dir
       ; Dyn (fun x -> x)
       ; As ["--pkg"; lib_unique_name]
       ; A "-o"; Target odoc_file
       ; Dep (Module.cmti_file m ~obj_dir)
       ]);
  (Module_or_mld.Module m, odoc_file)

let to_html sctx (m : Module_or_mld.t) odoc_file ~doc_dir ~odoc ~dir
      ~(includes : (unit, nothing Arg_spec.t) Build.t) ~libs =
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
    (List.map libs ~f:(SC.Doc.static_deps sctx)
     |> Build.all
     >>^ (fun _ -> ())
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

let css_file ~doc_dir = doc_dir ++ "odoc.css"

let toplevel_index ~doc_dir = doc_dir ++ "index.html"

let setup_library_rules sctx (lib : Library.t) ~dir ~scope ~modules
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
  let odoc = get_odoc sctx in
  let includes =
    let ctx = SC.context sctx in
    Build.memoize "includes"
      (requires
       >>> SC.Doc.deps sctx
       >>^ Lib.L.include_flags ~stdlib_dir:ctx.stdlib_dir)
  in
  let inputs_and_odoc_files =
    List.map (String_map.values modules) ~f:(fun m ->
      compile_module sctx ~odoc ~dir ~obj_dir ~includes ~dep_graphs
        ~doc_dir ~lib_unique_name m)
  in
  SC.Doc.setup_deps sctx lib (List.map inputs_and_odoc_files ~f:snd);
  (*
     let modules_and_odoc_files =
     if lib.wrapped then
     let main_module_name = String.capitalize lib.name in
     List.filter modules_and_odoc_files
     ~f:(fun (m, _) -> m.Module.name = main_module_name)
     else
     modules_and_odoc_files
     in*)
  let html_files =
    List.map inputs_and_odoc_files ~f:(fun (m, odoc_file) ->
      to_html sctx m odoc_file ~doc_dir ~odoc ~dir ~includes ~libs:[lib])
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

  let pkg_dir sctx ~(pkg : Package.t) =
    Path.append (SC.context sctx).build_dir pkg.path

let setup_package_rules =
  let mld_glob =
    Re.compile (
      Re.seq [Re.(rep1 any) ; Re.str ".mld" ; Re.eos]
    ) in
  fun sctx ~dir ~(pkg : Package.t) ->
    let odoc = get_odoc sctx in
    let mld_dir = SC.Doc.mld_dir sctx ~pkg:pkg.name in
    let mld_files = SC.eval_glob sctx ~dir:mld_dir mld_glob in
    let mld_files =
      List.map ~f:(fun f -> Mld.create ~name:(Filename.chop_extension f))
        mld_files in
    let includes = Build.arr (fun () -> Arg_spec.As []) in
    let mld_and_odoc_files =
      List.map mld_files ~f:(fun m ->
        compile_mld sctx ~odoc ~odoc_dir:dir ~pkg:pkg.name ~mld_dir m)
    in
    let html_files =
      List.map mld_and_odoc_files ~f:(fun (m, odoc_file) ->
        to_html sctx m odoc_file ~doc_dir:dir ~odoc ~dir ~includes ~libs:[])
    in
    SC.add_alias_deps sctx (Build_system.Alias.doc ~dir) html_files

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

let gen_rules sctx ~dir rest =
  match rest with
  | [] ->
    setup_css_rule sctx;
    setup_toplevel_index_rule sctx
  | "_mlds" :: pkg :: _ ->
    begin match String_map.find (SC.packages sctx) pkg with
    | None -> die "no documentation for non-existent package %s" pkg
    | Some _ ->
      List.iter ~f:(fun (d : Super_context.Dir_with_jbuild.t) ->
        List.iter d.stanzas ~f:(fun (stanza : Jbuild.Stanza.t) ->
          match stanza with
          | Documentation p when p.package.name = pkg ->
            let dir = Path.append ((SC.context sctx).build_dir) d.src_dir in
            SC.load_dir sctx ~dir
          | _ -> ()
        )
      ) (SC.stanzas sctx);
      let index_mld = "index.mld" in
      if not (String_set.mem (SC.Doc.mlds sctx ~pkg) index_mld) then (
        let mld_dir = SC.Doc.mld_dir sctx ~pkg in
        let index = Path.relative mld_dir index_mld in
        SC.Doc.register_mld sctx ~mld:index_mld ~pkg;
        SC.add_rule sctx (Build.write_file index "Generated index");
      )
    end
  | "_package" :: pkg :: _ ->
    begin match String_map.find (SC.packages sctx) pkg with
    | None ->
      die "no documentation for non-existent package %s" pkg
    | Some pkg -> setup_package_rules sctx ~dir ~pkg
    end
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
