open Import
open Jbuild
open Build.O

module SC = Super_context

let (++) = Path.relative

let lib_unique_name lib =
  let name = Lib.name lib in
  match Lib.status lib with
  | Installed -> assert false
  | Public _  -> name
  | Private scope_name -> SC.Scope_key.to_string name scope_name

let pkg_or_lnu lib =
  match Lib.package lib with
  | Some p -> Package.Name.to_string p
  | None -> lib_unique_name lib

type target =
  | Lib of Lib.t
  | Pkg of Package.Name.t

module Gen (S : sig val sctx : SC.t end) = struct
  open S

  let context = SC.context sctx

  module Paths = struct
    let root = context.Context.build_dir ++ "_doc"

    let odocs m =
      root ++ (
        match m with
        | Lib lib -> sprintf "_odoc/lib/%s" (lib_unique_name lib)
        | Pkg pkg -> sprintf "_odoc/pkg/%s" (Package.Name.to_string pkg)
      )

    let html_root = root ++ "_html"

    let html m =
      html_root ++ (
        match m with
        | Pkg pkg -> Package.Name.to_string pkg
        | Lib lib -> pkg_or_lnu lib
      )

    let gen_mld_dir (pkg : Package.t) =
      root ++ "_mlds" ++ (Package.Name.to_string pkg.name)
  end

  module Dep = struct
    let html_alias m =
      Build_system.Alias.doc ~dir:(Paths.html m)

    let alias = Build_system.Alias.make ".odoc-all"

    let deps =
      Build.dyn_paths (Build.arr (
        List.fold_left ~init:[] ~f:(fun acc (lib : Lib.t) ->
          if Lib.is_local lib then (
            let dir = Paths.odocs (Lib lib) in
            Build_system.Alias.stamp_file (alias ~dir) :: acc
          ) else (
            acc
          )
        )))

    let alias m = alias ~dir:(Paths.odocs m)

    (* let static_deps t lib = Build_system.Alias.dep (alias t lib) *)

    let setup_deps m files = SC.add_alias_deps sctx (alias m) files
  end

  let odoc = SC.resolve_program sctx "odoc" ~hint:"opam install odoc"
  let odoc_ext = ".odoc"

  module Mld : sig
    type t

    val create : Path.t -> t

    val odoc_file : doc_dir:Path.t -> t -> Path.t
    val odoc_input : t -> Path.t

  end = struct
    type t = Path.t

    let create p = p

    let odoc_file ~doc_dir t =
      let t = Filename.chop_extension (Path.basename t) in
      Path.relative doc_dir (sprintf "page-%s%s" t odoc_ext)

    let odoc_input t = t
  end

  let module_deps (m : Module.t) ~doc_dir ~(dep_graphs:Ocamldep.Dep_graphs.t) =
    Build.dyn_paths
      ((match m.intf with
         | Some _ ->
           Ocamldep.Dep_graph.deps_of dep_graphs.intf m
         | None ->
           (* When a module has no .mli, use the dependencies for the .ml *)
           Ocamldep.Dep_graph.deps_of dep_graphs.impl m)
       >>^ List.map ~f:(Module.odoc_file ~doc_dir))

  let compile_module (m : Module.t) ~obj_dir ~includes ~dep_graphs
        ~doc_dir ~pkg_or_lnu =
    let odoc_file = Module.odoc_file m ~doc_dir in
    SC.add_rule sctx
      (module_deps m ~doc_dir ~dep_graphs
       >>>
       includes
       >>>
       Build.run ~context ~dir:doc_dir odoc
         [ A "compile"
         ; A "-I"; Path doc_dir
         ; Dyn (fun x -> x)
         ; As ["--pkg"; pkg_or_lnu]
         ; A "-o"; Target odoc_file
         ; Dep (Module.cmti_file m ~obj_dir)
         ]);
    (m, odoc_file)

  let compile_mld (m : Mld.t) ~includes ~doc_dir ~pkg =
    let odoc_file = Mld.odoc_file m ~doc_dir in
    SC.add_rule sctx
      (includes
       >>>
       Build.run ~context ~dir:doc_dir odoc
         [ A "compile"
         ; Dyn (fun x -> x)
         ; As ["--pkg"; Package.Name.to_string pkg]
         ; A "-o"; Target odoc_file
         ; Dep (Mld.odoc_input m)
         ]);
    odoc_file

  type odoc =
    { odoc_input: Path.t
    ; html_dir: Path.t
    ; html_file: Path.t
    ; html_alias: Build_system.Alias.t
    ; typ: [`Module | `Mld]
    }

  let odoc_include_flags libs =
    let paths =
      libs |> List.fold_left ~f:(fun paths lib ->
        if Lib.is_local lib then (
          Path.Set.add paths (Paths.odocs (Lib lib))
        ) else (
          paths
        )
      ) ~init:Path.Set.empty in
    Arg_spec.S (List.concat_map (Path.Set.to_list paths)
                  ~f:(fun dir -> [Arg_spec.A "-I"; Path dir]))

  let to_html (odoc_file : odoc) ~deps =
    let to_remove, jbuilder_keep =
      match odoc_file.typ with
      | `Mld -> odoc_file.html_file, []
      | `Module ->
        let jbuilder_keep =
          Build.create_file (odoc_file.html_dir ++ Config.jbuilder_keep_fname) in
        odoc_file.html_dir, [jbuilder_keep]
    in
    SC.add_rule sctx
      (deps
       >>> Build.progn (
         Build.remove_tree to_remove
         :: Build.mkdir odoc_file.html_dir
         :: Build.run ~context ~dir:Paths.html_root
              odoc ~extra_targets:[odoc_file.html_file]
              [ A "html"
              ; Dyn odoc_include_flags
              ; A "-o"; Path Paths.html_root
              ; Dep odoc_file.odoc_input
              ]
         :: jbuilder_keep
       )
      );
    odoc_file.html_file

  let css_file = Paths.html_root ++ "odoc.css"

  let toplevel_index = Paths.html_root ++ "index.html"

  let setup_library_odoc_rules (library : Library.t) ~scope ~modules
        ~requires ~(dep_graphs:Ocamldep.Dep_graph.t Ml_kind.Dict.t) =
    let lib =
      Option.value_exn (Lib.DB.find_even_when_hidden (Scope.libs scope)
                          library.name) in
    (* Using the proper package name doesn't actually work since odoc assumes
       that a package contains only 1 library *)
    let pkg_or_lnu = pkg_or_lnu lib in
    let doc_dir = Paths.odocs (Lib lib) in
    let obj_dir = Lib.obj_dir lib in
    let includes =
      Build.memoize "includes"
        (requires
         >>> Dep.deps
         >>^ odoc_include_flags)
    in
    let modules_and_odoc_files =
      List.map (Module.Name.Map.values modules) ~f:(
        compile_module ~obj_dir ~includes ~dep_graphs
          ~doc_dir ~pkg_or_lnu)
    in
    Dep.setup_deps (Lib lib) (List.map modules_and_odoc_files ~f:snd
                             |> Path.Set.of_list)

  let setup_css_rule () =
    SC.add_rule sctx
      (Build.run ~context
         ~dir:context.build_dir
         ~extra_targets:[css_file]
         odoc
         [ A "css"; A "-o"; Path Paths.html_root ])

  let sp = Printf.sprintf

  let setup_toplevel_index_rule () =
    let list_items =
      Super_context.packages sctx
      |> Package.Name.Map.to_list
      |> List.filter_map ~f:(fun (name, pkg) ->
        let name = Package.Name.to_string name in
        let link = sp {|<a href="%s/index.html">%s</a>|} name name in
        let version_suffix =
          match pkg.Package.version_from_opam_file with
          | None ->
            ""
          | Some v ->
            sp {| <span class="version">%s</span>|} v
        in
        Some (sp "<li>%s%s</li>" link version_suffix))
    in
    let list_items = String.concat ~sep:"\n    " list_items in
    let html = sp
{|<!DOCTYPE html>
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
    </div>
  </body>
</html>|} list_items
    in
    SC.add_rule sctx @@ Build.write_file toplevel_index html


  let html_alias pkg =
    Build_system.Alias.doc ~dir:(
      Path.append context.build_dir pkg.Package.path
    )

  let libs_of_pkg ~pkg =
    match Package.Name.Map.find (SC.libs_by_package sctx) pkg with
    | None -> Lib.Set.empty
    | Some (_, libs) -> libs

  let load_all_odoc_rules_pkg ~pkg =
    let pkg_libs = libs_of_pkg ~pkg in
    SC.load_dir sctx ~dir:(Paths.odocs (Pkg pkg));
    Lib.Set.iter pkg_libs ~f:(fun lib ->
      SC.load_dir sctx ~dir:(Paths.odocs (Lib lib)));
    pkg_libs

  let create_odoc ~target odoc_input =
    let html_alias = Dep.html_alias target in
    let html_base = Paths.html target in
    match target with
    | Lib _ ->
      let html_dir =
        html_base ++ (
          Path.basename odoc_input
          |> Filename.chop_extension
          |> Stdune.String.capitalize
        ) in
      { odoc_input
      ; html_dir
      ; html_file = html_dir ++ "index.html"
      ; typ = `Module
      ; html_alias
      }
    | Pkg _ ->
      { odoc_input
      ; html_dir = html_base
      ; html_file = html_base ++ sprintf "%s.html" (
          Path.basename odoc_input
          |> Filename.chop_extension
          |> String.drop_prefix ~prefix:"page-"
          |> Option.value_exn
        )
      ; typ = `Mld
      ; html_alias
      }

  let setup_pkg_html_rules =
    let loaded = Package.Name.Table.create ~default_value:None in
    let odoc_glob =
      Re.compile (Re.seq [Re.(rep1 any) ; Re.str ".odoc" ; Re.eos]) in
    fun ~pkg ~libs ->
      if Package.Name.Table.get loaded pkg = None then begin
        Package.Name.Table.set loaded ~key:pkg ~data:(Some ());
        let odocs =
          let odocs target =
            let dir = Paths.odocs target in
            SC.eval_glob sctx ~dir odoc_glob
            |> List.map ~f:(fun d -> create_odoc (Path.relative dir d) ~target)
          in
          List.concat (
            odocs (Pkg pkg)
            :: (List.map libs ~f:(fun lib -> odocs (Lib lib)))
          ) in
        let html_files =
          let closure =
            match Lib.closure libs with
            | Ok closure -> closure
            | Error _ -> libs in
          let deps = Build.return closure >>> Dep.deps in
          List.map odocs ~f:(to_html ~deps) in
        List.iter (
          Dep.html_alias (Pkg pkg)
          :: List.map ~f:(fun lib -> Dep.html_alias (Lib lib)) libs
        ) ~f:(fun alias ->
          SC.add_alias_deps sctx alias
            (Path.Set.of_list [ css_file
                              ; toplevel_index
                              ])
        );
        List.combine odocs html_files
        |> List.iter ~f:(fun (odoc, html) ->
          SC.add_alias_deps sctx odoc.html_alias (Path.Set.singleton html)
        );
      end

  let gen_rules ~dir:_ rest =
    match rest with
    | ["_html"] ->
      setup_css_rule ();
      setup_toplevel_index_rule ()
    | "_mlds" :: _pkg :: _
    | "_odoc" :: "pkg" :: _pkg :: _ ->
      () (* rules were already setup lazily in gen_rules *)
    | "_odoc" :: "lib" :: lib :: _ ->
      let lib, lib_db = SC.Scope_key.of_string sctx lib in
      begin match Lib.DB.find lib_db lib with
      | Error _ -> ()
      | Ok lib  -> SC.load_dir sctx ~dir:(Lib.src_dir lib)
      end
    | "_html" :: lib_unique_name_or_pkg :: _ ->
      let setup_html_rules pkg =
        setup_pkg_html_rules ~pkg ~libs:(
          Lib.Set.to_list (load_all_odoc_rules_pkg ~pkg)
        ) in
      (* TODO we can be a better with the error handling in the case where
         lib_unique_name_or_pkg is neither a valid pkg or lnu *)
      let lib, lib_db = SC.Scope_key.of_string sctx lib_unique_name_or_pkg in
      begin match Lib.DB.find lib_db lib with
      | Error _ -> ()
      | Ok lib  -> Option.iter (Lib.package lib) ~f:setup_html_rules
      end;
      Option.iter
        (Package.Name.Map.find (SC.packages sctx)
           (Package.Name.of_string lib_unique_name_or_pkg))
        ~f:(fun pkg -> setup_html_rules pkg.name)
    | _ -> ()

  let setup_package_aliases (pkg : Package.t) =
    let alias = html_alias pkg in
    SC.add_alias_deps sctx alias (
      Dep.html_alias (Pkg pkg.name)
      :: (libs_of_pkg ~pkg:pkg.name
          |> Lib.Set.to_list
          |> List.map ~f:(fun lib -> Dep.html_alias (Lib lib)))
      |> List.map ~f:Build_system.Alias.stamp_file
      |> Path.Set.of_list
    )

  let entry_modules ~(pkg : Package.t) ~entry_modules_by_lib =
    libs_of_pkg ~pkg:pkg.name
    |> Lib.Set.to_list
    |> List.filter_map ~f:(fun l ->
      if Lib.is_local l then (
        Some (l, entry_modules_by_lib l)
      ) else (
        None
      ))
    |> Lib.Map.of_list_exn

  let default_index entry_modules =
    let b = Buffer.create 512 in
    Lib.Map.to_list entry_modules
    |> List.sort ~compare:(fun (x, _) (y, _) ->
      String.compare (Lib.name x) (Lib.name y))
    |> List.iter ~f:(fun (lib, modules) ->
      Buffer.add_string b (
        sprintf
          "{1 Library %s}\n\
           This library exposes the following toplevel modules:\n\
           {!modules:%s}\n"
          (Lib.name lib)
          (modules
           |> List.sort ~compare:(fun x y ->
             Module.Name.compare (Module.name x) (Module.name y))
           |> List.map ~f:(fun m -> Module.Name.to_string (Module.name m))
           |> String.concat ~sep:" ")
      )
    );
    Buffer.contents b

  let check_mlds_no_dupes ~pkg ~mlds =
    match
      List.map mlds ~f:(fun mld -> (Path.basename mld, mld))
      |> String_map.of_list
    with
    | Ok m -> m
    | Error (_, p1, p2) ->
      die "Package %s has two mld's with the same basename %s, %s"
        (Package.Name.to_string pkg.Package.name)
        (Path.to_string_maybe_quoted p1)
        (Path.to_string_maybe_quoted p2)

  let setup_package_odoc_rules ~pkg ~mlds ~entry_modules_by_lib =
    let mlds = check_mlds_no_dupes ~pkg ~mlds in
    let mlds =
      if String_map.mem mlds "index" then
        mlds
      else
        let entry_modules = entry_modules ~pkg ~entry_modules_by_lib in
        let gen_mld = Paths.gen_mld_dir pkg ++ "index.mld" in
        SC.add_rule sctx (
          Build.write_file gen_mld (default_index entry_modules)
        );
        String_map.add mlds "index" gen_mld in
    let odocs = List.map (String_map.values mlds) ~f:(fun mld ->
      compile_mld
        (Mld.create mld)
        ~pkg:pkg.name
        ~doc_dir:(Paths.odocs (Pkg pkg.name))
        ~includes:(Build.arr (fun _ -> Arg_spec.As []))
    ) in
    Dep.setup_deps (Pkg pkg.name) (Path.Set.of_list odocs)

  let init ~modules_by_lib ~mlds_of_dir =
    let docs_by_package =
      let map = lazy (
        SC.stanzas sctx
        |> List.concat_map ~f:(fun (w : SC.Dir_with_jbuild.t) ->
          List.filter_map w.stanzas ~f:(function
            | Jbuild.Stanza.Documentation (d : Jbuild.Documentation.t) ->
              Some (d.package.name, (w.ctx_dir, d))
            | _ ->
              None
          ))
        |> Package.Name.Map.of_list_multi
      ) in
      fun (p : Package.t) ->
        Option.value (Package.Name.Map.find (Lazy.force map) p.name) ~default:[]
    in
    let modules_by_lib =
      let module M = Map.Make(
      struct
        type t = Path.t * string
        let compare (d1, l1) (d2, l2) =
          match Path.compare d1 d2 with
          | Ordering.Eq -> String.compare l1 l2
          | o -> o
      end) in
      let lib_to_library = lazy (
        SC.stanzas sctx
        |> List.concat_map ~f:(fun (w : SC.Dir_with_jbuild.t) ->
          List.filter_map w.stanzas ~f:(function
            | Jbuild.Stanza.Library (l : Library.t) ->
              Some ((w.ctx_dir, Library.best_name l), l)
            | _ ->
              None
          ))
        |> M.of_list_exn
      ) in
      fun lib ->
        let dir = Lib.src_dir lib in
        let library = Option.value_exn (
          M.find (Lazy.force lib_to_library) (dir, Lib.name lib)
        ) in
        modules_by_lib ~dir library
    in
    SC.packages sctx
    |> Package.Name.Map.iter ~f:(fun (pkg : Package.t) ->
      let rules = lazy (
        setup_package_odoc_rules
          ~pkg
          ~mlds:(
            docs_by_package pkg
            |> List.concat_map ~f:(fun (dir, doc) -> mlds_of_dir doc ~dir)
          )
          ~entry_modules_by_lib:modules_by_lib
      ) in
      List.iter [ Paths.odocs (Pkg pkg.name)
                ; Paths.gen_mld_dir pkg ]
        ~f:(fun dir ->
          SC.on_load_dir sctx ~dir ~f:(fun () -> Lazy.force rules));
      (* setup @doc to build the correct html for the package *)
      setup_package_aliases pkg;
    );
    Super_context.add_alias_deps
      sctx
      (Build_system.Alias.private_doc ~dir:context.build_dir)
      (SC.stanzas sctx
       |> List.concat_map ~f:(fun (w : SC.Dir_with_jbuild.t) ->
         List.filter_map w.stanzas ~f:(function
           | Jbuild.Stanza.Library (l : Jbuild.Library.t) ->
             begin match l.public with
             | Some _ -> None
             | None ->
               let scope = SC.find_scope_by_dir sctx w.ctx_dir in
               Some (Option.value_exn (
                 Lib.DB.find_even_when_hidden (Scope.libs scope) l.name)
               )
             end
           | (_ : Jbuild.Stanza.t) -> None
         ))
       |> List.map ~f:(fun (lib : Lib.t) ->
         Build_system.Alias.stamp_file (Dep.alias (Lib lib)))
       |> Path.Set.of_list
      )

end
