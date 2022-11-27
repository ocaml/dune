open Import
open Dune_file.Plugin
open Memo.O

let meta_file ~dir { name; libraries = _; site = _, (pkg, site); _ } =
  Path.Build.L.relative dir
    [ ".site"
    ; Package.Name.to_string pkg
    ; Dune_engine.Section.Site.to_string site
    ; Package.Name.to_string name
    ; Findlib.meta_fn
    ]

let resolve_libs t public_libs =
  Resolve.Memo.List.map t.libraries ~f:(Lib.DB.resolve public_libs)

let setup_rules ~sctx ~dir t =
  let meta = meta_file ~dir t in
  let* public_libs = Scope.DB.public_libs (Super_context.context sctx) in
  Super_context.add_rule sctx ~dir
    (Action_builder.write_file_dyn meta
       (Resolve.Memo.read
          (let open Resolve.Memo.O in
          let+ requires = resolve_libs t public_libs in
          let meta =
            { Meta.name = None
            ; entries =
                [ Gen_meta.requires
                    (Lib_name.Set.of_list_map ~f:Lib.name requires)
                ]
            }
          in
          Format.asprintf "%a" Pp.to_fmt
            (Pp.vbox (Pp.seq (Meta.pp meta.entries) Pp.cut)))))

let install_rules ~sctx ~sites ~dir ({ name; site = loc, (pkg, site); _ } as t)
    =
  let* skip_files =
    let* public_libs = Scope.DB.public_libs (Super_context.context sctx) in
    if t.optional then Resolve.Memo.is_error (resolve_libs t public_libs)
    else Memo.return false
  in
  if skip_files then Memo.return []
  else
    let meta = meta_file ~dir t in
    let+ entry =
      Install.Entry.make_with_site
        ~dst:(sprintf "%s/%s" (Package.Name.to_string name) Findlib.meta_fn)
        (Site { pkg; site; loc })
        (Sites.section_of_site sites)
        ~kind:`File meta
    in
    [ Install.Entry.Sourced.create ~loc entry ]
