open Import
open Plugin
open Memo.O

let meta_file ~dir { name; libraries = _; site = _, (pkg, site); _ } =
  Path.Build.L.relative
    dir
    [ ".site"
    ; Package.Name.to_string pkg
    ; Site.to_string site
    ; Package.Name.to_string name
    ; Dune_findlib.Package.meta_fn
    ]
;;

let resolve_libs t public_libs =
  Resolve.Memo.List.map t.libraries ~f:(Lib.DB.resolve public_libs)
;;

let setup_rules ~sctx ~dir t =
  (let open Action_builder.O in
   let* public_libs =
     Super_context.context sctx
     |> Context.name
     |> Scope.DB.public_libs
     |> Action_builder.of_memo
   in
   Resolve.Memo.read
   @@
   let open Resolve.Memo.O in
   let+ meta =
     let+ requires = resolve_libs t public_libs in
     { Meta.name = None
     ; entries = [ Gen_meta.requires (Lib_name.Set.of_list_map ~f:Lib.name requires) ]
     }
   in
   Format.asprintf "%a" Pp.to_fmt (Pp.vbox (Pp.seq (Meta.pp meta.entries) Pp.cut)))
  |> (let meta = meta_file ~dir t in
      Action_builder.write_file_dyn meta)
  |> Super_context.add_rule sctx ~dir
;;

let install_rules ~sctx ~package_db ~dir ({ name; site = loc, (pkg, site); _ } as t) =
  let* skip_files =
    let* public_libs =
      Super_context.context sctx |> Context.name |> Scope.DB.public_libs
    in
    if t.optional
    then Resolve.Memo.is_error (resolve_libs t public_libs)
    else Memo.return false
  in
  if skip_files
  then Memo.return []
  else (
    let meta = meta_file ~dir t in
    let+ entry =
      Install_entry_with_site.make_with_site
        ~dst:(sprintf "%s/%s" (Package.Name.to_string name) Dune_findlib.Package.meta_fn)
        (Site { pkg; site; loc })
        (Package_db.section_of_site package_db)
        ~kind:`File
        meta
    in
    [ Install.Entry.Sourced.create ~loc entry ])
;;
