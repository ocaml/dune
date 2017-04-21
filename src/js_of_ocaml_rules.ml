open Import
module C = Super_context
let separate_compilation_enabled () = !Clflags.dev_mode

let pretty () = if !Clflags.dev_mode then ["--pretty"] else []
let sourcemap () = if !Clflags.dev_mode then ["--source-map-inline"] else []

let standard () = pretty () @ sourcemap ()

let install_jsoo_hint = "opam install js_of_ocaml-compiler"

let in_build_dir ~ctx =
  let init = Path.of_string (sprintf "_build/.js/%s" ctx.Context.name) in
  List.fold_left ~init ~f:Path.relative

let runtime_file ~sctx ~dir fname =
  let _lib, file =
    Artifacts.file_of_lib (C.artifacts sctx) ~from:dir ~use_provides:false
      (sprintf "js_of_ocaml-compiler:%s" fname)
  in
  match file with
  | Error _ ->
    Arg_spec.Dyn (fun _ ->
      Utils.library_not_found ~context:(C.context sctx).name ~hint:install_jsoo_hint "js_of_ocaml-compiler")
  | Ok f -> Arg_spec.Dep f

let prog_spec ~sctx ~hint bin =  match Artifacts.binary (C.artifacts sctx) bin with
  | Error _ ->
    Build.Prog_spec.Dyn (fun _ ->
      Utils.program_not_found ~context:(C.context sctx).name ~hint bin)
  | Ok p -> Build.Prog_spec.Dep p

let js_of_ocaml_rule ~sctx ~dir ~flags ~spec ~target =
  let jsoo = prog_spec ~sctx ~hint:install_jsoo_hint "js_of_ocaml" in
  let runtime = runtime_file ~sctx ~dir "runtime.js" in
  Build.run ~context:(C.context sctx) ~dir
    jsoo
    [ Arg_spec.As flags
    ; Arg_spec.A "-o"; Target target
    ; Arg_spec.A "--no-runtime"; runtime
    ; spec
    ]

let standalone_runtime_rule ~sctx ~dir ~flags ~javascript_files ~target =
  let spec =
    Arg_spec.S
      [ Arg_spec.Dyn (fun (libs,_) -> Arg_spec.Deps (Lib.jsoo_runtime_files libs))
      ; Arg_spec.Deps javascript_files
      ]
  in
  let flags = Ordered_set_lang.eval_with_standard flags ~standard:(standard ()) in
  let flags = "--runtime-only" :: flags in
  js_of_ocaml_rule ~sctx ~dir ~flags ~target ~spec

let exe_rule ~sctx ~dir ~flags ~javascript_files ~src ~target =
  let spec =
    Arg_spec.S
      [ Arg_spec.Dyn (fun (libs,_) -> Arg_spec.Deps (Lib.jsoo_runtime_files libs))
      ; Arg_spec.Deps javascript_files
      ; Arg_spec.Dep src
      ]
  in
  let flags = Ordered_set_lang.eval_with_standard flags ~standard:(standard ()) in
  js_of_ocaml_rule ~sctx ~dir ~flags ~spec ~target

let link_rule ~sctx ~dir ~runtime ~target =
  let ctx = C.context sctx in
  let get_all (libs,cm) =
    (* Special case for the stdlib because it is not referenced in the META *)
    let stdlib = Lib.External (Findlib.stdlib_with_archives ctx.findlib) in
    let all_libs =
      List.concat_map (stdlib :: libs) ~f:(function
        | Lib.External pkg ->
          List.map (Mode.Dict.get pkg.archives Mode.Byte) ~f:(fun name ->
            in_build_dir ~ctx [pkg.name; sprintf "%s.js" name])
        | Lib.Internal (dir, lib) ->
          [ Path.relative dir (sprintf "%s.cma.js" lib.name) ]
      )
    in
    let all_other_modules = List.map cm ~f:(fun m -> Path.extend_basename m ~suffix:".js") in
    Arg_spec.Deps (List.concat [all_libs;all_other_modules])
  in
  let jsoo_link = prog_spec ~sctx ~hint:install_jsoo_hint "jsoo_link" in
  Build.run ~context:(C.context sctx) ~dir
    jsoo_link
    [ Arg_spec.A "-o"; Target target
    ; Arg_spec.Dep runtime
    ; Arg_spec.As (sourcemap ())
    ; Arg_spec.Dyn get_all
    ]

let build_cm ~sctx ~dir ~js_of_ocaml ~src =
  if separate_compilation_enabled ()
  then let target = Path.extend_basename src ~suffix:".js" in
    let spec = Arg_spec.Dep src in
    let flags =
      Ordered_set_lang.eval_with_standard
        js_of_ocaml.Jbuild_types.Js_of_ocaml.flags
        ~standard:(standard ())
    in
    [ js_of_ocaml_rule ~sctx ~dir ~flags ~spec ~target ]
  else []

let setup_findlib ~sctx =
  if separate_compilation_enabled ()
  then
    let ctx = C.context sctx in
    let all_pkg =
      List.map
        (Findlib.all_packages ctx.findlib)
        ~f:(fun pkg ->
          (* Special case for the stdlib because it is not referenced in the META *)
          let pkg =
            if pkg.Findlib.name = "stdlib"
            then Findlib.stdlib_with_archives ctx.findlib
            else pkg
          in
          let archives = Mode.Dict.get pkg.Findlib.archives Mode.Byte in
          pkg.Findlib.name, pkg.dir, archives)
    in
    List.concat_map all_pkg
      ~f:(fun (pkg_name,pkg_dir,archives) ->
        List.map archives ~f:(fun name ->
          let src = Path.relative pkg_dir name in
          let target = in_build_dir ~ctx [ pkg_name; sprintf "%s.js" name] in
          let dir = in_build_dir ~ctx [ pkg_name ] in
          let spec = Arg_spec.Dep src in
          let flags = standard () in
          js_of_ocaml_rule ~sctx ~dir ~flags ~spec ~target
        ))
  else []

let build_exe ~sctx ~dir ~js_of_ocaml ~src =
  let {Jbuild_types.Js_of_ocaml.javascript_files; flags} = js_of_ocaml in
  let javascript_files = List.map javascript_files ~f:(Path.relative dir) in
  let mk_target ext = Path.extend_basename src ~suffix:ext in
  let target = mk_target ".js" in
  let standalone_runtime = mk_target ".runtime.js" in
  if separate_compilation_enabled () then
    [ link_rule ~sctx ~dir ~runtime:standalone_runtime ~target
    ; standalone_runtime_rule ~sctx ~dir ~flags ~javascript_files ~target:standalone_runtime
    ]
  else
    [ exe_rule ~sctx ~dir ~flags ~javascript_files ~src ~target ]
