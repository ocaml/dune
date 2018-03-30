open Import
open! No_io
open Build.O

module SC = Super_context

let separate_compilation_enabled () = !Clflags.dev_mode

let pretty    () = if !Clflags.dev_mode then ["--pretty"           ] else []
let sourcemap () = if !Clflags.dev_mode then ["--source-map-inline"] else []

let standard () = pretty () @ sourcemap ()

let install_jsoo_hint = "opam install js_of_ocaml-compiler"

let in_build_dir ~ctx =
  let init = Path.relative ctx.Context.build_dir ".js" in
  List.fold_left ~init ~f:Path.relative

let runtime_file ~sctx ~dir fname =
  match
    Artifacts.file_of_lib (SC.artifacts sctx)
      ~loc:(Loc.in_file (Utils.jbuild_file_in ~dir |> Path.to_string))
      ~lib:"js_of_ocaml-compiler" ~file:fname
  with
  | Error _ ->
    Arg_spec.Dyn (fun _ ->
      Utils.library_not_found ~context:(SC.context sctx).name
        ~hint:install_jsoo_hint
        "js_of_ocaml-compiler")
  | Ok f -> Arg_spec.Dep f

let js_of_ocaml_rule ~sctx ~dir ~flags ~spec ~target =
  let jsoo = SC.resolve_program sctx ~hint:install_jsoo_hint "js_of_ocaml" in
  let runtime = runtime_file ~sctx ~dir "runtime.js" in
  Build.run ~context:(SC.context sctx) ~dir
    jsoo
    [ Arg_spec.Dyn flags
    ; Arg_spec.A "-o"; Target target
    ; Arg_spec.A "--no-runtime"; runtime
    ; spec
    ]

let standalone_runtime_rule ~sctx ~dir ~javascript_files ~target ~requires =
  let spec =
    Arg_spec.S
      [ Arg_spec.of_result_map requires ~f:(fun libs ->
          Arg_spec.Deps (Lib.L.jsoo_runtime_files libs))
      ; Arg_spec.Deps javascript_files
      ]
  in
  Build.arr (fun (cm_files, flags) -> (cm_files, "--runtime-only" :: flags))
  >>>
  js_of_ocaml_rule ~sctx ~dir ~flags:(fun (_,flags) -> As flags) ~target ~spec

let exe_rule ~sctx ~dir ~javascript_files ~src ~target ~requires =
  let spec =
    Arg_spec.S
      [ Arg_spec.of_result_map requires ~f:(fun libs ->
          Arg_spec.Deps (Lib.L.jsoo_runtime_files libs))
      ; Arg_spec.Deps javascript_files
      ; Arg_spec.Dep src
      ]
  in
  js_of_ocaml_rule ~sctx ~dir ~flags:(fun (_,flags) -> As flags) ~spec ~target

let jsoo_archives lib =
  List.map (Lib.archives lib).byte ~f:(Path.extend_basename ~suffix:".js")

let link_rule ~sctx ~dir ~runtime ~target ~requires =
  let ctx = SC.context sctx in
  let get_all (cm, _) =
    Arg_spec.of_result_map requires ~f:(fun libs ->
      let all_libs =
        List.concat_map libs ~f:(fun (lib : Lib.t) ->
          let jsoo_archives = jsoo_archives lib in
          if Lib.is_local lib then (
            jsoo_archives
          ) else (
            let lib_name = Lib.name lib in
            List.map ~f:(fun js ->
              in_build_dir ~ctx [lib_name ; Path.basename js]) jsoo_archives
          )
        )
      in
      (* Special case for the stdlib because it is not referenced in the META *)
      let all_libs = in_build_dir ~ctx ["stdlib"; "stdlib.cma.js"] :: all_libs in
      let all_other_modules =
        List.map cm ~f:(fun m -> Path.extend_basename m ~suffix:".js")
      in
      Arg_spec.Deps (List.concat [all_libs;all_other_modules]))
  in
  let jsoo_link = SC.resolve_program sctx ~hint:install_jsoo_hint "jsoo_link" in
  Build.run ~context:(SC.context sctx) ~dir
    jsoo_link
    [ Arg_spec.A "-o"; Target target
    ; Arg_spec.Dep runtime
    ; Arg_spec.As (sourcemap ())
    ; Arg_spec.Dyn get_all
    ]

let build_cm sctx ~scope ~dir ~(js_of_ocaml:Jbuild.Js_of_ocaml.t) ~src ~target =
  if separate_compilation_enabled ()
  then
    let itarget = Path.extend_basename src ~suffix:".js" in
    let spec = Arg_spec.Dep src in
    let flags =
      SC.expand_and_eval_set sctx ~scope ~dir js_of_ocaml.flags
        ~standard:(standard ())
    in
    [ flags
      >>>
      js_of_ocaml_rule ~sctx ~dir ~flags:(fun flags ->
        As flags) ~spec ~target:itarget ]
    @ (if target = itarget then
         []
       else
         [Build.symlink ~src:itarget ~dst:target])
  else []

let setup_separate_compilation_rules sctx components =
  if separate_compilation_enabled ()
  then
    match components with
    | [] | _ :: _ :: _ -> ()
    | [pkg] ->
      let ctx = SC.context sctx in
      match Lib.DB.find (SC.installed_libs sctx) pkg with
      | Error _ -> ()
      | Ok pkg ->
        let archives = (Lib.archives pkg).byte in
        let archives =
          (* Special case for the stdlib because it is not referenced
             in the META *)
          match Lib.name pkg with
          | "stdlib" -> Path.relative ctx.stdlib_dir "stdlib.cma" :: archives
          | _ -> archives
        in
        List.iter archives ~f:(fun fn ->
          let name = Path.basename fn in
          let src = Path.relative (Lib.src_dir pkg) name in
          let target =
            in_build_dir ~ctx [ Lib.name pkg; sprintf "%s.js" name]
          in
          let dir = in_build_dir ~ctx [ Lib.name pkg ] in
          let spec = Arg_spec.Dep src in
          SC.add_rule sctx
            (Build.return (standard ())
             >>>
             js_of_ocaml_rule ~sctx ~dir ~flags:(fun flags ->
               As flags) ~spec ~target)
        )

let build_exe sctx ~dir ~js_of_ocaml ~src ~requires =
  let {Jbuild.Js_of_ocaml.javascript_files; _} = js_of_ocaml in
  let javascript_files = List.map javascript_files ~f:(Path.relative dir) in
  let mk_target ext = Path.extend_basename src ~suffix:ext in
  let target = mk_target ".js" in
  let standalone_runtime = mk_target ".runtime.js" in
  if separate_compilation_enabled () then
    [ link_rule ~sctx ~dir ~runtime:standalone_runtime ~target ~requires
    ; standalone_runtime_rule ~sctx ~dir ~javascript_files
        ~target:standalone_runtime ~requires
    ]
  else
    [ exe_rule ~sctx ~dir ~javascript_files ~src ~target ~requires ]
