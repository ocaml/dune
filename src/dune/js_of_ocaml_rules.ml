open! Stdune
open Import
open! No_io
module SC = Super_context

let dev_mode sctx = Profile.is_dev (SC.profile sctx)

let separate_compilation_enabled = dev_mode

let pretty sctx =
  if dev_mode sctx then
    [ "--pretty" ]
  else
    []

let sourcemap sctx =
  if dev_mode sctx then
    [ "--source-map-inline" ]
  else
    []

let standard sctx = pretty sctx @ sourcemap sctx

let install_jsoo_hint = "try: opam install js_of_ocaml-compiler"

let in_build_dir ~ctx args =
  Path.L.relative (Path.build ctx.Context.build_dir) (".js" :: args)

let jsoo ~dir sctx =
  SC.resolve_program sctx ~dir ~loc:None ~hint:install_jsoo_hint "js_of_ocaml"

let jsoo_link ~dir sctx =
  SC.resolve_program sctx ~dir ~loc:None ~hint:install_jsoo_hint "jsoo_link"

let runtime_file ~dir ~sctx file =
  match
    Artifacts.Public_libs.file_of_lib (SC.artifacts sctx).public_libs
      ~loc:Loc.none
      ~lib:(Lib_name.of_string_exn ~loc:None "js_of_ocaml-compiler")
      ~file
  with
  | Error _ -> (
    let fail =
      let fail () =
        Utils.library_not_found ~context:(SC.context sctx).name
          ~hint:install_jsoo_hint "js_of_ocaml-compiler"
      in
      Build.fail { fail }
    in
    match jsoo ~dir sctx with
    | Ok path ->
      let path = Path.relative (Path.parent_exn path) file in
      Build.if_file_exists path ~then_:(Build.arr (fun _ -> path)) ~else_:fail
    | _ ->
      fail )
  | Ok f ->
    Build.arr (fun _ -> f)

let js_of_ocaml_rule sctx ~dir ~flags ~spec ~target =
  let jsoo = jsoo ~dir sctx in
  let runtime_dep = runtime_file ~dir ~sctx "runtime.js" in
  Command.run ~dir:(Path.build dir) jsoo
    [ flags
    ; A "-o"
    ; Target target
    ; A "--no-runtime"
    ; Dyn (Build.S.map runtime_dep ~f:(fun x -> Command.Args.Dep x))
    ; spec
    ]

let standalone_runtime_rule cc ~javascript_files ~target ~flags =
  let spec =
    Command.Args.S
      [ Command.of_result_map (Compilation_context.requires_link cc)
          ~f:(fun libs -> Deps (Lib.L.jsoo_runtime_files libs))
      ; Deps javascript_files
      ]
  in
  let flags = Command.Args.S [ A "--runtime-only"; flags ] in
  js_of_ocaml_rule
    (Compilation_context.super_context cc)
    ~dir:(Compilation_context.dir cc)
    ~flags ~target ~spec

let exe_rule cc ~javascript_files ~src ~target ~flags =
  let dir = Compilation_context.dir cc in
  let sctx = Compilation_context.super_context cc in
  let spec =
    Command.Args.S
      [ Command.of_result_map (Compilation_context.requires_link cc)
          ~f:(fun libs -> Deps (Lib.L.jsoo_runtime_files libs))
      ; Deps javascript_files
      ; Dep (Path.build src)
      ]
  in
  js_of_ocaml_rule sctx ~dir ~spec ~target ~flags

let jsoo_archives ~ctx lib =
  let info = Lib.info lib in
  let jsoo_archive = Lib_info.jsoo_archive info in
  match jsoo_archive with
  | Some a ->
    [ a ]
  | None ->
    let archives = Lib_info.archives info in
    List.map archives.byte ~f:(fun archive ->
        in_build_dir ~ctx
          [ Lib_name.to_string (Lib.name lib); Path.basename archive ^ ".js" ])

let link_rule cc ~runtime ~target cm =
  let sctx = Compilation_context.super_context cc in
  let ctx = Compilation_context.context cc in
  let dir = Compilation_context.dir cc in
  let requires = Compilation_context.requires_link cc in
  let get_all =
    Build.S.map cm ~f:(fun cm ->
        Command.of_result_map requires ~f:(fun libs ->
            let all_libs = List.concat_map libs ~f:(jsoo_archives ~ctx) in
            (* Special case for the stdlib because it is not referenced in the
               META *)
            let all_libs =
              in_build_dir ~ctx [ "stdlib"; "stdlib.cma.js" ] :: all_libs
            in
            let all_other_modules =
              List.map cm ~f:(fun m -> Path.extend_basename m ~suffix:".js")
            in
            Deps (List.concat [ all_libs; all_other_modules ])))
  in
  let jsoo_link = jsoo_link ~dir sctx in
  Command.run ~dir:(Path.build dir) jsoo_link
    [ A "-o"
    ; Target target
    ; Dep (Path.build runtime)
    ; As (sourcemap sctx)
    ; Dyn get_all
    ]

let build_cm cctx ~(js_of_ocaml : Dune_file.Js_of_ocaml.t) ~src ~target =
  let sctx = Compilation_context.super_context cctx in
  let dir = Compilation_context.dir cctx in
  let expander = Compilation_context.expander cctx in
  if separate_compilation_enabled sctx then
    let spec = Command.Args.Dep (Path.build src) in
    let flags =
      Expander.expand_and_eval_set expander js_of_ocaml.flags
        ~standard:(Build.return (standard sctx))
    in
    [ js_of_ocaml_rule sctx ~dir ~flags:(Command.Args.dyn flags) ~spec ~target ]
  else
    []

let setup_separate_compilation_rules sctx components =
  if separate_compilation_enabled sctx then
    match components with
    | [] | _ :: _ :: _ ->
      ()
    | [ pkg ] -> (
      let pkg = Lib_name.of_string_exn ~loc:None pkg in
      let ctx = SC.context sctx in
      match Lib.DB.find (SC.installed_libs sctx) pkg with
      | None ->
        ()
      | Some pkg ->
        let info = Lib.info pkg in
        let archives = (Lib_info.archives info).byte in
        let archives =
          (* Special case for the stdlib because it is not referenced in the
             META *)
          match Lib_name.to_string (Lib.name pkg) with
          | "stdlib" ->
            Path.relative ctx.stdlib_dir "stdlib.cma" :: archives
          | _ ->
            archives
        in
        List.iter archives ~f:(fun fn ->
            let name = Path.basename fn in
            let src_dir = Lib_info.src_dir info in
            let src = Path.relative src_dir name in
            let lib_name = Lib_name.to_string (Lib.name pkg) in
            let target =
              in_build_dir ~ctx [ lib_name; sprintf "%s.js" name ]
              |> Path.as_in_build_dir_exn
            in
            let dir =
              Path.as_in_build_dir_exn (in_build_dir ~ctx [ lib_name ])
            in
            let spec = Command.Args.Dep src in
            SC.add_rule sctx ~dir
              (js_of_ocaml_rule sctx ~dir
                 ~flags:(As (standard sctx))
                 ~spec ~target)) )

let build_exe cc ~js_of_ocaml ~src ~(cm : Path.t list Build.s) ~flags ~promote
    =
  let { Dune_file.Js_of_ocaml.javascript_files; _ } = js_of_ocaml in
  let dir = Compilation_context.dir cc in
  let sctx = Compilation_context.super_context cc in
  let javascript_files =
    List.map javascript_files ~f:(Path.relative (Path.build dir))
  in
  let mk_target ext = Path.Build.extend_basename src ~suffix:ext in
  let target = mk_target ".js" in
  let standalone_runtime = mk_target ".runtime.js" in
  let mode : Dune_file.Rule.Mode.t =
    match promote with None -> Standard | Some p -> Promote p
  in
  if separate_compilation_enabled sctx then (
    SC.add_rule sctx ~dir
      (standalone_runtime_rule cc ~javascript_files ~target:standalone_runtime
         ~flags);
    SC.add_rule sctx ~dir ~mode
      (link_rule cc ~runtime:standalone_runtime ~target cm)
  ) else
    SC.add_rule sctx ~dir ~mode
      (exe_rule cc ~javascript_files ~src ~target ~flags)
