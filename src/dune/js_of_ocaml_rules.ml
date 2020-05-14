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

type sub_command =
  | Compile
  | Link
  | Build_runtime

let js_of_ocaml_rule sctx ~sub_command ~dir ~flags ~spec ~target =
  let jsoo = jsoo ~dir sctx in
  Command.run ~dir:(Path.build dir) jsoo
    [ ( match sub_command with
      | Compile -> S []
      | Link -> A "link"
      | Build_runtime -> A "build-runtime" )
    ; flags
    ; A "-o"
    ; Target target
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
  js_of_ocaml_rule
    (Compilation_context.super_context cc)
    ~sub_command:Build_runtime
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
  js_of_ocaml_rule sctx ~sub_command:Compile ~dir ~spec ~target ~flags

let jsoo_archives ~ctx lib =
  let info = Lib.info lib in
  let jsoo_archive = Lib_info.jsoo_archive info in
  match jsoo_archive with
  | Some a -> [ a ]
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
    Build.map cm ~f:(fun cm ->
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
  let spec = Command.Args.S [ Dep (Path.build runtime); Dyn get_all ] in
  let flags = Command.Args.As (sourcemap sctx) in
  js_of_ocaml_rule sctx ~sub_command:Link ~dir ~spec ~target ~flags

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
    [ js_of_ocaml_rule sctx ~sub_command:Compile ~dir
        ~flags:(Command.Args.dyn flags) ~spec ~target
    ]
  else
    []

let setup_separate_compilation_rules sctx components =
  if separate_compilation_enabled sctx then
    match components with
    | []
    | _ :: _ :: _ ->
      ()
    | [ pkg ] -> (
      let pkg = Lib_name.parse_string_exn (Loc.none, pkg) in
      let ctx = SC.context sctx in
      match Lib.DB.find (SC.installed_libs sctx) pkg with
      | None -> ()
      | Some pkg ->
        let info = Lib.info pkg in
        let archives = (Lib_info.archives info).byte in
        let archives =
          (* Special case for the stdlib because it is not referenced in the
             META *)
          match Lib_name.to_string (Lib.name pkg) with
          | "stdlib" -> Path.relative ctx.stdlib_dir "stdlib.cma" :: archives
          | _ -> archives
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
              (js_of_ocaml_rule sctx ~sub_command:Compile ~dir
                 ~flags:(As (standard sctx))
                 ~spec ~target)) )

let build_exe cc ~js_of_ocaml ~src ~(cm : Path.t list Build.t) ~flags ~promote =
  let { Dune_file.Js_of_ocaml.javascript_files; _ } = js_of_ocaml in
  let dir = Compilation_context.dir cc in
  let sctx = Compilation_context.super_context cc in
  let javascript_files =
    List.map javascript_files ~f:(Path.relative (Path.build dir))
  in
  let mk_target ext = Path.Build.extend_basename src ~suffix:ext in
  let target = mk_target ".js" in
  let standalone_runtime = mk_target ".runtime.js" in
  let mode : Rule.Mode.t =
    match promote with
    | None -> Standard
    | Some p -> Promote p
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
