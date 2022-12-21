open Import

let install_jsoo_hint = "opam install js_of_ocaml-compiler"

let in_build_dir ~ctx args =
  Path.Build.L.relative ctx.Context.build_dir (".js" :: args)

let jsoo ~dir sctx =
  Super_context.resolve_program sctx ~dir ~loc:None ~hint:install_jsoo_hint
    "js_of_ocaml"

type sub_command =
  | Compile
  | Link
  | Build_runtime

let js_of_ocaml_rule sctx ~sub_command ~dir ~(flags : _ Js_of_ocaml.Flags.t)
    ~spec ~target =
  let open Memo.O in
  let+ jsoo = jsoo ~dir sctx
  and+ flags = Super_context.js_of_ocaml_flags sctx ~dir flags in
  Command.run ~dir:(Path.build dir) jsoo
    [ (match sub_command with
      | Compile -> S []
      | Link -> A "link"
      | Build_runtime -> A "build-runtime")
    ; Command.Args.dyn
        (match sub_command with
        | Compile -> flags.compile
        | Link -> flags.link
        | Build_runtime -> flags.build_runtime)
    ; A "-o"
    ; Target target
    ; spec
    ]

let jsoo_runtime_files =
  List.concat_map ~f:(fun t -> Lib_info.jsoo_runtime (Lib.info t))

let standalone_runtime_rule cc ~javascript_files ~target ~flags =
  let libs = Compilation_context.requires_link cc in
  let spec =
    Command.Args.S
      [ Resolve.Memo.args
          (let open Resolve.Memo.O in
          let+ libs = libs in
          Command.Args.Deps (jsoo_runtime_files libs))
      ; Deps (List.map ~f:Path.build javascript_files)
      ]
  in
  let dir = Compilation_context.dir cc in
  js_of_ocaml_rule
    (Compilation_context.super_context cc)
    ~sub_command:Build_runtime ~dir ~flags ~target ~spec

let exe_rule cc ~javascript_files ~src ~target ~flags =
  let dir = Compilation_context.dir cc in
  let sctx = Compilation_context.super_context cc in
  let libs = Compilation_context.requires_link cc in
  let spec =
    Command.Args.S
      [ Resolve.Memo.args
          (let open Resolve.Memo.O in
          let+ libs = libs in
          Command.Args.Deps (jsoo_runtime_files libs))
      ; Deps (List.map ~f:Path.build javascript_files)
      ; Dep (Path.build src)
      ]
  in
  js_of_ocaml_rule sctx ~sub_command:Compile ~dir ~spec ~target ~flags

let with_js_ext s =
  match Filename.split_extension s with
  | name, ".cma" -> name ^ Js_of_ocaml.Ext.cma
  | name, ".cmo" -> name ^ Js_of_ocaml.Ext.cmo
  | _ -> assert false

let jsoo_archives ~ctx lib =
  let info = Lib.info lib in
  let jsoo_archive = Lib_info.jsoo_archive info in
  match jsoo_archive with
  | Some a -> [ a ]
  | None ->
    let archives = Lib_info.archives info in
    List.map archives.byte ~f:(fun archive ->
        Path.build
          (in_build_dir ~ctx
             [ Lib_name.to_string (Lib.name lib)
             ; with_js_ext (Path.basename archive)
             ]))

let link_rule cc ~runtime ~target cm ~flags ~link_time_code_gen =
  let open Memo.O in
  let sctx = Compilation_context.super_context cc in
  let ctx = Compilation_context.context cc in
  let dir = Compilation_context.dir cc in
  let requires = Compilation_context.requires_link cc in
  let special_units =
    Action_builder.of_memo
      (let+ pre = link_time_code_gen in
       List.concat_map pre ~f:(function
         | `Mod path -> [ Path.set_extension ~ext:Js_of_ocaml.Ext.cmo path ]
         | `Lib _ -> []))
  in
  let get_all =
    Action_builder.map (Action_builder.both cm special_units)
      ~f:(fun (cm, special_units) ->
        Resolve.Memo.args
          (let open Resolve.Memo.O in
          let+ libs = requires in
          let all_libs = List.concat_map libs ~f:(jsoo_archives ~ctx) in
          (* Special case for the stdlib because it is not referenced in the
             META *)
          let stdlib =
            Path.build
              (in_build_dir ~ctx [ "stdlib"; "stdlib" ^ Js_of_ocaml.Ext.cma ])
          in
          let all_other_modules =
            List.map cm ~f:(Path.set_extension ~ext:Js_of_ocaml.Ext.cmo)
          in
          Command.Args.Deps
            (List.concat
               [ [ stdlib ]; special_units; all_libs; all_other_modules ])))
  in
  let spec =
    let std_exit =
      Path.build
        (in_build_dir ~ctx [ "stdlib"; "std_exit" ^ Js_of_ocaml.Ext.cmo ])
    in
    Command.Args.S [ Dep (Path.build runtime); Dyn get_all; Dep std_exit ]
  in
  js_of_ocaml_rule sctx ~sub_command:Link ~dir ~spec ~target ~flags

let build_cm cc ~in_context ~src ~target =
  let sctx = Compilation_context.super_context cc in
  let dir = Compilation_context.dir cc in
  let spec = Command.Args.Dep (Path.build src) in
  let flags = in_context.Js_of_ocaml.In_context.flags in
  js_of_ocaml_rule sctx ~sub_command:Compile ~dir ~flags ~spec ~target

let setup_separate_compilation_rules sctx components =
  match components with
  | [] | _ :: _ :: _ -> Memo.return ()
  | [ pkg ] -> (
    let pkg = Lib_name.parse_string_exn (Loc.none, pkg) in
    let ctx = Super_context.context sctx in
    let open Memo.O in
    let* installed_libs = Lib.DB.installed ctx in
    Lib.DB.find installed_libs pkg >>= function
    | None -> Memo.return ()
    | Some pkg ->
      let info = Lib.info pkg in
      let lib_name = Lib_name.to_string (Lib.name pkg) in
      let archives =
        let archives = (Lib_info.archives info).byte in
        (* Special case for the stdlib because it is not referenced in the
           META *)
        match lib_name with
        | "stdlib" ->
          let archive =
            let stdlib_dir = (Lib.lib_config pkg).stdlib_dir in
            Path.relative stdlib_dir
          in
          archive "stdlib.cma" :: archive "std_exit.cmo" :: archives
        | _ -> archives
      in

      Memo.parallel_iter archives ~f:(fun fn ->
          let name = Path.basename fn in
          let target = in_build_dir ~ctx [ lib_name; with_js_ext name ] in
          let spec =
            let src_dir = Lib_info.src_dir info in
            let src = Path.relative src_dir name in
            Command.Args.Dep src
          in
          let dir = in_build_dir ~ctx [ lib_name ] in
          let open Memo.O in
          let* action_with_targets =
            js_of_ocaml_rule sctx ~sub_command:Compile ~dir
              ~flags:Js_of_ocaml.Flags.standard ~spec ~target
          in
          Super_context.add_rule sctx ~dir action_with_targets))

let build_exe cc ~loc ~in_context ~src ~(cm : Path.t list Action_builder.t)
    ~promote ~link_time_code_gen =
  let { Js_of_ocaml.In_context.javascript_files; flags } = in_context in
  let dir = Compilation_context.dir cc in
  let sctx = Compilation_context.super_context cc in
  let mk_target ext = Path.Build.set_extension src ~ext in
  let target = mk_target Js_of_ocaml.Ext.exe in
  let standalone_runtime = mk_target Js_of_ocaml.Ext.runtime in
  let mode : Rule.Mode.t =
    match promote with
    | None -> Standard
    | Some p -> Promote p
  in
  let open Memo.O in
  let* cmode = Super_context.js_of_ocaml_compilation_mode sctx ~dir in
  match (cmode : Js_of_ocaml.Compilation_mode.t) with
  | Separate_compilation ->
    standalone_runtime_rule cc ~javascript_files ~target:standalone_runtime
      ~flags
    >>= Super_context.add_rule ~loc sctx ~dir
    >>> link_rule cc ~runtime:standalone_runtime ~target cm ~flags
          ~link_time_code_gen
    >>= Super_context.add_rule sctx ~loc ~dir ~mode
  | Whole_program ->
    exe_rule cc ~javascript_files ~src ~target ~flags
    >>= Super_context.add_rule sctx ~loc ~dir ~mode

let runner = "node"
