open Import
open Build.O

module SC = Super_context

module Program = struct
  type t =
    { name             : string
    ; main_module_name : Module.Name.t
    }
end

module Linkage = struct
  type t =
    { mode  : Mode.t
    ; ext   : string
    ; flags : string list
    }

  let byte =
    { mode  = Byte
    ; ext   = ".bc"
    ; flags = []
    }

  let native =
    { mode  = Native
    ; ext   = ".exe"
    ; flags = []
    }

  let custom =
    { mode  = Byte
    ; ext   = ".exe"
    ; flags = ["-custom"]
    }

  let native_or_custom (context : Context.t) =
    match context.ocamlopt with
    | None   -> custom
    | Some _ -> native

  let make ~mode ~ext ?(flags=[]) () =
    { mode
    ; ext
    ; flags
    }
end

let link_exe
      ~dir
      ~obj_dir
      ~scope
      ~requires
      ~name
      ~(linkage:Linkage.t)
      ~top_sorted_modules
      ?(flags=Ocaml_flags.empty)
      ?(link_flags=Build.arr (fun _ -> []))
      ?(js_of_ocaml=Jbuild.Js_of_ocaml.default)
      sctx
  =
  let ctx = SC.context sctx in
  let mode = linkage.mode in
  let exe = Path.relative dir (name ^ linkage.ext) in
  let compiler = Option.value_exn (Context.compiler ctx mode) in
  let artifacts ~ext modules =
    List.map modules ~f:(Module.obj_file ~obj_dir ~ext)
  in
  let modules_and_cm_files =
    Build.memoize "cm files"
      (top_sorted_modules >>^ fun modules ->
       (modules,
        artifacts modules ~ext:(Cm_kind.ext (Mode.cm_kind mode))))
  in
  let register_native_objs_deps build =
    match mode with
    | Byte -> build
    | Native ->
      build >>>
      Build.dyn_paths (Build.arr (fun (modules, _) ->
        artifacts modules ~ext:ctx.ext_obj))
  in
  SC.add_rule sctx
    (Build.fanout4
       requires
       (register_native_objs_deps modules_and_cm_files >>^ snd)
       (Ocaml_flags.get flags mode)
       link_flags
     >>>
     Build.dyn_paths (Build.arr (fun (libs, _, _, _) ->
       Lib.L.archive_files libs ~mode ~ext_lib:ctx.ext_lib))
     >>>
     Build.run ~context:ctx
       (Ok compiler)
       [ Dyn (fun (_, _, flags,_) -> As flags)
       ; A "-o"; Target exe
       ; As linkage.flags
       ; Dyn (fun (_, _, _, link_flags) -> As link_flags)
       ; Dyn (fun (libs, _, _, _) ->
           Lib.L.link_flags libs ~mode ~stdlib_dir:ctx.stdlib_dir)
       ; Dyn (fun (_, cm_files, _, _) -> Deps cm_files)
       ]);
  if mode = Mode.Byte then
    let rules = Js_of_ocaml_rules.build_exe sctx ~dir ~js_of_ocaml ~src:exe in
    let libs_and_cm_and_flags =
      (requires &&& (modules_and_cm_files >>^ snd))
      &&&
      SC.expand_and_eval_set sctx ~scope ~dir js_of_ocaml.flags
        ~standard:(Js_of_ocaml_rules.standard ())
    in
    SC.add_rules sctx (List.map rules ~f:(fun r -> libs_and_cm_and_flags >>> r))

let build_and_link_many
      ~dir ~obj_dir ~programs ~modules
      ~scope
      ~linkages
      ?(requires=Build.return [])
      ?already_used
      ?(flags=Ocaml_flags.empty)
      ?link_flags
      ?(js_of_ocaml=Jbuild.Js_of_ocaml.default)
      sctx
  =
  let modules =
    Module.Name.Map.map modules ~f:(Module.set_obj_name ~wrapper:None)
  in

  let dep_graphs =
    Ocamldep.rules sctx ~dir ~modules ?already_used
      ~alias_module:None ~lib_interface_module:None
  in

  (* CR-someday jdimino: this should probably say [~dynlink:false] *)
  Module_compilation.build_modules sctx
    ~js_of_ocaml
    ~dynlink:true ~flags ~scope ~dir ~obj_dir ~dep_graphs ~modules
    ~requires ~alias_module:None;

  List.iter programs ~f:(fun { Program.name; main_module_name } ->
    let top_sorted_modules =
      let main = Option.value_exn
                   (Module.Name.Map.find modules main_module_name) in
      Ocamldep.Dep_graph.top_closed_implementations dep_graphs.impl
        [main]
    in
    List.iter linkages ~f:(fun linkage ->
      link_exe sctx
        ~dir
        ~obj_dir
        ~scope
        ~requires
        ~name
        ~linkage
        ~top_sorted_modules
        ~js_of_ocaml
        ~flags
        ?link_flags))

let build_and_link ~dir ~obj_dir ~program =
  build_and_link_many ~dir ~obj_dir ~programs:[program]
