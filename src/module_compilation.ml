open Import
open Jbuild_types
open Build.O

module SC = Super_context

let lib_cm_all ~dir (lib : Library.t) cm_kind =
  Alias.file (Alias.lib_cm_all ~dir lib.name cm_kind)

let lib_dependencies (libs : Lib.t list) ~(cm_kind : Cm_kind.t) =
  List.concat_map libs ~f:(function
    | External _ -> []
    | Internal (dir, lib) ->
      match cm_kind with
      | Cmi | Cmo ->
        [lib_cm_all ~dir lib Cmi]
      | Cmx ->
        [lib_cm_all ~dir lib Cmx])

let build_cm sctx ?sandbox ~dynlink ~flags ~cm_kind ~(dep_graph:Ocamldep.dep_graph)
      ~requires ~(modules : Module.t String_map.t) ~dir ~alias_module (m : Module.t) =
  let ctx = SC.context sctx in
  Option.iter (Mode.of_cm_kind cm_kind |> Context.compiler ctx) ~f:(fun compiler ->
    Option.iter (Module.cm_source ~dir m cm_kind) ~f:(fun src ->
      let ml_kind = Cm_kind.source cm_kind in
      let dst = Module.cm_file m ~dir cm_kind in
      let extra_args, extra_deps, extra_targets =
        match cm_kind, m.intf with
        (* If there is no mli, [ocamlY -c file.ml] produces both the
           .cmY and .cmi. We choose to use ocamlc to produce the cmi
           and to produce the cmx we have to wait to avoid race
           conditions. *)
        | Cmo, None -> [], [], [Module.cm_file m ~dir Cmi]
        | Cmx, None ->
          (* Change [-intf-suffix] so that the compiler thinks the
             cmi exists and reads it instead of re-creating it, which
             could create a race condition. *)
          ([ "-intf-suffix"
           ; Filename.extension m.impl.name
           ],
           [Module.cm_file m ~dir Cmi], [])
        | Cmi, None -> assert false
        | Cmi, Some _ -> [], [], []
        (* We need the .cmi to build either the .cmo or .cmx *)
        | (Cmo | Cmx), Some _ -> [], [Module.cm_file m ~dir Cmi], []
      in
      let extra_targets =
        match cm_kind with
        | Cmx -> Path.relative dir (m.obj_name ^ ctx.ext_obj) :: extra_targets
        | Cmi | Cmo -> extra_targets
      in
      let dep_graph = Ml_kind.Dict.get dep_graph ml_kind in
      let other_cm_files =
        Build.dyn_paths
          (dep_graph >>^ (fun dep_graph ->
             let deps =
               List.map (Utils.find_deps ~dir dep_graph m.name)
                 ~f:(Utils.find_module ~dir modules)
             in
             List.concat_map
               deps
               ~f:(fun m ->
                 match cm_kind with
                 | Cmi | Cmo -> [Module.cm_file m ~dir Cmi]
                 | Cmx -> [Module.cm_file m ~dir Cmi; Module.cm_file m ~dir Cmx])))
      in
      let extra_targets, cmt_args =
        match cm_kind with
        | Cmx -> (extra_targets, Arg_spec.S [])
        | Cmi | Cmo ->
          let fn = Option.value_exn (Module.cmt_file m ~dir ml_kind) in
          (fn :: extra_targets, A "-bin-annot")
      in
      SC.add_rule sctx ?sandbox ~targets:(dst :: extra_targets)
        (Build.paths extra_deps >>>
         other_cm_files >>>
         requires >>>
         Build.dyn_paths (Build.arr (lib_dependencies ~cm_kind)) >>>
         Build.run ~context:ctx (Dep compiler)
           [ Ocaml_flags.get_for_cm flags ~cm_kind
           ; cmt_args
           ; Dyn Lib.include_flags
           ; As extra_args
           ; if dynlink || cm_kind <> Cmx then As [] else A "-nodynlink"
           ; A "-no-alias-deps"
           ; A "-I"; Path dir
           ; (match alias_module with
              | None -> S []
              | Some (m : Module.t) -> As ["-open"; m.name])
           ; A "-o"; Path dst
           ; A "-c"; Ml_kind.flag ml_kind; Dep src
           ])))

let build_module sctx ?sandbox ~dynlink ~js_of_ocaml ~flags m ~dir ~dep_graph ~modules ~requires
      ~alias_module =
  List.iter Cm_kind.all ~f:(fun cm_kind ->
    build_cm sctx ?sandbox ~dynlink ~flags ~dir ~dep_graph ~modules m ~cm_kind ~requires
      ~alias_module);
  (* Build *.cmo.js *)
  let src = Module.cm_file m ~dir Cm_kind.Cmo in
  Js_of_ocaml_rules.build_cm sctx ~dir ~js_of_ocaml ~src

let build_modules sctx ~dynlink ~js_of_ocaml ~flags ~dir ~dep_graph ~modules ~requires ~alias_module =
  String_map.iter
    (match alias_module with
     | None -> modules
     | Some (m : Module.t) -> String_map.remove m.name modules)
    ~f:(fun ~key:_ ~data:m ->
      build_module sctx m ~dynlink ~js_of_ocaml ~flags ~dir ~dep_graph ~modules ~requires
        ~alias_module)
