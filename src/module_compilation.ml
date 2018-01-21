open Import
open Build.O
open! No_io

module SC = Super_context

let build_cm sctx ?sandbox ~dynlink ~flags ~cm_kind ~dep_graphs
      ~requires ~dir ~obj_dir ~alias_module (m : Module.t) =
  let ctx = SC.context sctx in
  Option.iter (Mode.of_cm_kind cm_kind |> Context.compiler ctx) ~f:(fun compiler ->
    Option.iter (Module.cm_source ~dir m cm_kind) ~f:(fun src ->
      let ml_kind = Cm_kind.source cm_kind in
      let dst = Module.cm_file m ~obj_dir cm_kind in
      let extra_args, extra_deps, extra_targets, extra_symlinks =
        match cm_kind, m.intf with
        (* If there is no mli, [ocamlY -c file.ml] produces both the
           .cmY and .cmi. We choose to use ocamlc to produce the cmi
           and to produce the cmx we have to wait to avoid race
           conditions. *)
        | Cmo, None -> [], [], [Module.cm_file m ~obj_dir Cmi],
                       [Module.cm_file m ~obj_dir:dir Cmi]
        | Cmx, None ->
          (* Change [-intf-suffix] so that the compiler thinks the
             cmi exists and reads it instead of re-creating it, which
             could create a race condition. *)
          ([ "-intf-suffix"
           ; Filename.extension m.impl.name
           ],
           [Module.cm_file m ~obj_dir Cmi], [], [])
        | Cmi, None -> assert false
        | Cmi, Some _ -> [], [], [], []
        (* We need the .cmi to build either the .cmo or .cmx *)
        | (Cmo | Cmx), Some _ -> [], [Module.cm_file m ~obj_dir Cmi], [], []
      in
      let extra_targets, extra_symlinks =
        match cm_kind with
        | Cmx ->
          Path.change_extension ~ext:ctx.ext_obj (Module.cm_file m ~obj_dir Cmx) :: extra_targets,
          Path.change_extension ~ext:ctx.ext_obj (Module.cm_file m ~obj_dir:dir Cmx) :: extra_symlinks
        | Cmi | Cmo -> extra_targets, extra_symlinks
      in
      let dep_graph = Ml_kind.Dict.get dep_graphs ml_kind in
      let other_cm_files =
        Build.dyn_paths
          (Ocamldep.Dep_graph.deps_of dep_graph m >>^ fun deps ->
           List.concat_map deps
             ~f:(fun m ->
               match cm_kind with
               | Cmi | Cmo -> [Module.cm_file m ~obj_dir Cmi]
               | Cmx -> [ Module.cm_file m ~obj_dir Cmi
                        ; Module.cm_file m ~obj_dir Cmx
                        ]))
      in
      let extra_targets, extra_symlinks, cmt_args =
        match cm_kind with
        | Cmx -> (extra_targets, extra_symlinks, Arg_spec.S [])
        | Cmi | Cmo ->
          let fn = Option.value_exn (Module.cmt_file m ~obj_dir ml_kind) in
          let fn2 = Option.value_exn (Module.cmt_file m ~obj_dir:dir ml_kind) in
          (fn :: extra_targets, fn2 :: extra_symlinks, A "-bin-annot")
      in
      let old_dst = Module.cm_file m ~obj_dir:dir cm_kind in
      if obj_dir <> dir then begin
        SC.add_rule sctx ?sandbox (Build.symlink ~src:dst ~dst:old_dst) ;
        List.iter2 extra_targets extra_symlinks ~f:(fun src dst ->
          SC.add_rule sctx ?sandbox (Build.symlink ~src ~dst))
      end;
      SC.add_rule sctx ?sandbox
        (Build.paths extra_deps >>>
         other_cm_files >>>
         requires &&&
         Ocaml_flags.get_for_cm flags ~cm_kind >>>
         Build.run ~context:ctx (Ok compiler)
           ~extra_targets
           [ Dyn (fun (_, ocaml_flags) -> As ocaml_flags)
           ; cmt_args
           ; Dyn (fun (libs, _) -> Lib.include_flags libs ~stdlib_dir:ctx.stdlib_dir)
           ; As extra_args
           ; if dynlink || cm_kind <> Cmx then As [] else A "-nodynlink"
           ; A "-no-alias-deps"
           ; A "-I"; Path obj_dir
           ; (match alias_module with
              | None -> S []
              | Some (m : Module.t) -> As ["-open"; m.name])
           ; A "-o"; Target dst
           ; A "-c"; Ml_kind.flag ml_kind; Dep src
           ])))

let build_module sctx ?sandbox ~dynlink ~js_of_ocaml ~flags m ~scope ~dir
      ~obj_dir ~dep_graphs ~requires ~alias_module =
  List.iter Cm_kind.all ~f:(fun cm_kind ->
    let requires = Cm_kind.Dict.get requires cm_kind in
    build_cm sctx ?sandbox ~dynlink ~flags ~dir ~obj_dir ~dep_graphs m ~cm_kind
      ~requires ~alias_module);
  (* Build *.cmo.js *)
  let src = Module.cm_file m ~obj_dir Cm_kind.Cmo in
  let target = Path.extend_basename (Module.cm_file m ~obj_dir:dir Cm_kind.Cmo) ~suffix:".js" in
  SC.add_rules sctx (Js_of_ocaml_rules.build_cm sctx ~scope ~dir ~js_of_ocaml ~src ~target)

let build_modules sctx ~dynlink ~js_of_ocaml ~flags ~scope ~dir ~obj_dir
      ~dep_graphs ~modules ~requires ~alias_module =
  let cmi_requires =
    Build.memoize "cmi library dependencies"
      (requires
       >>>
       SC.Libs.file_deps sctx ~ext:".cmi")
  in
  let cmi_and_cmx_requires =
    Build.memoize "cmi and cmx library dependencies"
      (requires
       >>>
       SC.Libs.file_deps sctx ~ext:".cmi-and-.cmx")
  in
  let requires : _ Cm_kind.Dict.t =
    { cmi = cmi_requires
    ; cmo = cmi_requires
    ; cmx = cmi_and_cmx_requires
    }
  in
  String_map.iter
    (match alias_module with
     | None -> modules
     | Some (m : Module.t) -> String_map.remove m.name modules)
    ~f:(fun ~key:_ ~data:m ->
      build_module sctx m ~dynlink ~js_of_ocaml ~flags ~scope ~dir ~obj_dir
        ~dep_graphs ~requires ~alias_module)
