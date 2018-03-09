open Import
open Build.O
open! No_io

module SC = Super_context

module Target : sig
  type t
  val cm : Module.t -> Cm_kind.t -> t
  val obj : Module.t -> ext:string -> t
  val cmt : Module.t -> Ml_kind.t -> t option
  val file : Path.t -> t -> Path.t
end = struct
  type t = Path.t
  let cm m cm_kind = Module.cm_file_unsafe m ~obj_dir:Path.root cm_kind
  let obj m ~ext = Module.obj_file m ~obj_dir:Path.root ~ext
  let cmt m ml_kind = Module.cmt_file m ~obj_dir:Path.root ml_kind
  let file dir t = Path.append dir t
end

let build_cm sctx ?sandbox ~dynlink ~flags ~cm_kind ~dep_graphs
      ~requires ~dir ~obj_dir ~alias_module (m : Module.t) =
  let ctx = SC.context sctx in
  Option.iter (Mode.of_cm_kind cm_kind |> Context.compiler ctx) ~f:(fun compiler ->
    Option.iter (Module.cm_source ~dir m cm_kind) ~f:(fun src ->
      let ml_kind = Cm_kind.source cm_kind in
      let dst = Module.cm_file_unsafe m ~obj_dir cm_kind in
      let extra_args, extra_deps, other_targets =
        match cm_kind, m.intf with
        (* If there is no mli, [ocamlY -c file.ml] produces both the
           .cmY and .cmi. We choose to use ocamlc to produce the cmi
           and to produce the cmx we have to wait to avoid race
           conditions. *)
        | Cmo, None -> [], [], [Target.cm m Cmi]
        | Cmx, None ->
          (* Change [-intf-suffix] so that the compiler thinks the
             cmi exists and reads it instead of re-creating it, which
             could create a race condition. *)
          [ "-intf-suffix"
          ; Filename.extension (Option.value_exn m.impl).name
          ],
          [Module.cm_file_unsafe m ~obj_dir Cmi],
          []
        | Cmi, None -> assert false
        | Cmi, Some _ -> [], [], []
        (* We need the .cmi to build either the .cmo or .cmx *)
        | (Cmo | Cmx), Some _ -> [], [Module.cm_file_unsafe m ~obj_dir Cmi], []
      in
      let other_targets =
        match cm_kind with
        | Cmx -> Target.obj m ~ext:ctx.ext_obj :: other_targets
        | Cmi | Cmo -> other_targets
      in
      let dep_graph = Ml_kind.Dict.get dep_graphs ml_kind in
      let other_cm_files =
        Build.dyn_paths
          (Ocamldep.Dep_graph.deps_of dep_graph m >>^ fun deps ->
           List.concat_map deps
             ~f:(fun m ->
               let deps = [Module.cm_file_unsafe m ~obj_dir Cmi] in
               if Module.has_impl m && cm_kind = Cmx then
                 Module.cm_file_unsafe m ~obj_dir Cmx :: deps
               else
                 deps))
      in
      let other_targets, cmt_args =
        match cm_kind with
        | Cmx -> (other_targets, Arg_spec.S [])
        | Cmi | Cmo ->
          let fn = Option.value_exn (Target.cmt m ml_kind) in
          (fn :: other_targets, A "-bin-annot")
      in
      let extra_targets = List.map other_targets ~f:(Target.file obj_dir) in
      if obj_dir <> dir then begin
        (* Symlink the object files in the original directory for
           backward compatibility *)
        let old_dst = Module.cm_file_unsafe m ~obj_dir:dir cm_kind in
        SC.add_rule sctx (Build.symlink ~src:dst ~dst:old_dst) ;
        List.iter2 extra_targets other_targets ~f:(fun in_obj_dir target ->
          let in_dir = Target.file dir target in
          SC.add_rule sctx (Build.symlink ~src:in_obj_dir ~dst:in_dir))
      end;
      let opaque =
        if cm_kind = Cmi && not (Module.has_impl m) && ctx.version >= (4, 03, 0) then
          Arg_spec.A "-opaque"
        else
          As []
      in
      SC.add_rule sctx ?sandbox
        (Build.paths extra_deps >>>
         other_cm_files >>>
         requires &&&
         Ocaml_flags.get_for_cm flags ~cm_kind >>>
         Build.run ~context:ctx (Ok compiler)
           ~extra_targets
           [ Dyn (fun (_, ocaml_flags) -> As ocaml_flags)
           ; cmt_args
           ; A "-I"; Path obj_dir
           ; Dyn (fun (libs, _) ->
               Lib.L.include_flags libs ~stdlib_dir:ctx.stdlib_dir)
           ; As extra_args
           ; if dynlink || cm_kind <> Cmx then As [] else A "-nodynlink"
           ; A "-no-alias-deps"; opaque
           ; (match alias_module with
              | None -> S []
              | Some (m : Module.t) ->
                As ["-open"; Module.Name.to_string m.name])
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
  let src = Module.cm_file_unsafe m ~obj_dir Cm_kind.Cmo in
  let target =
    Path.extend_basename (Module.cm_file_unsafe m ~obj_dir:dir Cm_kind.Cmo)
      ~suffix:".js"
  in
  SC.add_rules sctx
    (Js_of_ocaml_rules.build_cm sctx ~scope ~dir ~js_of_ocaml ~src ~target)

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
  Module.Name.Map.iter
    (match alias_module with
     | None -> modules
     | Some (m : Module.t) -> Module.Name.Map.remove modules m.name)
    ~f:(fun m ->
      build_module sctx m ~dynlink ~js_of_ocaml ~flags ~scope ~dir ~obj_dir
        ~dep_graphs ~requires ~alias_module)
