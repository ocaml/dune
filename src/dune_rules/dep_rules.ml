open Import
open Memo.O
module Parallel_map = Memo.Make_parallel_map (Module_name.Unique.Map)

let transitive_deps_contents modules =
  List.map modules ~f:(fun m ->
    (* TODO use object names *)
    Modules.Sourced_module.to_module m |> Module.name |> Module_name.to_string)
  |> String.concat ~sep:"\n"
;;

let ooi_deps
      ~vimpl
      ~sctx
      ~dir
      ~obj_dir
      ~dune_version
      ~vlib_obj_map
      ~(ml_kind : Ml_kind.t)
      (sourced_module : Modules.Sourced_module.t)
  =
  let m = Modules.Sourced_module.to_module sourced_module in
  let* read =
    let unit =
      let cm_kind =
        match ml_kind with
        | Intf -> Cm_kind.Cmi
        | Impl -> vimpl |> Vimpl.impl_cm_kind
      in
      Obj_dir.Module.cm_file_exn obj_dir m ~kind:(Ocaml cm_kind) |> Path.build
    in
    let sandbox =
      if dune_version >= (3, 3) then Some Sandbox_config.needs_sandboxing else None
    in
    let+ ocaml =
      let ctx = Super_context.context sctx in
      Context.ocaml ctx
    in
    Ocamlobjinfo.rules ocaml ~sandbox ~dir ~unit
  in
  let add_rule = Super_context.add_rule sctx ~dir in
  let read =
    Action_builder.memoize
      "ocamlobjinfo"
      (let open Action_builder.O in
       let+ (ooi : Ocamlobjinfo.t) = read in
       Module_name.Unique.Set.to_list ooi.intf
       |> List.filter_map ~f:(fun dep ->
         if Module.obj_name m = dep
         then None
         else Module_name.Unique.Map.find vlib_obj_map dep))
  in
  let+ () =
    add_rule
      (let target =
         Obj_dir.Module.dep obj_dir (Transitive (m, ml_kind)) |> Option.value_exn
       in
       Action_builder.map read ~f:transitive_deps_contents
       |> Action_builder.write_file_dyn target)
  in
  read
;;

let deps_of_module ~modules ~sandbox ~sctx ~dir ~obj_dir ~ml_kind m =
  match Module.kind m with
  | Wrapped_compat ->
    let interface_module =
      match Modules.With_vlib.lib_interface modules with
      | Some m -> m
      | None ->
        let modules = Modules.With_vlib.drop_vlib modules in
        Modules.compat_for_exn modules m
    in
    List.singleton interface_module |> Action_builder.return |> Memo.return
  | _ ->
    let+ deps = Ocamldep.deps_of ~sandbox ~modules ~sctx ~dir ~obj_dir ~ml_kind m in
    (match Modules.With_vlib.alias_for modules m with
     | [] -> deps
     | aliases ->
       let open Action_builder.O in
       let+ deps = deps in
       aliases @ deps)
;;

let deps_of_vlib_module ~obj_dir ~vimpl ~dir ~sctx ~ml_kind sourced_module =
  match
    let vlib = Vimpl.vlib vimpl in
    Lib.Local.of_lib vlib
  with
  | None ->
    let+ deps =
      let vlib_obj_map = Vimpl.vlib_obj_map vimpl in
      let dune_version =
        let impl = Vimpl.impl vimpl in
        Dune_project.dune_version impl.project
      in
      ooi_deps
        ~vimpl
        ~sctx
        ~dir
        ~obj_dir
        ~dune_version
        ~vlib_obj_map
        ~ml_kind
        sourced_module
    in
    Action_builder.map deps ~f:(List.map ~f:Modules.Sourced_module.to_module)
  | Some lib ->
    let vlib_obj_dir =
      let info = Lib.Local.info lib in
      Lib_info.obj_dir info
    in
    let m = Modules.Sourced_module.to_module sourced_module in
    let+ () =
      let src =
        Obj_dir.Module.dep vlib_obj_dir (Transitive (m, ml_kind))
        |> Option.value_exn
        |> Path.build
      in
      let dst =
        Obj_dir.Module.dep obj_dir (Transitive (m, ml_kind)) |> Option.value_exn
      in
      Super_context.add_rule sctx ~dir (Action_builder.symlink ~src ~dst)
    in
    let modules = Vimpl.vlib_modules vimpl |> Modules.With_vlib.modules in
    Ocamldep.read_deps_of ~obj_dir:vlib_obj_dir ~modules ~ml_kind m
;;

(** Tests whether a set of modules is a singleton *)
let has_single_file modules = Option.is_some @@ Modules.With_vlib.as_singleton modules

let rec deps_of
          ~obj_dir
          ~modules
          ~sandbox
          ~impl
          ~dir
          ~sctx
          ~ml_kind
          (m : Modules.Sourced_module.t)
  =
  let is_alias_or_root =
    match m with
    | Impl_of_virtual_module _ -> false
    | Imported_from_vlib m | Normal m ->
      (match Module.kind m with
       | Root | Alias _ -> true
       | _ -> false)
  in
  if is_alias_or_root || has_single_file modules
  then Memo.return (Action_builder.return [])
  else (
    let skip_if_source_absent f sourced_module =
      let m = Modules.Sourced_module.to_module m in
      if Module.has m ~ml_kind
      then f sourced_module
      else Memo.return (Action_builder.return [])
    in
    match m with
    | Imported_from_vlib _ ->
      let vimpl = Virtual_rules.vimpl_exn impl in
      skip_if_source_absent (deps_of_vlib_module ~obj_dir ~vimpl ~dir ~sctx ~ml_kind) m
    | Normal m ->
      skip_if_source_absent
        (deps_of_module ~modules ~sandbox ~sctx ~dir ~obj_dir ~ml_kind)
        m
    | Impl_of_virtual_module impl_or_vlib ->
      deps_of ~obj_dir ~modules ~sandbox ~impl ~dir ~sctx ~ml_kind
      @@
      let m = Ml_kind.Dict.get impl_or_vlib ml_kind in
      (match ml_kind with
       | Intf -> Imported_from_vlib m
       | Impl -> Normal m))
;;

let immediate_deps_of unit modules ~obj_dir ~ml_kind =
  match Module.kind unit with
  | Root | Alias _ -> Action_builder.return []
  | Wrapped_compat ->
    let interface_module =
      match Modules.With_vlib.lib_interface modules with
      | Some m -> m
      | None ->
        let modules = Modules.With_vlib.drop_vlib modules in
        Modules.compat_for_exn modules unit
    in
    List.singleton interface_module |> Action_builder.return
  | _ ->
    if has_single_file modules
    then Action_builder.return []
    else Ocamldep.read_immediate_deps_of ~obj_dir ~modules ~ml_kind unit
;;

let dict_of_func_concurrently f =
  let+ impl = f ~ml_kind:Ml_kind.Impl
  and+ intf = f ~ml_kind:Ml_kind.Intf in
  Ml_kind.Dict.make ~impl ~intf
;;

let for_module ~obj_dir ~modules ~sandbox ~impl ~dir ~sctx module_ =
  dict_of_func_concurrently
    (deps_of ~obj_dir ~modules ~sandbox ~impl ~dir ~sctx (Normal module_))
;;

let rules ~obj_dir ~modules ~sandbox ~impl ~sctx ~dir =
  match Modules.With_vlib.as_singleton modules with
  | Some m -> Memo.return (Dep_graph.Ml_kind.dummy m)
  | None ->
    dict_of_func_concurrently (fun ~ml_kind ->
      let+ per_module =
        Modules.With_vlib.obj_map modules
        |> Parallel_map.parallel_map ~f:(fun _obj_name m ->
          deps_of ~obj_dir ~modules ~sandbox ~impl ~sctx ~dir ~ml_kind m)
      in
      Dep_graph.make ~dir ~per_module)
;;
