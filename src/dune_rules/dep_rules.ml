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
      ~for_
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
    Ocamlobjinfo.rules ocaml ~sandbox ~dir ~units:[ unit ]
    |> Action_builder.map ~f:(function
      | [ x ] -> x
      | [] | _ :: _ -> assert false)
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
         Obj_dir.Module.dep obj_dir ~for_ (Transitive (m, ml_kind)) |> Option.value_exn
       in
       Action_builder.map read ~f:transitive_deps_contents
       |> Action_builder.write_file_dyn target)
  in
  read
;;

let wrapped_compat_deps modules m =
  let inner = Modules.compat_for_exn (Modules.With_vlib.drop_vlib modules) m in
  match Modules.With_vlib.lib_interface modules with
  | Some li -> [ li; inner ]
  | None -> [ inner ]
;;

let deps_of_module ~modules ~sandbox ~sctx ~dir ~obj_dir ~ml_kind ~for_ m =
  match Module.kind m with
  | Wrapped_compat ->
    wrapped_compat_deps modules m |> Action_builder.return |> Memo.return
  | _ ->
    let+ deps = Ocamldep.deps_of ~sandbox ~modules ~sctx ~dir ~obj_dir ~ml_kind ~for_ m in
    (match Modules.With_vlib.alias_for modules m with
     | [] -> deps
     | aliases ->
       let open Action_builder.O in
       let+ deps = deps in
       aliases @ deps)
;;

let deps_of_vlib_module ~obj_dir ~vimpl ~dir ~sctx ~ml_kind ~for_ sourced_module =
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
        ~for_
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
        Obj_dir.Module.dep vlib_obj_dir ~for_ (Transitive (m, ml_kind))
        |> Option.value_exn
        |> Path.build
      in
      let dst =
        Obj_dir.Module.dep obj_dir ~for_ (Transitive (m, ml_kind)) |> Option.value_exn
      in
      Super_context.add_rule sctx ~dir (Action_builder.symlink ~src ~dst)
    in
    let modules = Vimpl.vlib_modules vimpl |> Modules.With_vlib.modules in
    Ocamldep.read_deps_of ~obj_dir:vlib_obj_dir ~modules ~ml_kind ~for_ m
;;

(** Tests whether a set of modules is a singleton. *)
let has_single_file modules = Option.is_some @@ Modules.With_vlib.as_singleton modules

(* Conservative direction on resolution failure
   ===========================================

   Both [has_library_deps_of_lib] and [has_library_deps_of_resolved]
   below collapse a [_ Resolve.t] into a plain [bool] for [skip_
   ocamldep]'s consumption. Unresolved dependencies (e.g. the user
   declared [(libraries some_missing_lib)]) make the resolve fail.
   When that happens, both helpers return [true] — "act as if
   library deps are present".

   This is deliberate, not a default-because-we-must:

   - has-deps=true keeps [.d]-file rules in place. The unresolved-
     library error then surfaces in the compile rule, where the
     user gets a useful "library X is not available" diagnostic.

   - has-deps=false would short-circuit the [.d] rules. If anything
     downstream (notably the cross-library walk in
     [Compilation_context.build_lib_index]) later tried to read
     this stanza's [.d], dune would emit a "No rule found for
     .X.objs/X.impl.d" error, masking the real cause behind an
     infrastructure-level message.

   Conservative-true at worst runs ocamldep on a stanza whose
   compilation would have failed anyway; conservative-false risks
   turning a real dependency error into an obscure "no rule"
   error. The trade-off is asymmetric, so the choice is forced. *)

(** Canonical "does this library have any library dependencies?"
    answer for [lib]. The boolean controls whether ocamldep is
    short-circuited for [lib]'s own cctx (via [skip_ocamldep]
    below) and, in turn, whether [lib]'s [.d] files are produced.
    The cross-library walk in [Compilation_context.build_lib_index]
    consults the same helper to decide whether [lib] should land in
    [no_ocamldep] — routing both sites through this function is
    what keeps them in sync. Resolution failures fall back to
    has-deps=true; see the conservative-direction note above. *)
let has_library_deps_of_lib lib ~for_ =
  let open Memo.O in
  let+ resolved = Lib.requires lib ~for_ |> Memo.map ~f:Resolve.peek in
  match resolved with
  | Ok [] -> false
  | Ok _ | Error _ -> true
;;

(** Same answer as [has_library_deps_of_lib], but for cctxes that
    aren't built from a [Lib.t] — executables, tests, melange emit.
    Takes the resolved [direct] and [hidden] requires that
    [Compilation_context.create] has already split per
    [implicit_transitive_deps] mode. Resolution failures fall back
    to has-deps=true; see the conservative-direction note above. *)
let has_library_deps_of_resolved ~direct ~hidden =
  let resolved =
    let open Resolve.Memo.O in
    let+ direct = direct
    and+ hidden = hidden in
    match direct, hidden with
    | [], [] -> false
    | _ -> true
  in
  let open Memo.O in
  let+ peeked = Memo.map resolved ~f:Resolve.peek in
  Result.value peeked ~default:true
;;

(** Tests whether ocamldep can be short-circuited for [modules]: true
    for single-module stanzas that have no library dependencies. The
    premise — "no consumer of ocamldep output can benefit" — was
    valid before #4572; under that PR, cross-library consumers now
    read a target library's ocamldep output as part of the
    per-module inter-library dependency filter, so a target library
    short-circuited here must also be in
    [Compilation_context.build_lib_index]'s [no_ocamldep] set. Both
    sides resolve their [has_library_deps] view through
    [has_library_deps_of_lib] above. *)
let skip_ocamldep ~has_library_deps modules =
  has_single_file modules && not has_library_deps
;;

let rec deps_of
          ~obj_dir
          ~modules
          ~sandbox
          ~impl
          ~dir
          ~sctx
          ~ml_kind
          ~for_
          ~has_library_deps
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
  if is_alias_or_root || skip_ocamldep ~has_library_deps modules
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
      skip_if_source_absent
        (deps_of_vlib_module ~obj_dir ~vimpl ~dir ~sctx ~ml_kind ~for_)
        m
    | Normal m ->
      skip_if_source_absent
        (deps_of_module ~modules ~sandbox ~sctx ~dir ~obj_dir ~ml_kind ~for_)
        m
    | Impl_of_virtual_module impl_or_vlib ->
      deps_of ~obj_dir ~modules ~sandbox ~impl ~dir ~sctx ~ml_kind ~for_ ~has_library_deps
      @@
      let m = Ml_kind.Dict.get impl_or_vlib ml_kind in
      (match ml_kind with
       | Intf -> Imported_from_vlib m
       | Impl -> Normal m))
;;

(* [read_deps_of_module] reports intra-stanza module dependencies. For
   single-module stanzas that dependency graph is trivially empty
   regardless of whether the stanza declares library dependencies, so we
   keep the unconditional short-circuit here. *)
let read_deps_of_module ~modules ~obj_dir dep ~for_ =
  let (Obj_dir.Module.Dep.Immediate (unit, _) | Transitive (unit, _)) = dep in
  match Module.kind unit with
  | Root | Alias _ -> Action_builder.return []
  | Wrapped_compat -> wrapped_compat_deps modules unit |> Action_builder.return
  | _ ->
    if has_single_file modules
    then Action_builder.return []
    else (
      match dep with
      | Immediate (unit, ml_kind) ->
        Ocamldep.read_immediate_deps_of ~obj_dir ~modules ~ml_kind ~for_ unit
      | Transitive (unit, ml_kind) ->
        let open Action_builder.O in
        let+ deps = Ocamldep.read_deps_of ~obj_dir ~modules ~ml_kind ~for_ unit in
        (match Modules.With_vlib.alias_for modules unit with
         | [] -> deps
         | aliases -> aliases @ deps))
;;

let read_immediate_deps_of ~obj_dir ~modules ~ml_kind ~for_ m =
  read_deps_of_module ~modules ~obj_dir (Immediate (m, ml_kind)) ~for_
;;

let read_deps_of ~obj_dir ~modules ~ml_kind ~for_ m =
  if Module.has m ~ml_kind
  then read_deps_of_module ~modules ~obj_dir (Transitive (m, ml_kind)) ~for_
  else Action_builder.return []
;;

let dict_of_func_concurrently f =
  let+ impl = f ~ml_kind:Ml_kind.Impl
  and+ intf = f ~ml_kind:Ml_kind.Intf in
  Ml_kind.Dict.make ~impl ~intf
;;

let for_module ~obj_dir ~modules ~sandbox ~impl ~dir ~sctx ~for_ module_ =
  dict_of_func_concurrently
    (deps_of
       ~obj_dir
       ~modules
       ~sandbox
       ~impl
       ~dir
       ~sctx
       ~for_
       ~has_library_deps:true
       (Normal module_))
;;

let rules ~obj_dir ~modules ~sandbox ~impl ~sctx ~dir ~for_ ~has_library_deps =
  match Modules.With_vlib.as_singleton modules with
  | Some m when not has_library_deps -> Memo.return (Dep_graph.Ml_kind.dummy m)
  | Some _ | None ->
    dict_of_func_concurrently (fun ~ml_kind ->
      let+ per_module =
        Modules.With_vlib.obj_map modules
        |> Parallel_map.parallel_map ~f:(fun _obj_name m ->
          deps_of
            ~obj_dir
            ~modules
            ~sandbox
            ~impl
            ~sctx
            ~dir
            ~ml_kind
            ~for_
            ~has_library_deps
            m)
      in
      Dep_graph.make ~dir ~per_module)
    |> Memo.map ~f:(Dep_graph.Ml_kind.for_module_compilation ~modules)
;;
