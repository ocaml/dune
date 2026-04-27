open Import
open Memo.O

module Includes = struct
  type t = Command.Args.without_targets Command.Args.t Lib_mode.Cm_kind.Map.t

  let make ~project ~direct_requires ~hidden_requires lib_config
    : _ Lib_mode.Cm_kind.Map.t
    =
    let open Resolve.Memo.O in
    let iflags direct_libs hidden_libs mode =
      Lib_flags.L.include_flags ~project ~direct_libs ~hidden_libs mode lib_config
    in
    let make_includes_args ~mode =
      (let+ direct_libs = direct_requires
       and+ hidden_libs = hidden_requires in
       iflags direct_libs hidden_libs mode)
      |> Resolve.Memo.args
      |> Command.Args.memo
    in
    { ocaml =
        (let cmi_includes = make_includes_args ~mode:(Ocaml Byte) in
         { cmi = cmi_includes
         ; cmo = cmi_includes
         ; cmx = make_includes_args ~mode:(Ocaml Native)
         })
    ; melange =
        (let mel_includes = make_includes_args ~mode:Melange in
         { cmi = mel_includes; cmj = mel_includes })
    }
  ;;

  let empty = Lib_mode.Cm_kind.Map.make_all Command.Args.empty
end

type opaque =
  | Explicit of bool
  | Inherit_from_settings

let eval_opaque (ocaml : Ocaml_toolchain.t) profile = function
  | Explicit b -> b
  | Inherit_from_settings ->
    Profile.is_dev profile && Ocaml.Version.supports_opaque_for_mli ocaml.version
;;

type modules =
  { modules : Modules.With_vlib.t
  ; dep_graphs : Dep_graph.t Ml_kind.Dict.t
  }

let singleton_modules m =
  { modules = Modules.With_vlib.singleton m; dep_graphs = Dep_graph.Ml_kind.dummy m }
;;

type t =
  { super_context : Super_context.t
  ; scope : Scope.t
  ; obj_dir : Path.Build.t Obj_dir.t
  ; modules : modules
  ; flags : Ocaml_flags.t
  ; requires_compile : Lib.t list Resolve.Memo.t
  ; requires_hidden : Lib.t list Resolve.Memo.t
  ; requires_link : Lib.t list Resolve.t Memo.Lazy.t
  ; implements : Virtual_rules.t
  ; parameters : Module_name.t list Resolve.Memo.t
  ; instances : Parameterised_instances.t Resolve.Memo.t option
  ; includes : Includes.t
  ; lib_index : Lib_file_deps.Lib_index.t Resolve.t Memo.Lazy.t
  ; has_virtual_impl : bool Resolve.t Memo.Lazy.t
  ; preprocessing : Pp_spec.t
  ; opaque : bool
  ; js_of_ocaml : Js_of_ocaml.In_context.t option Js_of_ocaml.Mode.Pair.t
  ; sandbox : Sandbox_config.t
  ; package : Package.t option
  ; melange_package_name : Lib_name.t option
  ; modes : Lib_mode.Map.Set.t
  ; bin_annot : bool
  ; bin_annot_cms : bool
  ; cms_cmt_dependency : Workspace.Context.Cms_cmt_dependency.t
  ; loc : Loc.t option
  ; ocaml : Ocaml_toolchain.t
  ; for_ : Compilation_mode.t
  }

let loc t = t.loc
let super_context t = t.super_context
let scope t = t.scope
let dir t = Obj_dir.dir t.obj_dir
let obj_dir t = t.obj_dir
let modules t = t.modules.modules
let flags t = t.flags
let requires_compile t = t.requires_compile
let requires_hidden t = t.requires_hidden
let requires_link t = Memo.Lazy.force t.requires_link
let parameters t = t.parameters
let includes t = t.includes
let lib_index t = Memo.Lazy.force t.lib_index
let has_virtual_impl t = Memo.Lazy.force t.has_virtual_impl
let preprocessing t = t.preprocessing
let opaque t = t.opaque
let js_of_ocaml t = t.js_of_ocaml
let sandbox t = t.sandbox
let set_sandbox t sandbox = { t with sandbox }
let package t = t.package
let melange_package_name t = t.melange_package_name
let implements t = t.implements
let modes t = t.modes
let bin_annot t = t.bin_annot
let bin_annot_cms t = t.bin_annot_cms
let cms_cmt_dependency t = t.cms_cmt_dependency
let context t = Super_context.context t.super_context
let dep_graphs t = t.modules.dep_graphs
let ocaml t = t.ocaml

let parameters_main_modules parameters =
  let open Resolve.Memo.O in
  let* parameters = parameters in
  Resolve.Memo.List.map parameters ~f:(fun param ->
    let+ main = Lib.main_module_name param in
    match main with
    | Some main -> main
    | None ->
      Code_error.raise
        "Expected library parameter to have a main module"
        [ "param", Lib.to_dyn param ])
;;

(* Build a [Lib_index] from the consumer's [direct_requires]. Hidden
   requires are intentionally excluded — the user has not committed
   to them as direct dependencies, so they fall to glob in the
   post-walk classification rather than getting per-module
   precision. Local libs short-circuited by [Dep_rules.skip_ocamldep]
   (single-module + no resolved deps) land in [no_ocamldep] so the
   cross-library walk doesn't read [.d] files dune never produced. *)
let build_lib_index ~super_context ~libs ~for_ =
  let open Resolve.Memo.O in
  let+ per_lib =
    Resolve.Memo.List.map libs ~f:(fun lib ->
      match Lib_info.entry_modules (Lib.info lib) ~for_ with
      | External (Ok names) ->
        Resolve.Memo.return (List.map names ~f:(fun n -> n, lib, None), None, None)
      | External (Error e) -> Resolve.Memo.of_result (Error e)
      | Local ->
        Resolve.Memo.lift_memo
          (let open Memo.O in
           let* mods =
             Dir_contents.modules_of_local_lib
               super_context
               (Lib.Local.of_lib_exn lib)
               ~for_
           in
           (* Every local lib's entries carry [Some m] so the walk
              can read the entry's ocamldep. Whether to issue per-
              module deps is tracked separately via [unwrapped_local]
              (tight-eligible = local AND unwrapped). Wrapped local
              libs are walkable too via the indexing below, but
              remain non-tight (see [lib_file_deps.ml] for the
              ocamldep-granularity reason). *)
           let unwrapped =
             match Lib_info.wrapped (Lib.info lib) with
             | Some (This w) -> not (Wrapped.to_bool w)
             | Some (From _) | None -> false
           in
           let entry_modules = Modules.entry_modules mods in
           let entry_entries =
             List.map entry_modules ~f:(fun m -> Module.name m, lib, Some m)
           in
           let entries =
             if unwrapped
             then entry_entries
             else (
               let entry_obj_names =
                 Module_name.Unique.Set.of_list_map entry_modules ~f:Module.obj_name
               in
               let child_modules =
                 Modules.fold_user_available mods ~init:[] ~f:(fun m acc ->
                   if Module_name.Unique.Set.mem entry_obj_names (Module.obj_name m)
                   then acc
                   else m :: acc)
               in
               (* The BFS reaches wrapped libs' children through
                  whichever of these matches the wrapper kind:

                  - Auto-generated wrappers ([Alias]) lack a [.d]
                    ([Dep_rules.deps_of] short-circuits on [Alias]).
                    Index every child under the wrapper's name
                    (multi-entry) so a frontier hit on the wrapper
                    expands to the children directly.

                  - Hand-written wrappers ([Impl]/[Intf_only]) have
                    a [.d] that names exposed children via the
                    [module X = X] aliases the user wrote. Index
                    children under their own names; the BFS
                    resolves those emitted names in the next round.
                    Multi-entry-under-wrapper would re-read every
                    child's [.d] on each wrapper hit, including
                    children the wrapper doesn't expose. *)
               let any_alias_wrapper =
                 List.exists entry_modules ~f:(fun m ->
                   match Module.kind m with
                   | Alias _ -> true
                   | _ -> false)
               in
               let child_entries =
                 if any_alias_wrapper
                 then
                   List.concat_map entry_modules ~f:(fun wrapper ->
                     List.map child_modules ~f:(fun child ->
                       Module.name wrapper, lib, Some child))
                 else
                   List.map child_modules ~f:(fun child ->
                     Module.name child, lib, Some child)
               in
               entry_entries @ child_entries)
           in
           (* Only single-module libs can land in [no_ocamldep];
              multi-module libs always have [.d] files for their
              children. Skip the helper call otherwise. *)
           let+ no_ocamldep_lib =
             match Modules.as_singleton mods with
             | None -> Memo.return None
             | Some _ ->
               (* Same helper the lib's own cctx uses — keeps the
                  skip decision and the walk's prediction in lock
                  step. *)
               let+ has_resolved_deps = Dep_rules.has_library_deps_of_lib lib ~for_ in
               if has_resolved_deps then None else Some lib
           in
           entries, no_ocamldep_lib, if unwrapped then Some lib else None))
  in
  let entries = List.concat_map per_lib ~f:(fun (e, _, _) -> e) in
  let no_ocamldep = List.filter_map per_lib ~f:(fun (_, n, _) -> n) |> Lib.Set.of_list in
  let unwrapped_local =
    List.filter_map per_lib ~f:(fun (_, _, u) -> u) |> Lib.Set.of_list
  in
  Lib_file_deps.Lib_index.create ~no_ocamldep ~unwrapped_local ~entries
;;

let create
      ~super_context
      ~scope
      ~obj_dir
      ~modules
      ~flags
      ~requires_compile
      ~requires_link
      ?(preprocessing = Pp_spec.dummy)
      ~opaque
      ~js_of_ocaml
      ~package
      ~melange_package_name
      ?(implements = Virtual_rules.no_implements)
      ?parameters
      ?modes
      ?bin_annot
      ?bin_annot_cms
      ?cms_cmt_dependency
      ?loc
      ?instances
      ?own_lib
      for_
  =
  let project = Scope.project scope in
  let context = Super_context.context super_context in
  let* ocaml = Context.ocaml context in
  let direct_requires, hidden_requires =
    match Dune_project.implicit_transitive_deps project ocaml.version with
    | Enabled -> Memo.Lazy.force requires_link, Resolve.Memo.return []
    | Disabled -> requires_compile, Resolve.Memo.return []
    | Disabled_with_hidden_includes ->
      let requires_hidden =
        let open Resolve.Memo.O in
        let+ requires_compile = requires_compile
        and+ requires_link = Memo.Lazy.force requires_link in
        let requires_table = Table.create (module Lib) 5 in
        List.iter ~f:(fun lib -> Table.set requires_table lib ()) requires_compile;
        List.filter requires_link ~f:(fun l -> not (Table.mem requires_table l))
      in
      requires_compile, requires_hidden
  in
  let parameters =
    match parameters with
    | None -> Resolve.Memo.return []
    | Some parameters -> parameters_main_modules parameters
  in
  let sandbox =
    match for_ with
    | Compilation_mode.Ocaml -> Sandbox_config.no_special_requirements
    | Compilation_mode.Melange -> Sandbox_config.needs_sandboxing
  in
  let modes =
    let default = { Lib_mode.Map.ocaml = Mode.Dict.make_both true; melange = false } in
    Option.value ~default modes
  in
  let opaque =
    let profile = Context.profile context in
    eval_opaque ocaml profile opaque
  in
  let* has_library_deps =
    (* For library cctxes, route through the same helper
       [build_lib_index] uses, so the lib's skip decision and the
       cross-stanza walk's prediction can't drift. Other cctxes
       just need the boolean. *)
    match own_lib with
    | Some lib -> Dep_rules.has_library_deps_of_lib lib ~for_
    | None ->
      Dep_rules.has_library_deps_of_resolved
        ~direct:direct_requires
        ~hidden:hidden_requires
  in
  let+ dep_graphs =
    Dep_rules.rules
      ~dir:(Obj_dir.dir obj_dir)
      ~sandbox
      ~obj_dir
      ~sctx:super_context
      ~impl:implements
      ~modules
      ~for_
      ~has_library_deps
  and+ bin_annot =
    match bin_annot with
    | Some b -> Memo.return b
    | None -> Env_stanza_db.bin_annot ~dir:(Obj_dir.dir obj_dir)
  and+ bin_annot_cms =
    match bin_annot_cms with
    | Some b -> Memo.return b
    | None -> Env_stanza_db.bin_annot_cms ~dir:(Obj_dir.dir obj_dir)
  and+ cms_cmt_dependency =
    match cms_cmt_dependency with
    | Some v -> Memo.return v
    | None ->
      let context = Super_context.context super_context in
      Memo.return (Context.cms_cmt_dependency context)
  in
  { super_context
  ; scope
  ; obj_dir
  ; modules = { modules; dep_graphs }
  ; flags
  ; requires_compile = direct_requires
  ; requires_hidden = hidden_requires
  ; requires_link
  ; implements
  ; parameters
  ; includes = Includes.make ~project ~direct_requires ~hidden_requires ocaml.lib_config
  ; lib_index =
      Memo.lazy_ (fun () ->
        let open Resolve.Memo.O in
        let* libs = direct_requires in
        build_lib_index ~super_context ~libs ~for_)
  ; has_virtual_impl =
      Memo.lazy_ (fun () ->
        let open Resolve.Memo.O in
        let+ direct = direct_requires
        and+ hidden = hidden_requires in
        List.exists (direct @ hidden) ~f:(fun lib -> Option.is_some (Lib.implements lib)))
  ; preprocessing
  ; opaque
  ; js_of_ocaml
  ; sandbox
  ; package
  ; melange_package_name
  ; modes
  ; bin_annot
  ; bin_annot_cms
  ; cms_cmt_dependency
  ; loc
  ; ocaml
  ; instances
  ; for_
  }
;;

let for_ t = t.for_

let alias_and_root_module_flags =
  let extra = [ "-w"; "-49" ] in
  fun base -> Ocaml_flags.append_common base extra |> Ocaml_flags.append_nostdlib
;;

let for_alias_module t alias_module =
  let keep_flags = Modules.With_vlib.is_stdlib_alias (modules t) alias_module in
  let flags =
    if keep_flags
    then (* in the case of stdlib, these flags can be written by the user *)
      t.flags
    else (
      let project = Scope.project t.scope in
      let dune_version = Dune_project.dune_version project in
      let profile = Super_context.context t.super_context |> Context.profile in
      Ocaml_flags.default ~dune_version ~profile)
  in
  let flags =
    match t.instances with
    | None -> flags
    | Some _ ->
      (* If the alias file instantiates parameterised libraries,
         the [misplace-attribute] warning is currently raised on
         [@jane.non_erasable.instances] *)
      Ocaml_flags.append_common flags [ "-w"; "-53" ]
  in
  let sandbox =
    (* If the compiler reads the cmi for module alias even with [-w -49
       -no-alias-deps], we must sandbox the build of the alias module since the
       modules it references are built after. *)
    if Ocaml.Version.always_reads_alias_cmi t.ocaml.version
    then Sandbox_config.needs_sandboxing
    else Sandbox_config.no_special_requirements
  in
  let (modules, includes) : modules * Includes.t =
    match Modules.With_vlib.is_stdlib_alias t.modules.modules alias_module with
    | false -> singleton_modules alias_module, Includes.empty
    | true ->
      (* The stdlib alias module is different from the alias modules usually
         produced by Dune: it contains code and depends on a few other
         [CamlinnternalXXX] modules from the stdlib, so we need the full set of
         modules to compile it. *)
      t.modules, t.includes
  in
  { t with flags = alias_and_root_module_flags flags; includes; sandbox; modules }
;;

let for_root_module t root_module =
  let flags =
    let project = Scope.project t.scope in
    let dune_version = Dune_project.dune_version project in
    let profile = Super_context.context t.super_context |> Context.profile in
    Ocaml_flags.default ~profile ~dune_version
  in
  { t with
    flags = alias_and_root_module_flags flags
  ; modules = singleton_modules root_module
  }
;;

let for_module_generated_at_link_time cctx ~requires ~module_ =
  let opaque =
    (* Cmi's of link time generated modules are compiled with -opaque, hence
       their implementation must also be compiled with -opaque *)
    Ocaml.Version.supports_opaque_for_mli cctx.ocaml.version
  in
  let modules = singleton_modules module_ in
  let includes =
    let hidden_requires = Resolve.Memo.return [] in
    let direct_requires = requires in
    Includes.make
      ~project:(Scope.project cctx.scope)
      ~direct_requires
      ~hidden_requires
      cctx.ocaml.lib_config
  in
  { cctx with
    opaque
  ; flags = Ocaml_flags.empty
  ; requires_link = Memo.lazy_ (fun () -> requires)
  ; requires_compile = requires
  ; includes
  ; lib_index = Memo.lazy_ (fun () -> Resolve.Memo.return Lib_file_deps.Lib_index.empty)
  ; modules
  }
;;

let for_wrapped_compat t =
  (* See #10689 *)
  let flags = Ocaml_flags.append_common t.flags [ "-w"; "-53" ] in
  { t with includes = Includes.empty; flags }
;;

let for_plugin_executable t ~embed_in_plugin_libraries =
  let libs = Scope.libs t.scope in
  let requires_link =
    Memo.lazy_ (fun () ->
      Resolve.Memo.List.map ~f:(Lib.DB.resolve libs) embed_in_plugin_libraries)
  in
  { t with requires_link }
;;

let without_bin_annot t =
  { t with
    bin_annot = false
  ; bin_annot_cms = false
  ; cms_cmt_dependency = Workspace.Context.Cms_cmt_dependency.No_dependency
  }
;;

let set_obj_dir t obj_dir = { t with obj_dir }
let set_modes t ~modes = { t with modes }

let instances t =
  match t.instances with
  | None -> Action_builder.return Parameterised_instances.none
  | Some instances -> Resolve.Memo.read instances
;;
