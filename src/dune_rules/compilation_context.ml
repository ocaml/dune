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

(* Per-cctx cache of [Lib_flags.L.include_flags] keyed on (lib_mode, sorted
   kept_libs). Two consumer modules with the same kept-libs share one [Args.t].
   Cache is per-cctx, so regenerating the cctx (e.g. on a [(libraries ...)]
   edit) discards it — the requires-split need not be in the key. *)
(* Cache the raw-refs Action_builder built for each [(dep_m,
   ml_kind, cm_kind, is_consumer)] tuple within a single cctx.
   Sibling consumers iterate over overlapping [trans_deps] sets;
   without this cache each call reconstructs a fresh
   [Action_builder.t] tree (the inner [ocamldep] result is shared,
   but the wrapping per-module logic is rebuilt N times per
   stanza). [Table.find] short-circuits before allocating. *)
module Raw_refs = struct
  module Key = struct
    type t =
      { obj_name : Module_name.Unique.t
      ; ml_kind : Ml_kind.t
      ; cm_kind : Lib_mode.Cm_kind.t
      ; is_consumer : bool
      }

    let cm_kind_tag : Lib_mode.Cm_kind.t -> int = function
      | Ocaml Cmi -> 0
      | Ocaml Cmo -> 1
      | Ocaml Cmx -> 2
      | Melange Cmi -> 3
      | Melange Cmj -> 4
    ;;

    let ml_kind_tag : Ml_kind.t -> int = function
      | Intf -> 0
      | Impl -> 1
    ;;

    let equal a b =
      Module_name.Unique.equal a.obj_name b.obj_name
      && ml_kind_tag a.ml_kind = ml_kind_tag b.ml_kind
      && cm_kind_tag a.cm_kind = cm_kind_tag b.cm_kind
      && Bool.equal a.is_consumer b.is_consumer
    ;;

    let hash { obj_name; ml_kind; cm_kind; is_consumer } =
      Poly.hash
        ( Module_name.Unique.to_string obj_name
        , ml_kind_tag ml_kind
        , cm_kind_tag cm_kind
        , is_consumer )
    ;;

    let to_dyn { obj_name; ml_kind; cm_kind; is_consumer } =
      let open Dyn in
      record
        [ "obj_name", Module_name.Unique.to_dyn obj_name
        ; "ml_kind", string (Ml_kind.to_string ml_kind)
        ; "cm_kind", Lib_mode.Cm_kind.to_dyn cm_kind
        ; "is_consumer", bool is_consumer
        ]
    ;;
  end

  type t = (Key.t, Module_name.Set.t Action_builder.t) Table.t

  let create () : t = Table.create (module Key) 64
end

module Filtered_includes = struct
  module Key = struct
    type t =
      { lib_mode : Lib_mode.t
      ; kept_libs : Lib.t list (** sorted by [Lib.compare] *)
      }

    let equal a b =
      Lib_mode.equal a.lib_mode b.lib_mode && List.equal Lib.equal a.kept_libs b.kept_libs
    ;;

    let hash { lib_mode; kept_libs } =
      Poly.hash (Lib_mode.hash lib_mode, List.map kept_libs ~f:Lib.hash)
    ;;

    let to_dyn { lib_mode; kept_libs } =
      let open Dyn in
      record
        [ "lib_mode", Lib_mode.to_dyn lib_mode; "kept_libs", list Lib.to_dyn kept_libs ]
    ;;
  end

  type t = (Key.t, Command.Args.without_targets Command.Args.t Action_builder.t) Table.t

  let create () : t = Table.create (module Key) 16
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
  ; pps_runtime_libs : Lib.t list Resolve.Memo.t
  ; opaque : bool
  ; js_of_ocaml : Js_of_ocaml.In_context.t option Js_of_ocaml.Mode.Pair.t
  ; sandbox : Sandbox_config.t
  ; package : Package.t option
  ; melange_package_name : Lib_name.t option
  ; modes : Mode.Dict.Set.t option
  ; bin_annot : bool
  ; bin_annot_cms : bool
  ; cms_cmt_dependency : Workspace.Context.Cms_cmt_dependency.t
  ; loc : Loc.t option
  ; ocaml : Ocaml_toolchain.t
  ; for_ : Compilation_mode.t
  ; filtered_includes : Filtered_includes.t
  ; raw_refs : Raw_refs.t
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
let pps_runtime_libs t = t.pps_runtime_libs
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

(* Hidden libs must be indexed: otherwise unreached ones fall to
   the glob branch and over-invalidate. *)
let build_lib_index ~super_context ~libs ~for_ =
  let open Resolve.Memo.O in
  let+ per_lib =
    Resolve.Memo.List.map libs ~f:(fun lib ->
      match Lib_info.entry_modules (Lib.info lib) ~for_ with
      | External (Ok names) ->
        Resolve.Memo.return (List.map names ~f:(fun n -> n, lib, None), None)
      | External (Error e) -> Resolve.Memo.of_result (Error e)
      | Local ->
        let* mods =
          Resolve.Memo.lift_memo
            (Dir_contents.modules_of_local_lib
               super_context
               (Lib.Local.of_lib_exn lib)
               ~for_)
        in
        (* [no_ocamldep_lib] mirrors [Dep_rules.skip_ocamldep]'s
           [has_library_deps] gating: use the *resolved* requires
           (which include pps runtime libs added via
           [add_pp_runtime_deps]), not the static [(libraries ...)]
           field. A single-module lib with no [(libraries ...)] but
           with [(preprocess (pps X))] still has its ocamldep run
           because [Lib.requires] is non-empty; classifying it as
           [no_ocamldep] would cause the cross-library walk to skip
           it and drop transitive [.cmi] deps reachable through its
           post-pp ocamldep output. *)
        let+ requires_resolved = Lib.requires lib ~for_ in
        (* [Some m] only for unwrapped locals (tight-eligible);
           wrapped locals and externals → [None]. *)
        let unwrapped =
          match Lib_info.wrapped (Lib.info lib) with
          | Some (This w) -> not (Wrapped.to_bool w)
          | Some (From _) | None -> false
        in
        (* Post-pp [Module.t]: mirror [Pp_spec.pped_modules_map]
           so the cross-lib walker reads ocamldep on the source
           the dep lib's compile pipeline actually produces.
           Staged Pps entries have no materialised [.pp.ml].
           Pps lists composed only of [Instrumentation_backend]
           entries are conditional (active only when the build
           enables [--instrument-with]); we cannot be sure a
           [.pp.ml] exists at this site, so we fall back to
           non-tight-eligible ([m_opt = None]) for those. Pps
           with at least one [Ordinary] entry always produces
           a [.pp.ml]. *)
        let preprocess = Lib_info.preprocess (Lib.info lib) ~for_:Ocaml in
        let post_pp_module m =
          match Preprocess.Per_module.find (Module.name m) preprocess with
          | No_preprocessing | Future_syntax _ -> Some (Module.ml_source m)
          | Action _ -> Some (Module.ml_source (Module.pped m))
          | Pps { staged = false; pps; _ } ->
            if
              List.exists pps ~f:(function
                | Preprocess.With_instrumentation.Ordinary _ -> true
                | Instrumentation_backend _ -> false)
            then Some (Module.pped (Module.ml_source m))
            else None
          | Pps { staged = true; _ } -> None
        in
        let entries =
          List.map (Modules.entry_modules mods) ~f:(fun m ->
            Module.name m, lib, if unwrapped then post_pp_module m else None)
        in
        let no_ocamldep_lib =
          match Modules.as_singleton mods with
          | Some _ when List.is_empty requires_resolved -> Some lib
          | _ -> None
        in
        entries, no_ocamldep_lib)
  in
  let entries = List.concat_map per_lib ~f:fst in
  let no_ocamldep = List.filter_map per_lib ~f:snd |> Lib.Set.of_list in
  Lib_file_deps.Lib_index.create ~no_ocamldep entries
;;

let create
      ~super_context
      ~scope
      ~obj_dir
      ~modules
      ~flags
      ~requires_compile
      ~requires_link
      ?(pps_runtime_libs = Resolve.Memo.return [])
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
      for_
  =
  let project = Scope.project scope in
  let context = Super_context.context super_context in
  let* ocaml = Context.ocaml context in
  let modes =
    match for_, modes with
    | Compilation_mode.Melange, _ -> None
    | Ocaml, Some modes -> Some modes
    | Ocaml, None -> Some (Mode.Dict.make_both true)
  in
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
  let sandbox = Compilation_mode.default_sandbox for_ in
  let modes =
    match for_, modes with
    | Compilation_mode.Melange, _ -> None
    | Ocaml, Some modes -> Some modes
    | Ocaml, None -> Some (Mode.Dict.make_both true)
  in
  let opaque =
    let profile = Context.profile context in
    eval_opaque ocaml profile opaque
  in
  let* has_library_deps =
    (* Single-module stanzas still run ocamldep when they have library
       deps so the per-module filter can inform the result. *)
    let open Resolve.Memo.O in
    let+ direct = direct_requires
    and+ hidden = hidden_requires in
    match direct, hidden with
    | [], [] -> false
    | _ -> true
  in
  (* Resolution errors surface later through the normal compilation
     rules; assume deps are present here. *)
  let has_library_deps = Resolve.peek has_library_deps |> Result.value ~default:true in
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
        let* d = direct_requires
        and* h = hidden_requires in
        build_lib_index ~super_context ~libs:(d @ h) ~for_)
  ; has_virtual_impl =
      Memo.lazy_ (fun () ->
        let open Resolve.Memo.O in
        let+ direct = direct_requires
        and+ hidden = hidden_requires in
        List.exists (direct @ hidden) ~f:(fun lib -> Option.is_some (Lib.implements lib)))
  ; preprocessing
  ; pps_runtime_libs
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
  ; filtered_includes = Filtered_includes.create ()
  ; raw_refs = Raw_refs.create ()
  }
;;

let for_ t = t.for_

let cached_raw_refs t ~dep_m ~ml_kind ~cm_kind ~is_consumer compute =
  let cache_key =
    { Raw_refs.Key.obj_name = Module.obj_name dep_m; ml_kind; cm_kind; is_consumer }
  in
  match Table.find t.raw_refs cache_key with
  | Some builder -> builder
  | None ->
    let builder = compute () in
    Table.set t.raw_refs cache_key builder;
    builder
;;

let filtered_include_flags t ~cm_kind ~kept_libs =
  let lib_mode = Lib_mode.of_cm_kind cm_kind in
  let cache_key =
    { Filtered_includes.Key.lib_mode; kept_libs = Lib.Set.to_list kept_libs }
  in
  match Table.find t.filtered_includes cache_key with
  | Some builder -> builder
  | None ->
    (* Cache the [Action_builder.t] (not the resolved args) up-front
       at rule-construction time so all compile rules sharing this
       [(lib_mode, kept_libs)] share one builder; [Action_builder.memoize]
       then dedupes its evaluation. Mirrors the cache pattern in
       [Ocamldep.read_immediate_deps_words]. *)
    let builder =
      let open Action_builder.O in
      let* direct_requires = Resolve.Memo.read t.requires_compile in
      let+ hidden_requires = Resolve.Memo.read t.requires_hidden in
      let direct_filtered = List.filter direct_requires ~f:(Lib.Set.mem kept_libs) in
      let hidden_filtered = List.filter hidden_requires ~f:(Lib.Set.mem kept_libs) in
      let project = Scope.project t.scope in
      let lib_config = t.ocaml.lib_config in
      Lib_flags.L.include_flags
        ~project
        ~direct_libs:direct_filtered
        ~hidden_libs:hidden_filtered
        lib_mode
        lib_config
      |> Command.Args.memo
    in
    let builder = Action_builder.memoize "filtered_include_flags" builder in
    Table.set t.filtered_includes cache_key builder;
    builder
;;

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
  ; lib_index =
      Memo.lazy_ (fun () ->
        (* Unreachable: synthesised modules use [Dep_graph.dummy]
           (whose [dir] is [Path.Build.root]), so
           [lib_deps_for_module]'s [can_filter] dir-equality guard
           fails and the non-filter fallback is taken. *)
        Code_error.raise
          "Compilation_context.lib_index forced for a module synthesised at link time; \
           this should be unreachable."
          [])
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
let set_modes t ~modes = { t with modes = Some modes }

let instances t =
  match t.instances with
  | None -> Action_builder.return Parameterised_instances.none
  | Some instances -> Resolve.Memo.read instances
;;
