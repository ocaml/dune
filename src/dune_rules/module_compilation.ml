open Import
open Memo.O

let all_libs cctx =
  let open Resolve.Memo.O in
  let+ d = Compilation_context.requires_compile cctx
  and+ h = Compilation_context.requires_hidden cctx in
  d @ h
;;

(* Package dependency inference walks file dependencies of installable rules.
   When narrowing drops a local library's artifacts, retain a dependency on its
   stable package metadata so strict package checks still see the declaration. *)
let strict_package_deps_markers cctx libs =
  let project = Compilation_context.scope cctx |> Scope.project in
  if not (Dune_project.strict_package_deps project)
  then Dep.Set.empty
  else (
    let build_context = Compilation_context.context cctx |> Context.build_context in
    let current_package = Compilation_context.package cctx |> Option.map ~f:Package.id in
    List.filter_map libs ~f:(fun lib ->
      let package =
        match Lib_info.status (Lib.info lib) with
        | Lib_info.Status.Public (_, package) | Lib_info.Status.Private (_, Some package)
          -> Some package
        | Lib_info.Status.Installed_private | Lib_info.Status.Installed
        | Lib_info.Status.Private (_, None) -> None
      in
      Option.bind package ~f:(fun package ->
        match current_package with
        | Some current when Package.Id.compare current (Package.id package) = Eq -> None
        | None | Some _ ->
          let dir =
            Path.Build.append_source build_context.build_dir (Package.dir package)
          in
          let filename =
            Package.Name.to_string (Package.name package) ^ "." ^ Dune_package.fn
          in
          Some (Path.Build.relative dir filename |> Path.build |> Dep.file)))
    |> Dep.Set.of_list)
;;

let union_module_name_sets_mapped xs ~f =
  Action_builder.List.map xs ~f
  |> Action_builder.map
       ~f:(List.fold_left ~init:Module_name.Set.empty ~f:Module_name.Set.union)
;;

(* Native compilation consumes interfaces produced with byte flags, so both
   flag sets can introduce opens that affect its dependency frontier. *)
let open_modules_for_mode flags mode =
  let open Action_builder.O in
  let open_modules mode =
    let+ flags = Ocaml_flags.get flags mode in
    Ocaml_flags.extract_open_module_names flags
  in
  match mode with
  | Lib_mode.Ocaml Native ->
    let* byte = open_modules (Lib_mode.Ocaml Byte) in
    let+ native = open_modules (Lib_mode.Ocaml Native) in
    Module_name.Set.union byte native
  | (Lib_mode.Ocaml Byte | Lib_mode.Melange) as mode -> open_modules mode
;;

let module_kind_is_filterable m =
  match Module.kind m with
  | Root | Wrapped_compat | Impl_vmodule | Virtual | Parameter -> false
  | Intf_only | Impl | Alias _ -> true
;;

(* BFS over tight-eligible entries: each (lib, entry) pair's impl+intf ocamldep
   names extend the frontier. Entries lacking a tight-eligible module —
   [None]-entries (wrapped locals, externals, staged-Pps modules) and libs in
   [no_ocamldep] (walker-terminal singletons) — are skipped by
   [lookup_tight_entries], terminating chains through them. The [Module.t]
   supplied here is the post-pp form (constructed in [build_lib_index]), so
   ocamldep runs on the dep lib's [.pp.ml] (action / non-staged Pps) or directly
   on the source (no preprocessing / future-syntax).

   Each visited local lib's [-open Foo] flags also extend the frontier: a dep
   lib whose effective flags open [Foo] can use [Foo]'s identifiers without
   naming [Foo] in its source, so ocamldep on the dep lib's source produces
   no token to walk through. The opened module names are the missing edges.
   Both the stanza's own [(flags ...)] and any [(env ...)] stanza that
   contributes default flags to the dep lib's directory can inject [-open]
   entries; we evaluate the fully-merged flags so neither path is missed
   (see #14517 for the env-stanza case). External libs are short-circuited:
   their [src_dir] is not a build path, and env stanzas cannot inject flags
   into already-compiled artifacts. *)
let cross_lib_tight_set ~sandbox ~sctx ~lib_index ~mode ~initial_refs =
  let open Action_builder.O in
  let read_lib_opens lib =
    if not (Lib.is_local lib)
    then Action_builder.return Module_name.Set.empty
    else (
      let info = Lib.info lib in
      let spec = Lib_info.stanza_flags info in
      let dir = Lib_info.src_dir info |> Path.as_in_build_dir_exn in
      let* ocaml_flags =
        Action_builder.of_memo (Ocaml_flags_db.ocaml_flags sctx ~dir spec)
      in
      open_modules_for_mode ocaml_flags mode)
  in
  let read_entry_deps (lib, m) =
    let obj_dir = Lib.info lib |> Lib_info.obj_dir |> Obj_dir.as_local_exn in
    let* impl_deps =
      Ocamldep.read_immediate_deps_raw_of ~sandbox ~sctx ~obj_dir ~ml_kind:Impl m
    in
    let* intf_deps =
      Ocamldep.read_immediate_deps_raw_of ~sandbox ~sctx ~obj_dir ~ml_kind:Intf m
    in
    let+ lib_opens = read_lib_opens lib in
    Module_name.Set.union impl_deps (Module_name.Set.union intf_deps lib_opens)
  in
  let rec loop ~seen ~frontier =
    if Module_name.Set.is_empty frontier
    then Action_builder.return seen
    else (
      let pairs =
        Module_name.Set.fold frontier ~init:[] ~f:(fun name acc ->
          Lib_file_deps.Lib_index.lookup_tight_entries lib_index name @ acc)
      in
      let* discovered = union_module_name_sets_mapped pairs ~f:read_entry_deps in
      let seen = Module_name.Set.union seen frontier in
      let frontier = Module_name.Set.diff discovered seen in
      loop ~seen ~frontier)
  in
  loop ~seen:Module_name.Set.empty ~frontier:initial_refs
;;

(* See the module-level comment in [lib_file_deps.ml] for the two dep shapes
   ([deps_of_entry_modules] vs [deps_of_entries]) and why wrapped dep libraries
   always take the glob path. *)
let lib_deps_for_module ~cctx ~obj_dir ~for_ ~dep_graph ~opaque ~cm_kind ~ml_kind ~mode m =
  let open Action_builder.O in
  let cctx_includes_for_cm_kind () =
    Lib_mode.Cm_kind.Map.get (Compilation_context.includes cctx) cm_kind
  in
  let can_filter =
    (* Skip when [dep_graph] is the dummy ([Dep_graph.dummy] with
       [dir = Path.Build.root]); used for singleton-module stanzas and
       link-time-synthesised modules where no transitive deps are available. *)
    Path.Build.equal (Dep_graph.dir dep_graph) (Obj_dir.dir obj_dir)
    (* Modules synthesised outside the stanza, handed to [ocamlc_i]. *)
    && Dep_graph.mem dep_graph m
    && module_kind_is_filterable m
    && Module.has m ~ml_kind
    (* Consumer-stanza virtual-impl: handled by [Dep_rules]. The deps-side
       counterpart ([has_virtual_impl] below) covers the case where a lib in
       [requires] is a virtual impl. *)
    && not (Virtual_rules.is_virtual_or_parameter (Compilation_context.implements cctx))
  in
  let* libs = Resolve.Memo.read (all_libs cctx) in
  if not can_filter
  then
    Action_builder.return
      (cctx_includes_for_cm_kind (), Lib_file_deps.deps_of_entries ~opaque ~cm_kind libs)
  else
    let* has_virtual_impl =
      Resolve.Memo.read (Compilation_context.has_virtual_impl cctx)
    in
    if has_virtual_impl
    then
      Action_builder.return
        (cctx_includes_for_cm_kind (), Lib_file_deps.deps_of_entries ~opaque ~cm_kind libs)
    else
      let* lib_index = Resolve.Memo.read (Compilation_context.lib_index cctx) in
      let sandbox = Compilation_context.sandbox cctx in
      let sctx = Compilation_context.super_context cctx in
      let* trans_deps = Dep_graph.deps_of dep_graph m in
      (* Read [dep_m]'s [.ml]-side ocamldep only when its references can
         propagate to the consumer:

         | [dep_m] is              | [cm_kind]   | [opaque] | read [.ml]?  |
         | ----------------------- | ----------- | -------- | ------------ |
         | consumer ([m] itself)   | any         | any      | iff [Impl]   |
         | trans_dep, no [.mli]    | any         | any      | yes          |
         | trans_dep, has [.mli]   | [Cmx]       | false    | yes (inline) |
         | trans_dep, has [.mli]   | [Cmx]       | true     | no           |
         | trans_dep, has [.mli]   | [Cmi]/[Cmo] | any      | no           | *)
      let need_impl_deps_of dep_m ~is_consumer =
        if is_consumer
        then (
          match ml_kind with
          | Ml_kind.Impl -> true
          | Intf -> false)
        else
          (not (Module.has dep_m ~ml_kind:Intf))
          ||
          match cm_kind with
          | Ocaml Cmx -> not opaque
          | Ocaml (Cmi | Cmo) | Melange _ -> false
      in
      let read_dep_m_raw dep_m ~is_consumer =
        let key : Compilation_context.Raw_refs.Key.t =
          let obj_name = Module.obj_name dep_m in
          if is_consumer
          then Consumer { obj_name; ml_kind }
          else Transitive { obj_name; cm_kind }
        in
        Compilation_context.cached_raw_refs cctx ~key ~compute:(fun () ->
          let* impl_deps =
            if need_impl_deps_of dep_m ~is_consumer
            then
              Ocamldep.read_immediate_deps_raw_of
                ~sandbox
                ~sctx
                ~obj_dir
                ~ml_kind:Impl
                dep_m
            else Action_builder.return Module_name.Set.empty
          in
          let+ intf_deps =
            Ocamldep.read_immediate_deps_raw_of
              ~sandbox
              ~sctx
              ~obj_dir
              ~ml_kind:Intf
              dep_m
          in
          Module_name.Set.union impl_deps intf_deps)
      in
      let* m_raw = read_dep_m_raw m ~is_consumer:true in
      let* trans_raw =
        union_module_name_sets_mapped trans_deps ~f:(read_dep_m_raw ~is_consumer:false)
      in
      let all_raw = Module_name.Set.union m_raw trans_raw in
      let* open_modules = open_modules_for_mode (Compilation_context.flags cctx) mode in
      (* [Stdlib] is implicitly opened by every compile and so is referenced by
         every consumer even when ocamldep doesn't list it. For OCaml-mode
         compiles the compiler's built-in stdlib path supplies it (no dune lib
         to keep); for Melange-mode compiles the [melange]/[stdlib] lib supplies
         it as a regular dune dependency, and would otherwise be dropped from
         [kept_libs] for consumers that don't syntactically name a [Stdlib.X]
         member. Force it onto the frontier so the [Lib_index] lookup adds it
         when present. *)
      let referenced =
        let implicit_stdlib =
          match Module_name.of_string_opt "Stdlib" with
          | Some n -> Module_name.Set.singleton n
          | None -> Module_name.Set.empty
        in
        Module_name.Set.union (Module_name.Set.union all_raw open_modules) implicit_stdlib
      in
      let { Lib_file_deps.Lib_index.tight; non_tight } =
        Lib_file_deps.Lib_index.filter_libs_with_modules
          lib_index
          ~referenced_modules:referenced
      in
      (* [ppx_runtime_libraries] introduce module references through post-pp
         source that ocamldep cannot see; carry them through to [all_libs] so
         the classification fold sees them, and force them onto the glob path
         via [must_glob_set]. *)
      let* pps_runtime_libs =
        Resolve.Memo.read (Compilation_context.pps_runtime_libs cctx)
      in
      (* [Lib.closure]'s memo key is order- and multiplicity-sensitive on the
         input list. [pps_runtime_libs] can both contain duplicates (multiple
         pps sharing a runtime dep) and overlap with [tight]/[non_tight] (a lib
         referenced both syntactically and via [add_pp_runtime_deps]).
         [sort_uniq] keeps the input canonical for memoization. *)
      let direct_libs =
        List.sort_uniq
          ~compare:Lib.compare
          (Lib.Map.keys tight @ Lib.Set.to_list non_tight @ pps_runtime_libs)
      in
      (* Close transitively over transparent aliases that ocamldep doesn't
         report. *)
      let* all_libs = Resolve.Memo.read (Lib.closure direct_libs ~linking:false ~for_) in
      let* tight_set =
        cross_lib_tight_set ~sandbox ~sctx ~lib_index ~mode ~initial_refs:referenced
      in
      (* Wrapped-lib soundness recovery: when the cross-library walk reaches a
         wrapped local lib's wrapper, the consumer may reach the lib's
         transitive closure through aliases the walk cannot see; glob that
         closure unconditionally. *)
      let wrapped_referenced =
        Lib_file_deps.Lib_index.wrapped_libs_referenced
          lib_index
          ~referenced_modules:tight_set
      in
      let* must_glob_libs =
        Resolve.Memo.read
          (Lib.closure
             (List.sort_uniq
                ~compare:Lib.compare
                (Lib.Set.to_list wrapped_referenced @ pps_runtime_libs))
             ~linking:false
             ~for_)
      in
      let must_glob_set = Lib.Set.of_list must_glob_libs in
      (* Classify each lib in [all_libs]: - lib has [None]-entry referenced
         (mixed-entry or wrapped) → glob (covers the None entries' [.cmi]s); -
         lib has only [Some] entries referenced → per-module deps; - lib
         unreached but tight-eligible → drop (link rule pulls it in via
         [requires_link]); - lib unreached and not tight-eligible → glob.
         [kept_libs] gets every lib that contributes a tight or glob dep — used
         by [Compilation_context.filtered_include_flags] to scope the consumer's
         [-I]/[-H] flags. *)
      let { Lib_file_deps.Lib_index.tight = tight_modules; non_tight = non_tight_set } =
        Lib_file_deps.Lib_index.filter_libs_with_modules
          lib_index
          ~referenced_modules:tight_set
      in
      let tight_deps, glob_libs, kept_libs =
        List.fold_left
          all_libs
          ~init:(Dep.Set.empty, [], Lib.Set.empty)
          ~f:(fun (td, gl, kl) lib ->
            if Lib.Set.mem must_glob_set lib || Lib.Set.mem non_tight_set lib
            then td, lib :: gl, Lib.Set.add kl lib
            else (
              match Lib.Map.find tight_modules lib with
              | Some modules ->
                ( Dep.Set.union
                    td
                    (Lib_file_deps.deps_of_entry_modules ~opaque ~cm_kind lib modules)
                , gl
                , Lib.Set.add kl lib )
              | None ->
                if Lib_file_deps.Lib_index.is_tight_eligible lib_index lib
                then td, gl, kl
                else td, lib :: gl, Lib.Set.add kl lib))
      in
      let glob_deps = Lib_file_deps.deps_of_entries ~opaque ~cm_kind glob_libs in
      let package_deps =
        List.filter libs ~f:(fun lib -> not (Lib.Set.mem kept_libs lib))
        |> strict_package_deps_markers cctx
      in
      let+ include_flags =
        Compilation_context.filtered_include_flags cctx ~cm_kind ~kept_libs
      in
      include_flags, Dep.Set.union package_deps (Dep.Set.union tight_deps glob_deps)
;;

let lib_cm_deps ~cctx ~cm_kind ~ml_kind ~mode m =
  let obj_dir = Compilation_context.obj_dir cctx in
  let opaque = Compilation_context.opaque cctx in
  let for_ = Compilation_context.for_ cctx in
  let dep_graph = Ml_kind.Dict.get (Compilation_context.dep_graphs cctx) ml_kind in
  Action_builder.dyn_deps
    (lib_deps_for_module
       ~cctx
       ~obj_dir
       ~for_
       ~dep_graph
       ~opaque
       ~cm_kind
       ~ml_kind
       ~mode
       m)
;;

(* Arguments for the compiler to prevent it from being too clever.

   The compiler creates the cmi when it thinks a .ml file has no corresponding
   .mli. However this behavior is a bit racy and doesn't work well when the
   extension is not .ml or when the .ml and .mli are in different directories.
   This flags makes the compiler think there is a .mli file and will the read
   the cmi file rather than create it. *)
let force_read_cmi ~obj_dir ~version ~src ~cm_kind module_ : _ Command.Args.t =
  let cmi_file =
    Obj_dir.Module.cm_file_exn obj_dir module_ ~kind:(Lib_mode.Cm_kind.cmi cm_kind)
    |> Path.build
  in
  match Version.supports_cmi_file version, Lib_mode.of_cm_kind cm_kind with
  | true, Ocaml _ -> S [ A "-cmi-file"; Dep cmi_file ]
  | _ ->
    S
      [ As [ "-intf-suffix"; Filename.Extension.Or_empty.to_string (Path.extension src) ]
      ; Hidden_deps (Dep.Set.of_files [ cmi_file ])
      ]
;;

(* Build the cm* if the corresponding source is present, in the case of cmi if
   the mli is not present it is added as additional target to the .cmo
   generation *)

let opens modules m =
  Command.Args.As (Modules.With_vlib.local_open modules m |> Ocaml_flags.open_flags)
;;

let as_parameter_arg m =
  Command.Args.As (if Module.kind m = Parameter then [ "-as-parameter" ] else [])
;;

let as_argument_for cctx m =
  Command.Args.dyn
    (let open Action_builder.O in
     let impl = Compilation_context.implements cctx in
     Virtual_rules.implements_parameter impl m
     |> Resolve.Memo.read
     >>| function
     | None -> []
     | Some parameter -> [ "-as-argument-for"; Module_name.to_string parameter ])
;;

let parameters cctx =
  Command.Args.dyn
    (let open Action_builder.O in
     Resolve.Memo.read (Compilation_context.parameters cctx)
     >>| List.concat_map ~f:(fun m -> [ "-parameter"; Module_name.to_string m ]))
;;

let add_rule ?mode ?loc ~can_go_in_shared_cache sctx ~dir build =
  let build =
    if can_go_in_shared_cache
    then
      Action_builder.With_targets.map
        build
        ~f:(Action.Full.add_can_go_in_shared_cache true)
    else build
  in
  Super_context.add_rule sctx ?mode ?loc ~dir build
;;

let other_cm_files
      ~opaque
      ~cm_kind
      ~obj_dir
      ~cms_cmt_dependency
      ~bin_annot
      ~bin_annot_cms
      ~is_ox
  =
  List.concat_map ~f:(fun m ->
    let cmi_kind = Lib_mode.Cm_kind.cmi cm_kind in
    let deps = [ Path.build (Obj_dir.Module.cm_file_exn obj_dir m ~kind:cmi_kind) ] in
    let deps =
      if Module.has m ~ml_kind:Impl && cm_kind = Ocaml Cmx && not opaque
      then (
        let cmx = Obj_dir.Module.cm_file_exn obj_dir m ~kind:(Ocaml Cmx) in
        Path.build cmx :: deps)
      else if Module.has m ~ml_kind:Impl && cm_kind = Melange Cmj
      then (
        let cmj = Obj_dir.Module.cm_file_exn obj_dir m ~kind:(Melange Cmj) in
        Path.build cmj :: deps)
      else deps
    in
    (* Add .cms/.cmt dependencies when enabled. Like .cmx dependencies, these are
       skipped when -opaque is used. *)
    let cms_cmt_deps =
      let open Workspace.Context.Cms_cmt_dependency in
      match cms_cmt_dependency with
      | No_dependency -> []
      | Depends_on_cms when bin_annot_cms && is_ox && not opaque ->
        (* We pass as [cm_kind] [Ocaml Cmx/Cmi] but the specific [cm_kind]
           doesn't matter here: .cms/.cmt files are stored in byte_dir
           regardless (see [Obj_dir.Module.cms_file]). *)
        List.filter_opt
          [ Obj_dir.Module.cms_file obj_dir m ~ml_kind:Impl ~cm_kind:(Ocaml Cmx)
          ; Obj_dir.Module.cms_file obj_dir m ~ml_kind:Intf ~cm_kind:(Ocaml Cmi)
          ]
        |> List.map ~f:Path.build
      | Depends_on_cms -> []
      | Depends_on_cmt when bin_annot && is_ox && not opaque ->
        List.filter_opt
          [ Obj_dir.Module.cmt_file obj_dir m ~ml_kind:Impl ~cm_kind:(Ocaml Cmx)
          ; Obj_dir.Module.cmt_file obj_dir m ~ml_kind:Intf ~cm_kind:(Ocaml Cmi)
          ]
        |> List.map ~f:Path.build
      | Depends_on_cmt -> []
    in
    cms_cmt_deps @ deps)
;;

let cm_kind_can_go_in_shared_cache = function
  | Lib_mode.Cm_kind.Melange _ -> true
  | Lib_mode.Cm_kind.Ocaml _ -> false
;;

let copy_interface ~sctx ~dir ~obj_dir ~cm_kind m =
  (* symlink the .cmi into the public interface directory *)
  Memo.when_
    (Module.visibility m <> Visibility.Private
     && Obj_dir.need_dedicated_public_dir obj_dir)
    (fun () ->
       let can_go_in_shared_cache = cm_kind_can_go_in_shared_cache cm_kind in
       let cmi_kind = Lib_mode.Cm_kind.cmi cm_kind in
       add_rule
         ~can_go_in_shared_cache
         sctx
         ~dir
         (Action_builder.symlink
            ~src:(Path.build (Obj_dir.Module.cm_file_exn obj_dir m ~kind:cmi_kind))
            ~dst:(Obj_dir.Module.cm_public_file_exn obj_dir m ~kind:cmi_kind)))
;;

let melange_js_basename m =
  match Module.file ~ml_kind:Impl m with
  | Some s ->
    (* we aren't using Filename.extension because we want to handle
       filenames such as foo.pp.ml *)
    (match String.lsplit2 (Path.basename s |> Filename.to_string) ~on:'.' with
     | None ->
       Code_error.raise
         "could not extract module name from file path"
         [ "module", Module.to_dyn m ]
     | Some (module_name, _) -> Filename.of_string_exn module_name)
  | None ->
    Code_error.raise
      "could not find melange source from module"
      [ "module", Module.to_dyn m ]
;;

let melange_args (cctx : Compilation_context.t) (cm_kind : Lib_mode.Cm_kind.t) module_ =
  match cm_kind with
  | Ocaml (Cmi | Cmo | Cmx) | Melange Cmi -> []
  | Melange Cmj ->
    let melange_cli =
      let scope = Compilation_context.scope cctx in
      let dune_project = Scope.project scope in
      Melange.Cli.of_project dune_project
    in
    let mel_package_name, mel_package_output =
      let package_output =
        Module.source ~ml_kind:Impl module_
        |> Option.value_exn
        |> Module.File.original_path
        |> Path.parent_exn
      in
      match Compilation_context.melange_package_name cctx with
      | None -> [], package_output
      | Some lib_name ->
        let dir =
          let package_output = Path.as_in_build_dir_exn package_output in
          let lib_root_dir = Path.build (Compilation_context.dir cctx) in
          let src_dir = Path.build package_output in
          let build_dir =
            Compilation_context.super_context cctx
            |> Super_context.context
            |> Context.build_dir
          in
          Path.drop_prefix_exn src_dir ~prefix:lib_root_dir
          |> Path.Local.to_string
          |> Path.Build.relative build_dir
        in
        ( [ Command.Args.A melange_cli.package_name; A (Lib_name.to_string lib_name) ]
        , Path.build dir )
    in
    Command.Args.A melange_cli.stop_after_cmj
    :: A melange_cli.package_output
    :: Command.Args.Path mel_package_output
    :: A melange_cli.module_name
    :: A (melange_js_basename module_ |> Filename.to_string)
    :: mel_package_name
;;

let build_cm cctx ~force_write_cmi ~precompiled_cmi ~cm_kind (m : Module.t) =
  if force_write_cmi && precompiled_cmi
  then Code_error.raise "force_write_cmi and precompiled_cmi are mutually exclusive" [];
  let sctx = Compilation_context.super_context cctx in
  let dir = Compilation_context.dir cctx in
  let obj_dir = Compilation_context.obj_dir cctx in
  let ctx = Super_context.context sctx in
  let mode = Lib_mode.of_cm_kind cm_kind in
  let sandbox =
    match Module.kind m with
    | Root ->
      (* This is need to guarantee that no local modules shadow the modules
         referenced by the root module *)
      Sandbox_config.needs_sandboxing
    | _ -> Compilation_context.sandbox cctx
  in
  let ocaml = Compilation_context.ocaml cctx in
  let* compiler =
    match mode with
    | Melange ->
      let loc = Compilation_context.loc cctx in
      let+ melc = Melange_binary.melc sctx ~loc ~dir in
      Some melc
    | Ocaml mode ->
      Memo.return
        (let compiler = Ocaml_toolchain.compiler ocaml mode in
         (* TODO one day remove this silly optimization *)
         match compiler with
         | Ok _ as s -> Some s
         | Error _ -> None)
  in
  (let open Option.O in
   let* compiler = compiler in
   let ml_kind = Lib_mode.Cm_kind.source cm_kind in
   let+ src = Module.file m ~ml_kind in
   let original = Module.source_without_pp m ~ml_kind in
   let dst = Obj_dir.Module.cm_file_exn obj_dir m ~kind:cm_kind in
   let obj =
     Obj_dir.Module.obj_file obj_dir m ~kind:(Ocaml Cmx) ~ext:ocaml.lib_config.ext_obj
   in
   let open Memo.O in
   let* (extra_args : _ Command.Args.t) =
     if precompiled_cmi
     then
       (* CR-someday Alizter: We should use the correct precompiled cmi here.
          Currently, we don't have easy access to it. *)
       Memo.return
         (Command.Args.As
            [ "-intf-suffix"; Filename.Extension.Or_empty.to_string (Path.extension src) ])
     else (
       (* If we're compiling an implementation, then the cmi is present *)
       let public_vlib_module = Module.kind m = Impl_vmodule in
       match cm_kind, Module.file m ~ml_kind:Intf, public_vlib_module with
       (* If there is no mli, [ocamlY -c file.ml] produces both the .cmY and
          .cmi. We choose to use ocamlc to produce the cmi and to produce the
          cmx we have to wait to avoid race conditions. *)
       | (Ocaml Cmo | Melange Cmj), None, false ->
         if force_write_cmi
         then Memo.return (Command.Args.As [ "-intf-suffix"; ".dummy-ignore-mli" ])
         else
           let+ () = copy_interface ~dir ~obj_dir ~sctx ~cm_kind m in
           let cmi_kind = Lib_mode.Cm_kind.cmi cm_kind in
           Command.Args.Hidden_targets
             [ Obj_dir.Module.cm_file_exn obj_dir m ~kind:cmi_kind ]
       | (Ocaml Cmo | Melange Cmj), None, true | (Ocaml (Cmo | Cmx) | Melange Cmj), _, _
         -> Memo.return (force_read_cmi ~obj_dir ~version:ocaml.version ~cm_kind ~src m)
       | (Ocaml Cmi | Melange Cmi), _, _ ->
         let+ () = copy_interface ~dir ~obj_dir ~sctx ~cm_kind m in
         Command.Args.empty)
   in
   let other_targets =
     match cm_kind with
     | Ocaml (Cmi | Cmo) | Melange (Cmi | Cmj) -> Command.Args.empty
     | Ocaml Cmx -> Hidden_targets [ obj ]
   in
   let opaque = Compilation_context.opaque cctx in
   let skip_lib_deps =
     match Module.kind m with
     | Alias _ ->
       not (Modules.With_vlib.is_stdlib_alias (Compilation_context.modules cctx) m)
     | Wrapped_compat -> true
     | Intf_only | Virtual | Impl | Impl_vmodule | Root | Parameter -> false
   in
   let lib_cm_args =
     if skip_lib_deps
     then
       Action_builder.return
         (Lib_mode.Cm_kind.Map.get (Compilation_context.includes cctx) cm_kind)
     else lib_cm_deps ~cctx ~cm_kind ~ml_kind ~mode m
   in
   let other_cm_files =
     let dep_graph = Ml_kind.Dict.get (Compilation_context.dep_graphs cctx) ml_kind in
     let module_deps = Dep_graph.deps_of dep_graph m in
     let cms_cmt_dependency = Compilation_context.cms_cmt_dependency cctx in
     let bin_annot = Compilation_context.bin_annot cctx in
     let bin_annot_cms = Compilation_context.bin_annot_cms cctx in
     let is_ox = Ocaml_config.ox ocaml.ocaml_config in
     Action_builder.dyn_paths_unit
       (Action_builder.map
          module_deps
          ~f:
            (other_cm_files
               ~opaque
               ~cm_kind
               ~obj_dir
               ~cms_cmt_dependency
               ~bin_annot
               ~bin_annot_cms
               ~is_ox))
   in
   let cmt_args =
     match cm_kind with
     | Ocaml Cmx -> Command.Args.empty
     | Ocaml (Cmi | Cmo) | Melange (Cmi | Cmj) ->
       if Compilation_context.bin_annot cctx
       then (
         let fn =
           Option.value_exn (Obj_dir.Module.cmt_file obj_dir m ~cm_kind ~ml_kind)
         in
         let annots =
           [ "-bin-annot" ]
           @
           if Version.supports_bin_annot_occurrences ocaml.version
           then [ "-bin-annot-occurrences" ]
           else []
         in
         S [ Hidden_targets [ fn ]; As annots ])
       else Command.Args.empty
   in
   let cms_args =
     match cm_kind with
     | Ocaml Cmx | Melange _ -> Command.Args.empty
     | Ocaml (Cmi | Cmo) ->
       if Compilation_context.bin_annot_cms cctx && Ocaml_config.ox ocaml.ocaml_config
       then (
         match Obj_dir.Module.cms_file obj_dir m ~cm_kind ~ml_kind with
         | None -> Command.Args.empty
         | Some fn -> S [ Hidden_targets [ fn ]; As [ "-bin-annot-cms" ] ])
       else Command.Args.empty
   in
   let opaque_arg : _ Command.Args.t =
     let intf_only = cm_kind = Ocaml Cmi && not (Module.has m ~ml_kind:Impl) in
     if opaque || (intf_only && Ocaml.Version.supports_opaque_for_mli ocaml.version)
     then A "-opaque"
     else Command.Args.empty
   in
   let flags = Command.Args.dyn (Ocaml_flags.get (Compilation_context.flags cctx) mode) in
   let pp_flags, sandbox =
     match Module.pp_flags m with
     | None -> Command.Args.empty, sandbox
     | Some (pp, sandbox') -> Command.Args.dyn pp, Sandbox_config.inter sandbox sandbox'
   in
   let opens =
     let modules = Compilation_context.modules cctx in
     opens modules m
   in
   let obj_dirs =
     Obj_dir.all_obj_dirs obj_dir ~mode
     |> List.concat_map ~f:(fun p -> [ Command.Args.A "-I"; Path (Path.build p) ])
   in
   let can_go_in_shared_cache = cm_kind_can_go_in_shared_cache cm_kind in
   add_rule
     ~can_go_in_shared_cache
     sctx
     ~dir:
       (let dune_version =
          Compilation_context.scope cctx |> Scope.project |> Dune_project.dune_version
        in
        (* TODO DUNE4 get rid of the old behavior *)
        if dune_version >= (3, 7) then dir else Context.build_dir ctx)
     ?loc:(Compilation_context.loc cctx)
     (let open Action_builder.With_targets.O in
      Action_builder.with_no_targets other_cm_files
      >>> Command.run
            ~dir:(Path.build (Context.build_dir ctx))
            ~sandbox
            ~forbid_action_runner:true
            compiler
            [ flags
            ; pp_flags
            ; cmt_args
            ; cms_args
            ; Command.Args.S obj_dirs
            ; Command.Args.Dyn lib_cm_args
            ; extra_args
            ; as_parameter_arg m
            ; as_argument_for cctx m
            ; parameters cctx
            ; S (melange_args cctx cm_kind m)
            ; A "-no-alias-deps"
            ; opaque_arg
            ; opens
            ; A "-o"
            ; Target dst
            ; A "-c"
            ; Command.Ml_kind.flag ml_kind
            ; Dep src
            ; (* We add a hidden dependency on the original, pre-PPX source
                 file, which the compiler wants to find to display error
                 location snippets. *)
              Hidden_deps (Dep.Set.of_files (Option.to_list original))
            ; other_targets
            ]))
  |> Memo.Option.iter ~f:Fun.id
;;

let build_module ?(force_write_cmi = false) ?(precompiled_cmi = false) cctx m =
  let open Memo.O in
  let build_cm = build_cm cctx m ~force_write_cmi ~precompiled_cmi in
  match Compilation_context.for_ cctx with
  | Ocaml ->
    let* () = build_cm ~cm_kind:(Ocaml Cmo)
    and* () = build_cm ~cm_kind:(Ocaml Cmx)
    and* () =
      Memo.when_ (not precompiled_cmi) (fun () -> build_cm ~cm_kind:(Ocaml Cmi))
    in
    let obj_dir = Compilation_context.obj_dir cctx in
    (match Obj_dir.Module.cm_file obj_dir m ~kind:(Ocaml Cmo) with
     | None -> Memo.return ()
     | Some src ->
       let ml_kind = Ml_kind.Impl in
       let dep_graph = Ml_kind.Dict.get (Compilation_context.dep_graphs cctx) ml_kind in
       let module_deps = Dep_graph.deps_of dep_graph m in
       Memo.parallel_iter Js_of_ocaml.Mode.all ~f:(fun mode ->
         Compilation_context.js_of_ocaml cctx
         |> Js_of_ocaml.Mode.Pair.select ~mode
         |> Memo.Option.iter ~f:(fun in_context ->
           (* Build *.cmo.js / *.wasmo *)
           let sctx = Compilation_context.super_context cctx in
           let dir = Compilation_context.dir cctx in
           let action_with_targets =
             Jsoo_rules.build_cm
               cctx
               ~dir
               ~in_context
               ~mode
               ~src:(Path.build src)
               ~obj_dir
               ~deps:module_deps
               ~config:None
           in
           Super_context.add_rule sctx ~dir action_with_targets)))
  | Melange ->
    let* () = build_cm ~cm_kind:(Melange Cmj)
    and* () =
      Memo.when_ (not precompiled_cmi) (fun () -> build_cm ~cm_kind:(Melange Cmi))
    in
    let project = Compilation_context.scope cctx |> Scope.project in
    let dir = Compilation_context.dir cctx in
    let predicate_dir =
      let obj_dir = Compilation_context.obj_dir cctx in
      Obj_dir.melange_dir obj_dir
    in
    let predicate =
      [ Lib_mode.Cm_kind.ext (Melange Cmi); Lib_mode.Cm_kind.ext (Melange Cmj) ]
      |> Glob.matching_extensions
      |> Predicate_lang.Glob.of_glob
    in
    let deps =
      File_selector.of_predicate_lang
        ~dir:(Path.build predicate_dir)
        ~only_generated_files:(Dune_project.dune_version project >= (3, 0))
        predicate
      |> Action_builder.paths_matching_unit ~loc:Loc.none
    in
    Rules.Produce.Alias.add_deps (Alias.make Alias0.all ~dir) deps
;;

let ocamlc_i_action ~deps cctx (m : Module.t) =
  let obj_dir = Compilation_context.obj_dir cctx in
  let ctx = Compilation_context.super_context cctx |> Super_context.context in
  let src = Option.value_exn (Module.file m ~ml_kind:Impl) in
  let original = Module.source_without_pp m ~ml_kind:Impl in
  let sandbox =
    match Module.kind m with
    | Root -> Sandbox_config.needs_sandboxing
    | _ -> Compilation_context.sandbox cctx
  in
  let pp_flags, sandbox =
    match Module.pp_flags m with
    | None -> Command.Args.empty, sandbox
    | Some (pp_flags, pp_sandbox) ->
      Command.Args.dyn pp_flags, Sandbox_config.inter sandbox pp_sandbox
  in
  let cm_deps =
    Action_builder.dyn_paths_unit
      (let open Action_builder.O in
       Ml_kind.Dict.get deps Impl
       >>| List.concat_map ~f:(fun m ->
         [ Path.build (Obj_dir.Module.cm_file_exn obj_dir m ~kind:(Ocaml Cmi)) ]))
  in
  let lib_cm_args =
    lib_cm_deps ~cctx ~cm_kind:(Ocaml Cmo) ~ml_kind:Impl ~mode:(Ocaml Byte) m
  in
  let ocaml_flags = Ocaml_flags.get (Compilation_context.flags cctx) (Ocaml Byte) in
  let modules = Compilation_context.modules cctx in
  let ocaml = Compilation_context.ocaml cctx in
  let open Action_builder.O in
  cm_deps
  >>> Command.run'
        (Ok ocaml.ocamlc)
        ~dir:(Path.build (Context.build_dir ctx))
        ~sandbox
        ~forbid_action_runner:true
        [ Command.Args.dyn ocaml_flags
        ; pp_flags
        ; A "-I"
        ; Path (Path.build (Obj_dir.byte_dir obj_dir))
        ; Command.Args.Dyn lib_cm_args
        ; as_parameter_arg m
        ; as_argument_for cctx m
        ; parameters cctx
        ; opens modules m
        ; A "-short-paths"
        ; A "-i"
        ; Command.Ml_kind.flag Impl
        ; Dep src
        ; Hidden_deps (Dep.Set.of_files (Option.to_list original))
        ]
;;

let ocamlc_i ~deps cctx m ~output =
  let sctx = Compilation_context.super_context cctx in
  let dir = Compilation_context.dir cctx in
  ocamlc_i_action ~deps cctx m
  |> Action_builder.with_stdout_to output
  |> Super_context.add_rule sctx ~dir
;;

let infer_interface cctx m =
  let sctx = Compilation_context.super_context cctx in
  let dir = Compilation_context.dir cctx in
  let action =
    let source_file =
      match Module.source_without_pp m ~ml_kind:Intf with
      | Some source_file -> Path.as_in_build_dir_exn source_file
      | None ->
        Module.source_without_pp m ~ml_kind:Impl
        |> Option.value_exn
        |> Path.set_extension ~ext:Filename.Extension.mli
        |> Path.as_in_build_dir_exn
    in
    let source_path = Path.build source_file in
    let open Action_builder.O in
    let+ action =
      let deps =
        let dep_graphs = Compilation_context.dep_graphs cctx in
        Ml_kind.Dict.of_func (fun ~ml_kind ->
          Dep_graph.deps_of (Ml_kind.Dict.get dep_graphs ml_kind) m)
      in
      ocamlc_i_action ~deps cctx m
    and+ () = Action_builder.paths_existing [ source_path ] in
    Action.Full.map action ~f:(fun action ->
      let correction_file =
        Path.Build.extend_basename source_file ~suffix:Filename.corrected
      in
      Action.progn
        [ Action.with_stdout_to correction_file action
        ; Action.diff ~optional:true source_path correction_file
        ])
  in
  Super_context.execute_action_stdout
    sctx
    ~loc:(Option.value (Compilation_context.loc cctx) ~default:Loc.none)
    ~dir
    action
  >>| fun (_ : string) -> ()
;;

module Alias_module = struct
  (* The alias module is an implementation detail to support wrapping library
     modules under a single toplevel name. Since OCaml doesn't have proper
     support for namespacing at the moment, in order to expose module `X` of
     library `foo` as `Foo.X`, Dune does the following:

     - it compiles x.ml to Foo__X.cmo, Foo__X.cmx, Foo__X.o, ... - it implicitly
       exposes a module alias [module X = Foo__X] to all the modules of the `foo`
       library

     The second point is achieved by implicitly opening a module containing such
     aliases for all modules of `foo` when compiling modules of `foo` via the
     `-open` option of the compiler. This module is called the alias module and
     is implicitly generated by Dune.*)

  module Literals = struct
    let header = "(* generated by dune *)\n"
    let canonical_prefix = "\n(** @canonical "
    let canonical_path_separator = '.'
    let canonical_suffix = " *)\nmodule "
    let alias_separator = " = "
    let alias_suffix = "\n"
    let shadowed_prefix = "\nmodule "
    let shadowed_definition_suffix = " = struct end\n"
    let shadowed_deprecation = "[@@deprecated \"this module is shadowed\"]\n"
  end

  module Alias = struct
    type t =
      { local_name : Module_name.t
      ; canonical_path : Module_name.Path.t
      ; obj_name : Module_name.Unique.t
      }

    let add_canonical_path builder (name :: names : Module_name.Path.t) =
      let open Literals in
      String_builder.add_string builder (Module_name.to_string name);
      List.iter names ~f:(fun name ->
        String_builder.add_char builder canonical_path_separator;
        String_builder.add_string builder (Module_name.to_string name))
    ;;

    let length =
      let static_length =
        let open Literals in
        String.length canonical_prefix
        + String.length canonical_suffix
        + String.length alias_separator
        + String.length alias_suffix
      in
      let module_name_length name =
        let name = Module_name.to_string name in
        String.length name
      in
      let canonical_path_length (name :: names : Module_name.Path.t) =
        let init = module_name_length name in
        List.fold_left names ~init ~f:(fun length name ->
          (* The [+ 1] accounts for the ['.'] between path components. *)
          length + 1 + module_name_length name)
      in
      fun { canonical_path; local_name; obj_name } ->
        static_length
        + canonical_path_length canonical_path
        + module_name_length local_name
        + String.length (Module_name.Unique.to_string obj_name)
    ;;

    let add builder { canonical_path; local_name; obj_name } =
      let open Literals in
      let obj_name = Module_name.Unique.to_name ~loc:Loc.none obj_name in
      String_builder.add_string builder canonical_prefix;
      add_canonical_path builder canonical_path;
      String_builder.add_string builder canonical_suffix;
      String_builder.add_string builder (Module_name.to_string local_name);
      String_builder.add_string builder alias_separator;
      String_builder.add_string builder (Module_name.to_string obj_name);
      String_builder.add_string builder alias_suffix
    ;;
  end

  type t =
    { aliases : Alias.t list
    ; shadowed : Module_name.t list
    ; instances : Parameterised_instances.t
    }

  let add_shadowed builder shadowed =
    let open Literals in
    String_builder.add_string builder shadowed_prefix;
    String_builder.add_string builder (Module_name.to_string shadowed);
    String_builder.add_string builder shadowed_definition_suffix;
    String_builder.add_string builder shadowed_deprecation
  ;;

  let to_ml =
    let static_length =
      let open Literals in
      String.length shadowed_prefix
      + String.length shadowed_definition_suffix
      + String.length shadowed_deprecation
    in
    let shadowed_length shadowed =
      let name_length = String.length (Module_name.to_string shadowed) in
      static_length + name_length
    in
    let total_length { aliases; shadowed; instances } =
      let length = String.length Literals.header in
      let length =
        List.fold_left aliases ~init:length ~f:(fun length alias ->
          length + Alias.length alias)
      in
      let length =
        List.fold_left shadowed ~init:length ~f:(fun length shadowed ->
          length + shadowed_length shadowed)
      in
      length + Parameterised_instances.ml_source_length instances
    in
    fun ({ aliases; shadowed; instances } as t) ->
      let builder = String_builder.create (total_length t) in
      String_builder.add_string builder Literals.header;
      List.iter aliases ~f:(Alias.add builder);
      List.iter shadowed ~f:(add_shadowed builder);
      Parameterised_instances.add_ml_source builder instances;
      String_builder.build_exact_exn builder
  ;;

  let of_modules project modules group instances =
    let aliases =
      Modules.Group.for_alias group
      |> List.map ~f:(fun (local_name, m) ->
        let canonical_path = Modules.With_vlib.canonical_path modules group m in
        let obj_name = Module.obj_name m in
        { Alias.local_name; canonical_path; obj_name })
    in
    let shadowed =
      if Dune_project.dune_version project < (3, 5)
      then []
      else (
        let lib_interface = Modules.Group.lib_interface group in
        match Module.kind lib_interface with
        | Alias _ -> []
        | _ -> [ Module.name (Modules.Group.alias group) ])
    in
    { aliases; shadowed; instances }
  ;;
end

let build_alias_module cctx group =
  let alias_module = Modules.Group.alias group in
  let* () =
    let alias_file =
      let open Action_builder.O in
      let+ instances = Compilation_context.instances cctx in
      let project = Compilation_context.scope cctx |> Scope.project in
      let modules = Compilation_context.modules cctx in
      Alias_module.of_modules project modules group instances |> Alias_module.to_ml
    in
    let dir = Compilation_context.dir cctx in
    let sctx = Compilation_context.super_context cctx in
    Super_context.add_rule
      ~loc:Loc.none
      sctx
      ~dir
      (let file = Option.value_exn (Module.file alias_module ~ml_kind:Impl) in
       Action_builder.write_file_dyn (Path.as_in_build_dir_exn file) alias_file)
  in
  let cctx = Compilation_context.for_alias_module cctx alias_module in
  build_module cctx alias_module
;;

let root_source entries =
  let b = Buffer.create 128 in
  List.iter entries ~f:(fun name ->
    Printf.bprintf
      b
      "module %s = %s\n"
      (Module_name.to_string name)
      (Module_name.to_string name));
  Buffer.contents b
;;

let build_root_module cctx root_module =
  let for_ = Compilation_context.for_ cctx in
  let sctx = Compilation_context.super_context cctx in
  let entries =
    match Compilation_context.user_written_requires cctx with
    | Some requires_compile -> Root_module.entries sctx ~requires_compile ~for_
    | None -> Code_error.raise "root module without user-written dependencies" []
  in
  let cctx = Compilation_context.for_root_module cctx root_module in
  let file = Option.value_exn (Module.file root_module ~ml_kind:Impl) in
  let dir = Compilation_context.dir cctx in
  let* () =
    Super_context.add_rule
      ~loc:Loc.none
      sctx
      ~dir
      (let target = Path.as_in_build_dir_exn file in
       Action_builder.write_file_dyn
         target
         (let open Action_builder.O in
          let+ entries = entries in
          root_source entries))
  in
  build_module cctx root_module
;;

let build_all cctx =
  let for_wrapped_compat = lazy (Compilation_context.for_wrapped_compat cctx) in
  let modules = Compilation_context.modules cctx in
  Memo.parallel_iter
    (Modules.With_vlib.fold_no_vlib_with_aliases
       modules
       ~init:[]
       ~normal:(fun x acc -> `Normal x :: acc)
       ~alias:(fun group acc -> `Alias group :: acc))
    ~f:(function
      | `Alias group -> build_alias_module cctx group
      | `Normal m ->
        (match Module.kind m with
         | Alias _ -> assert false
         | Root -> build_root_module cctx m
         | Wrapped_compat ->
           let cctx = Lazy.force for_wrapped_compat in
           build_module cctx m
         | _ ->
           let cctx =
             if Modules.With_vlib.is_stdlib_alias modules m
             then
               (* XXX it would probably be simpler if the flags were just for this
                  module in the definition of the stanza *)
               Compilation_context.for_alias_module cctx m
             else cctx
           in
           build_module cctx m))
;;

let with_empty_intf ~sctx ~dir module_ =
  let name =
    Module.file module_ ~ml_kind:Impl
    |> Option.value_exn
    |> Path.set_extension ~ext:Filename.Extension.mli
  in
  let rule =
    Action_builder.write_file
      (Path.as_in_build_dir_exn name)
      "(* Auto-generated by Dune *)"
  in
  let+ () = Super_context.add_rule sctx ~dir rule in
  Module.add_file module_ Ml_kind.Intf (Module.File.make Dialect.ocaml name)
;;
