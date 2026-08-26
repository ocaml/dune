open Import
open Memo.O
module Parallel_map = Memo.Map (Module_name.Unique.Map)

module Transitive_deps_output : sig
  type t

  val empty : t
  val equal : t -> t -> bool
  val of_modules : Module.t list -> t
  val parse : modules:Modules.With_vlib.t -> t -> Module.t list

  val merge
    :  dir:Path.Build.t
    -> transitive:t Action_builder.t list
    -> immediate:Module_name.Unique.t list
    -> t Action_builder.t
end = struct
  type encoded = string
  type t = encoded

  module Merge = struct
    module Spec = struct
      type ('src, 'dst) t =
        { transitive : encoded list
        ; immediate : Module_name.Unique.t list
        }

      let name = "merge_dep_output"
      let version = 3
      let runs_process = false
      let can_run_in_action_runner = false
      let is_useful_to ~memoize:_ = true

      let bimap
            (type src dst src' dst')
            ({ transitive; immediate } : (src, dst) t)
            (_path : src -> src')
            (_target : dst -> dst')
        : (src', dst') t
        =
        { transitive; immediate }
      ;;

      let encode
            (type src dst)
            ({ transitive; immediate } : (src, dst) t)
            (_input : src -> Sexp.t)
            (_output : dst -> Sexp.t)
        : Sexp.t
        =
        List
          [ List (List.map transitive ~f:(fun s -> Sexp.Atom s))
          ; List
              (List.map
                 ~f:(fun s -> Sexp.Atom (Module_name.Unique.to_string s))
                 immediate)
          ]
      ;;

      let action { transitive; immediate } ~ectx:_ ~(eenv : Action.env) =
        Async.async (fun () ->
          let deps =
            List.fold_left
              transitive
              ~init:(Module_name.Unique.Set.of_list immediate)
              ~f:(fun set deps ->
                String.split_lines deps
                |> Module_name.Unique.Set.of_list_map
                     ~f:Module_name.Unique.of_string_unchecked
                |> Module_name.Unique.Set.union set)
            |> Module_name.Unique.Set.to_list_map ~f:Module_name.Unique.to_string
          in
          let stdout = Process.Io.out_channel eenv.stdout_to in
          List.iter deps ~f:(fun dep ->
            output_string stdout dep;
            output_char stdout '\n'))
      ;;
    end

    module Action = Action_ext.Make (Spec)

    let action ~transitive ~immediate = Action.action { transitive; immediate }
  end

  let empty = ""
  let equal = String.equal

  let of_modules modules =
    List.map modules ~f:(fun m -> Module.obj_name m |> Module_name.Unique.to_string)
    |> String.concat ~sep:"\n"
  ;;

  let parse ~modules output =
    let obj_map = Modules.With_vlib.obj_map modules in
    String.split_lines output
    |> List.filter_map ~f:(fun m ->
      let obj_name = Module_name.Unique.of_string_unchecked m in
      Module_name.Unique.Map.find obj_map obj_name
      |> Option.map ~f:Modules.Sourced_module.to_module)
  ;;

  let merge ~dir ~transitive ~immediate =
    let open Action_builder.O in
    let action =
      let+ transitive = Action_builder.all transitive in
      { Rule.Anonymous_action.action =
          Merge.action ~transitive ~immediate
          |> Action.Full.make ~sandbox:Sandbox_config.no_sandboxing
      ; loc = Loc.none
      ; dir
      }
    in
    Build_system.execute_action_stdout action |> Action_builder.of_memo
  ;;
end

module Dep_key = struct
  type t = Module_name.Unique.t * Ml_kind.t

  let ml_kind_equal (x : Ml_kind.t) (y : Ml_kind.t) =
    match x, y with
    | Ml_kind.Impl, Impl | Intf, Intf -> true
    | Impl, Intf | Intf, Impl -> false
  ;;

  let ml_kind_hash (ml_kind : Ml_kind.t) =
    match ml_kind with
    | Ml_kind.Impl -> 0
    | Intf -> 1
  ;;

  let equal = Tuple.T2.equal Module_name.Unique.equal ml_kind_equal

  let hash =
    Tuple.T2.hash
      (fun obj_name -> Module_name.Unique.to_string obj_name |> String.hash)
      ml_kind_hash
  ;;

  let to_dyn = Tuple.T2.to_dyn Module_name.Unique.to_dyn Ml_kind.to_dyn
end

let transitive_dep m =
  match Module.kind m with
  | Root | Alias _ -> None
  | _ ->
    Some (Module.obj_name m, if Module.has m ~ml_kind:Intf then Ml_kind.Intf else Impl)
;;

let ooi_deps
      ~vimpl
      ~sctx
      ~dir
      ~obj_dir
      ~vlib_obj_dir
      ~dune_version
      ~vlib_obj_map
      ~(for_ : Compilation_mode.t)
      (sourced_module : Modules.Sourced_module.t)
      ~(ml_kind : Ml_kind.t)
  =
  let m = Modules.Sourced_module.to_module sourced_module in
  let sandbox =
    if dune_version >= (3, 3) then Some Sandbox_config.needs_sandboxing else None
  in
  let+ ocaml = Context.ocaml (Super_context.context sctx) in
  let read unit =
    Ocamlobjinfo.rules ocaml ~sandbox ~dir ~units:[ unit ]
    |> Action_builder.map ~f:(function
      | [ x ] -> x
      | [] | _ :: _ -> assert false)
    |> Action_builder.memoize "ocamlobjinfo"
    |> Action_builder.map ~f:(fun (ooi : Ocamlobjinfo.t) ->
      Module_name.Unique.Set.to_list ooi.intf
      |> List.filter_map ~f:(fun dep ->
        if Module.obj_name m = dep
        then None
        else
          Module_name.Unique.Map.find vlib_obj_map dep
          |> Option.map ~f:Modules.Sourced_module.to_module))
  in
  let read_copied kind =
    Obj_dir.Module.cm_file_exn obj_dir m ~kind |> Path.build |> read
  in
  match ml_kind with
  | Intf ->
    read_copied
      (match for_ with
       | Ocaml -> Lib_mode.Cm_kind.Ocaml Cmi
       | Melange -> Melange Cmi)
  | Impl ->
    (match for_ with
     | Ocaml -> read_copied (Ocaml (Vimpl.impl_cm_kind vimpl))
     | Melange ->
       let no_deps = Action_builder.return [] in
       (match
          Obj_dir.Module.cmt_file vlib_obj_dir m ~ml_kind:Impl ~cm_kind:(Melange Cmj)
        with
        | None -> no_deps
        | Some cmt -> Action_builder.if_file_exists cmt ~then_:(read cmt) ~else_:no_deps))
;;

let wrapped_compat_deps modules m =
  let inner = Modules.compat_for_exn (Modules.With_vlib.drop_vlib modules) m in
  match Modules.With_vlib.lib_interface modules with
  | Some li -> [ li; inner ]
  | None -> [ inner ]
;;

let preprocessed_modules_of_local_lib ~sctx lib ~for_ =
  let info = Lib.Local.info lib in
  let dir = Lib_info.src_dir info in
  let* modules = Dir_contents.modules_of_local_lib sctx lib ~for_
  and* version =
    let+ ocaml = Context.ocaml (Super_context.context sctx) in
    ocaml.version
  and* preprocess =
    let* scope = Scope.DB.find_by_dir dir in
    Instrumentation.with_instrumentation
      (Lib_info.preprocess info ~for_)
      ~instrumentation_backend:(Lib.DB.instrumentation_backend (Scope.libs scope))
    |> Resolve.Memo.read_memo
  in
  Pp_spec.pped_modules preprocess version modules
;;

type imported_vlib_impl_deps =
  | Transitive
  | Immediate of { stage_copied_objects_if_cmt_missing : unit Action_builder.t Lazy.t }

type imported_vlib_deps =
  { deps_of :
      Modules.Sourced_module.t
      -> ml_kind:Ml_kind.t
      -> Module.t list Action_builder.t Memo.t
  ; impl_deps : imported_vlib_impl_deps
  }

type memoized_transitive_deps =
  { output : Transitive_deps_output.t
  ; parsed : Module.t list
  }

type transitive_deps =
  { sandbox : Sandbox_config.t
  ; modules : Modules.With_vlib.t
  ; sctx : Super_context.t
  ; dir : Path.Build.t
  ; obj_dir : Path.Build.t Obj_dir.t
  ; obj_map : Modules.Sourced_module.t Module_name.Unique.Map.t
  ; imported_vlib_deps : imported_vlib_deps option
  ; memo : (Dep_key.t, memoized_transitive_deps) Action_builder.memo Lazy.t
  }

let rec create_transitive_deps ~sandbox ~modules ~sctx ~dir ~obj_dir ~imported_vlib_deps =
  let obj_map = Modules.With_vlib.obj_map modules in
  let rec t =
    { sandbox
    ; modules
    ; sctx
    ; dir
    ; obj_dir
    ; obj_map
    ; imported_vlib_deps
    ; memo =
        lazy
          (Action_builder.create_memo
             "ocamldep transitive deps"
             ~input:(module Dep_key)
             ~cutoff:(fun x y -> Transitive_deps_output.equal x.output y.output)
             ~human_readable_description:(fun (obj_name, ml_kind) ->
               Pp.textf
                 "transitive deps of %s.%s in %s"
                 (Module_name.Unique.to_string obj_name)
                 (Ml_kind.to_string ml_kind)
                 (Path.Build.to_string dir))
             (fun (obj_name, ml_kind) ->
                let output =
                  match Module_name.Unique.Map.find obj_map obj_name with
                  | None -> Action_builder.return Transitive_deps_output.empty
                  | Some m -> transitive_deps_output_of_sourced_module t m ~ml_kind
                in
                Action_builder.map output ~f:(fun output ->
                  { output; parsed = Transitive_deps_output.parse ~modules output })))
    }
  in
  t

and transitive_deps_output_uncached t unit ~ml_kind =
  let open Action_builder.O in
  let* immediate_deps =
    Ocamldep.read_immediate_deps_of
      ~sandbox:t.sandbox
      ~sctx:t.sctx
      ~obj_dir:t.obj_dir
      ~modules:t.modules
      ~ml_kind
      unit
  in
  let transitive =
    List.filter_map immediate_deps ~f:transitive_dep
    |> List.map ~f:(fun dep ->
      Action_builder.exec_memo (Lazy.force t.memo) dep
      |> Action_builder.map ~f:(fun memoized -> memoized.output))
  in
  let immediate = List.map immediate_deps ~f:Module.obj_name in
  Transitive_deps_output.merge ~dir:t.dir ~transitive ~immediate

and transitive_deps_output_of_sourced_module
      t
      (sourced_module : Modules.Sourced_module.t)
      ~ml_kind
  =
  match sourced_module with
  | Normal m -> transitive_deps_output_uncached t m ~ml_kind
  | Imported_from_vlib m -> transitive_deps_output_of_imported_vlib t m ~ml_kind
  | Impl_of_virtual_module impl_or_vlib ->
    let m = Ml_kind.Dict.get impl_or_vlib ml_kind in
    (match ml_kind with
     | Impl -> transitive_deps_output_uncached t m ~ml_kind
     | Intf -> transitive_deps_output_of_imported_vlib t m ~ml_kind)

and transitive_deps_output_of_imported_vlib t m ~ml_kind =
  match t.imported_vlib_deps with
  | None ->
    Code_error.raise
      "imported vlib module without vlib deps"
      [ "module", Module.to_dyn m
      ; "ml_kind", Ml_kind.to_dyn ml_kind
      ; "dir", Path.Build.to_dyn t.dir
      ]
  | Some { deps_of; impl_deps } ->
    let open Action_builder.O in
    let* deps =
      Action_builder.of_memo
        (deps_of (Modules.Sourced_module.Imported_from_vlib m) ~ml_kind)
    in
    let* deps = deps in
    (match ml_kind, impl_deps with
     | Intf, _ | Impl, Transitive ->
       Transitive_deps_output.of_modules deps |> Action_builder.return
     | Impl, Immediate _ ->
       let transitive =
         List.filter_map deps ~f:(fun dep ->
           match Module.kind dep with
           | Root | Alias _ -> None
           | _ ->
             let ml_kind = if Module.has dep ~ml_kind:Impl then Ml_kind.Impl else Intf in
             Some
               (Action_builder.exec_memo (Lazy.force t.memo) (Module.obj_name dep, ml_kind)
                |> Action_builder.map ~f:(fun memoized -> memoized.output)))
       in
       let immediate = List.map deps ~f:Module.obj_name in
       Transitive_deps_output.merge ~dir:t.dir ~transitive ~immediate)
;;

let transitive_deps_of t ~ml_kind unit =
  let obj_name = Module.obj_name unit in
  let deps =
    match Module_name.Unique.Map.find t.obj_map obj_name with
    | Some _ ->
      Action_builder.exec_memo (Lazy.force t.memo) (obj_name, ml_kind)
      |> Action_builder.map ~f:(fun output -> output.parsed)
    | None ->
      transitive_deps_output_uncached t unit ~ml_kind
      |> Action_builder.map ~f:(Transitive_deps_output.parse ~modules:t.modules)
      |> Action_builder.memoize
           (sprintf
              "%s.%s.transitive-deps"
              (Module_name.Unique.to_string obj_name)
              (Ml_kind.to_string ml_kind))
  in
  match ml_kind, t.imported_vlib_deps with
  | Intf, _ | Impl, None | Impl, Some { impl_deps = Transitive; _ } -> deps
  | Impl, Some { impl_deps = Immediate { stage_copied_objects_if_cmt_missing }; _ } ->
    let open Action_builder.O in
    let+ deps = deps
    and+ () = Lazy.force stage_copied_objects_if_cmt_missing in
    deps
;;

let make_imported_vlib_deps ~obj_dir ~vimpl ~dir ~sctx ~sandbox ~for_ : imported_vlib_deps
  =
  let vlib = Vimpl.vlib vimpl in
  match Lib.Local.of_lib vlib with
  | None ->
    let vlib_obj_map = Vimpl.vlib_obj_map vimpl in
    let vlib_obj_dir = Lib.info vlib |> Lib_info.obj_dir in
    let impl_deps =
      match for_ with
      | Compilation_mode.Ocaml -> Transitive
      | Compilation_mode.Melange ->
        let stage_copied_objects_if_cmt_missing =
          lazy
            (let modules =
               Module_name.Unique.Map.values vlib_obj_map
               |> List.map ~f:Modules.Sourced_module.to_module
             in
             (* Without CMTs, stage every copied vlib object directly. Adding them as
                module-graph edges could introduce cycles that aren't in the source. *)
             let cmts =
               List.filter_map modules ~f:(fun m ->
                 Obj_dir.Module.cmt_file
                   vlib_obj_dir
                   m
                   ~ml_kind:Impl
                   ~cm_kind:(Melange Cmj))
             in
             (let open Action_builder.O in
              let* cmts_exist =
                List.map cmts ~f:Action_builder.file_exists |> Action_builder.all
              in
              if List.for_all cmts_exist ~f:Fun.id
              then Action_builder.return ()
              else (
                let files =
                  Obj_dir.Module.L.cm_files obj_dir modules ~kind:(Melange Cmi)
                  @ Obj_dir.Module.L.cm_files obj_dir modules ~kind:(Melange Cmj)
                in
                Action_builder.paths files))
             |> Action_builder.memoize "stage copied vlib objects if CMT missing")
        in
        Immediate { stage_copied_objects_if_cmt_missing }
    in
    let dune_version =
      let impl = Vimpl.impl vimpl in
      Dune_project.dune_version impl.project
    in
    { deps_of =
        ooi_deps
          ~vimpl
          ~sctx
          ~dir
          ~obj_dir
          ~vlib_obj_dir
          ~dune_version
          ~vlib_obj_map
          ~for_
    ; impl_deps
    }
  | Some lib ->
    let vlib_obj_dir =
      let info = Lib.Local.info lib in
      Lib_info.obj_dir info
    in
    let transitive_deps =
      Memo.lazy_ ~name:"transitive-module-dependencies" (fun () ->
        let+ modules = preprocessed_modules_of_local_lib ~sctx lib ~for_ in
        let modules = Modules.With_vlib.modules modules in
        create_transitive_deps
          ~sandbox
          ~modules
          ~sctx
          ~dir:(Obj_dir.dir vlib_obj_dir)
          ~obj_dir:vlib_obj_dir
          ~imported_vlib_deps:None)
    in
    let deps_of sourced_module ~ml_kind =
      let* transitive_deps = Memo.Lazy.force transitive_deps in
      let m = Modules.Sourced_module.to_module sourced_module in
      transitive_deps_of transitive_deps ~ml_kind m |> Memo.return
    in
    { deps_of; impl_deps = Transitive }
;;

let make_transitive_deps ~obj_dir ~modules ~sandbox ~impl ~dir ~sctx ~for_ =
  let imported_vlib_deps =
    match (Modules.With_vlib.split_by_lib modules).vlib with
    | [] -> None
    | _ :: _ ->
      Some
        (make_imported_vlib_deps
           ~obj_dir
           ~vimpl:(Virtual_rules.vimpl_exn impl)
           ~dir
           ~sctx
           ~sandbox
           ~for_)
  in
  ( create_transitive_deps ~sandbox ~modules ~sctx ~dir ~obj_dir ~imported_vlib_deps
  , imported_vlib_deps )
;;

let deps_of_module ~modules ~transitive_deps ~ml_kind m =
  match Module.kind m with
  | Wrapped_compat ->
    wrapped_compat_deps modules m |> Action_builder.return |> Memo.return
  | _ ->
    let deps = transitive_deps_of transitive_deps ~ml_kind m in
    Memo.return
      (match Modules.With_vlib.alias_for modules m with
       | [] -> deps
       | aliases ->
         let open Action_builder.O in
         let+ deps = deps in
         aliases @ deps)
;;

(** Tests whether a set of modules is a singleton *)
let has_single_file modules = Option.is_some @@ Modules.With_vlib.as_singleton modules

let rec deps_of
          ~modules
          ~transitive_deps
          ~imported_vlib_deps
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
      let module_ = Modules.Sourced_module.to_module sourced_module in
      if Module.has module_ ~ml_kind
      then f sourced_module
      else Memo.return (Action_builder.return [])
    in
    match m with
    | Imported_from_vlib _ ->
      (match imported_vlib_deps with
       | None -> Code_error.raise "imported vlib module without vlib deps" []
       | Some imported_vlib_deps ->
         skip_if_source_absent
           (fun sourced_module -> imported_vlib_deps.deps_of sourced_module ~ml_kind)
           m)
    | Normal _ ->
      skip_if_source_absent
        (fun sourced_module ->
           Modules.Sourced_module.to_module sourced_module
           |> deps_of_module ~modules ~transitive_deps ~ml_kind)
        m
    | Impl_of_virtual_module impl_or_vlib ->
      deps_of ~modules ~transitive_deps ~imported_vlib_deps ~ml_kind
      @@
      let m = Ml_kind.Dict.get impl_or_vlib ml_kind in
      (match ml_kind with
       | Intf -> Imported_from_vlib m
       | Impl -> Normal m))
;;

let read_transitive_deps_of_module
      ~sandbox
      ~sctx
      ~modules
      ~obj_dir
      ~impl
      ~dir
      ~for_
      ~ml_kind
      unit
  =
  match Module.kind unit with
  | Root | Alias _ -> Action_builder.return []
  | Wrapped_compat -> wrapped_compat_deps modules unit |> Action_builder.return
  | _ ->
    if has_single_file modules
    then Action_builder.return []
    else
      let open Action_builder.O in
      let transitive_deps, _imported_vlib_deps =
        make_transitive_deps ~obj_dir ~modules ~sandbox ~impl ~dir ~sctx ~for_
      in
      let+ deps = transitive_deps_of transitive_deps ~ml_kind unit in
      (match Modules.With_vlib.alias_for modules unit with
       | [] -> deps
       | aliases -> aliases @ deps)
;;

let read_immediate_deps_of ~sandbox ~sctx ~obj_dir ~modules ~ml_kind m =
  match Module.kind m with
  | Root | Alias _ -> Action_builder.return []
  | Wrapped_compat -> wrapped_compat_deps modules m |> Action_builder.return
  | _ ->
    if has_single_file modules
    then Action_builder.return []
    else Ocamldep.read_immediate_deps_of ~sandbox ~sctx ~obj_dir ~modules ~ml_kind m
;;

let read_deps_of ~sandbox ~sctx ~obj_dir ~modules ~impl ~dir ~for_ ~ml_kind m =
  if Module.has m ~ml_kind
  then
    read_transitive_deps_of_module
      ~sandbox
      ~sctx
      ~modules
      ~obj_dir
      ~impl
      ~dir
      ~for_
      ~ml_kind
      m
  else Action_builder.return []
;;

let dict_of_func_concurrently f =
  let+ impl = f ~ml_kind:Ml_kind.Impl
  and+ intf = f ~ml_kind:Ml_kind.Intf in
  Ml_kind.Dict.make ~impl ~intf
;;

let for_module ~obj_dir ~modules ~sandbox ~impl ~dir ~sctx ~for_ module_ =
  let transitive_deps, imported_vlib_deps =
    make_transitive_deps ~obj_dir ~modules ~sandbox ~impl ~dir ~sctx ~for_
  in
  dict_of_func_concurrently
    (deps_of ~modules ~transitive_deps ~imported_vlib_deps (Normal module_))
;;

let rules ~obj_dir ~modules ~sandbox ~impl ~sctx ~dir ~for_ =
  match Modules.With_vlib.as_singleton modules with
  | Some m -> Memo.return (Dep_graph.Ml_kind.dummy m)
  | None ->
    let transitive_deps, imported_vlib_deps =
      make_transitive_deps ~obj_dir ~modules ~sandbox ~impl ~dir ~sctx ~for_
    in
    dict_of_func_concurrently (fun ~ml_kind ->
      let+ per_module =
        Modules.With_vlib.obj_map modules
        |> Parallel_map.parallel_map ~f:(fun _obj_name m ->
          deps_of ~modules ~transitive_deps ~imported_vlib_deps ~ml_kind m)
      in
      Dep_graph.make ~dir ~per_module)
    |> Memo.map ~f:(Dep_graph.Ml_kind.for_module_compilation ~modules)
;;
