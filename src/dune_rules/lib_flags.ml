open Import
open Memo.O

module Link_params = struct
  type t =
    { include_dirs : Path.t list
    ; deps : Path.t list
      (* List of files that will be read by the compiler at link time and
           appear directly on the command line *)
    ; hidden_deps : Path.t list
      (* List of files that will be read by the compiler at link time but do
       not appear on the command line *)
    }

  let get sctx (t : Lib.t) (mode : Link_mode.t) (lib_config : Lib_config.t) =
    let info = Lib.info t in
    let lib_files = Lib_info.foreign_archives info
    and dll_files = Lib_info.foreign_dll_files info in
    (* OCaml library archives [*.cma] and [*.cmxa] are directly listed in the
       command line. *)
    let deps = Mode.Dict.get (Lib_info.archives info) (Link_mode.mode mode) in
    (* Foreign archives [lib*.a] and [dll*.so] and native archives [lib*.a] are
       declared as hidden dependencies, and appropriate [-I] flags are provided
       separately to help the linker locate them. *)
    let select_lib_files = Mode.Map.Multi.for_only ~and_all:true lib_files in
    let+ hidden_deps =
      match mode with
      | Byte -> Memo.return dll_files
      | Byte_for_jsoo -> Memo.return []
      | Byte_with_stubs_statically_linked_in -> Memo.return @@ select_lib_files Mode.Byte
      | Native ->
        let+ native_archives =
          let+ modules = Dir_contents.modules_of_lib sctx t in
          Lib_info.eval_native_archives_exn info ~modules
        in
        let lib_files = select_lib_files Mode.Native in
        List.rev_append native_archives lib_files
    in
    let include_dirs =
      let files =
        match mode with
        | Byte -> dll_files
        | Byte_for_jsoo -> []
        | Byte_with_stubs_statically_linked_in | Native -> select_lib_files Mode.Native
      in
      let files =
        match Lib_info.exit_module info with
        | None -> files
        | Some _ ->
          (* The exit module is copied next to the archive, so we add the
             archive here so that its directory ends up in [include_dirs]. *)
          files @ deps
      in
      (* TODO: Remove the below unsafe call to [parent_exn] by separating files
         and directories at the type level. Then any file will have a
         well-defined parent directory, possibly ".". *)
      let dirs = List.map files ~f:Path.parent_exn in
      List.sort_uniq dirs ~compare:Path.compare
    in
    let hidden_deps =
      match Lib_info.exit_module info with
      | None -> hidden_deps
      | Some m ->
        let obj_name =
          Path.relative (Lib_info.src_dir info) (Module_name.uncapitalize m)
        in
        (match mode with
         | Byte_for_jsoo | Byte | Byte_with_stubs_statically_linked_in ->
           Path.extend_basename obj_name ~suffix:(Cm_kind.ext Cmo) :: hidden_deps
         | Native ->
           Path.extend_basename obj_name ~suffix:(Cm_kind.ext Cmx)
           :: Path.extend_basename obj_name ~suffix:lib_config.ext_obj
           :: hidden_deps)
    in
    { deps; hidden_deps; include_dirs }
  ;;
end

let link_deps sctx t mode lib_config =
  let+ x = Link_params.get sctx t mode lib_config in
  List.rev_append x.hidden_deps x.deps
;;

module L = struct
  type nonrec t = Lib.t list

  let to_flags flag dirs =
    Command.Args.S
      (Path.Set.fold dirs ~init:[] ~f:(fun dir acc ->
         Command.Args.Path dir :: A flag :: acc)
       |> List.rev)
  ;;

  let to_iflags dir = to_flags "-I" dir
  let to_hflags dir = to_flags "-H" dir

  let remove_stdlib dirs (lib_config : Lib_config.t) =
    Path.Set.remove dirs lib_config.stdlib_dir
  ;;

  type mode =
    { lib_mode : Lib_mode.t
    ; melange_emit : bool
    }

  let include_paths =
    let add_public_dir ~visible_cmi obj_dir acc mode =
      match visible_cmi with
      | false -> acc
      | true ->
        let public_cmi_dirs =
          List.map
            ~f:(fun f -> f obj_dir)
            (match mode with
             | { lib_mode = Ocaml _; _ } -> [ Obj_dir.public_cmi_ocaml_dir ]
             | { lib_mode = Melange; melange_emit = false } ->
               [ Obj_dir.public_cmi_melange_dir ]
             | { lib_mode = Melange; melange_emit = true } ->
               (* Add the dir where `.cmj` files exist, even for installed
                  private libraries. Melange needs to query `.cmj` files for
                  `import` information *)
               [ Obj_dir.melange_dir; Obj_dir.public_cmi_melange_dir ])
        in
        List.fold_left public_cmi_dirs ~init:acc ~f:Path.Set.add
    in
    fun ?project ts mode lib_config ->
      let visible_cmi =
        match project with
        | None -> fun _ -> true
        | Some project ->
          let check_project lib =
            match Lib.project lib with
            | None -> false
            | Some project' -> Dune_project.equal project project'
          in
          fun lib ->
            (match Lib_info.status (Lib.info lib) with
             | Private (_, Some _) | Installed_private -> check_project lib
             | _ -> true)
      in
      let dirs =
        List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
          let obj_dir = Lib_info.obj_dir (Lib.info t) in
          let visible_cmi = visible_cmi t in
          match mode.lib_mode with
          | Melange -> add_public_dir ~visible_cmi obj_dir acc mode
          | Ocaml ocaml_mode ->
            let acc = add_public_dir ~visible_cmi obj_dir acc mode in
            (match ocaml_mode with
             | Byte -> acc
             | Native ->
               let native_dir = Obj_dir.native_dir obj_dir in
               Path.Set.add acc native_dir))
      in
      remove_stdlib dirs lib_config
  ;;

  let include_flags ?project ~direct_libs ~hidden_libs mode lib_config =
    let include_paths ts =
      include_paths ?project ts { lib_mode = mode; melange_emit = false }
    in
    let hidden_includes = to_hflags (include_paths hidden_libs lib_config) in
    let direct_includes = to_iflags (include_paths direct_libs lib_config) in
    Command.Args.S [ direct_includes; hidden_includes ]
  ;;

  let melange_emission_include_flags ?project ts lib_config =
    to_iflags
      (include_paths ?project ts { lib_mode = Melange; melange_emit = true } lib_config)
  ;;

  let include_paths ?project ts mode =
    include_paths ?project ts { lib_mode = mode; melange_emit = false }
  ;;

  let c_include_paths ts lib_config =
    let dirs =
      List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
        let src_dir = Lib_info.src_dir (Lib.info t) in
        Path.Set.add acc src_dir)
    in
    (* I don't remember why this is being done anymore. Anyone else has a clue? *)
    remove_stdlib dirs lib_config
  ;;

  let c_include_flags ts sctx =
    let local, external_ =
      List.fold_left ts ~init:([], Dep.Set.empty) ~f:(fun (local, external_) lib ->
        let info = Lib.info lib in
        match Lib_info.public_headers info with
        | External paths -> local, Dep.Set.union external_ (Dep.Set.of_files paths)
        | Local (_loc, public_headers) ->
          let dir = Path.as_in_build_dir_exn @@ Lib_info.src_dir info in
          let headers =
            let+ expander = Super_context.expander sctx ~dir in
            let deps, sandbox = Dep_conf_eval.unnamed ~expander public_headers in
            assert (Sandbox_config.equal sandbox Sandbox_config.no_special_requirements);
            deps
          in
          headers :: local, external_)
    in
    let local =
      let open Action_builder.O in
      let* bindings = Action_builder.of_memo @@ Memo.all_concurrently local in
      let+ () = Action_builder.all_unit bindings in
      Command.Args.empty
    in
    let include_flags =
      let open Action_builder.O in
      let+ lib_config =
        Action_builder.of_memo
          Memo.O.(
            let+ ocaml = Super_context.context sctx |> Context.ocaml in
            ocaml.lib_config)
      in
      to_iflags (c_include_paths ts lib_config)
    in
    Command.Args.S [ Dyn local; Hidden_deps external_; Dyn include_flags ]
  ;;

  let dll_dir_paths libs =
    List.fold_left libs ~init:Path.Set.empty ~f:(fun dll_file_paths lib ->
      List.fold_left
        (Lib_info.foreign_dll_files (Lib.info lib))
        ~init:dll_file_paths
        ~f:(fun dll_file_paths dll_file_path ->
          let dll_dir_path = Path.parent_exn dll_file_path in
          Path.Set.add dll_file_paths dll_dir_path))
  ;;

  let toplevel_ld_paths ts lib_config =
    let with_dlls =
      List.filter ts ~f:(fun t ->
        match Lib_info.foreign_dll_files (Lib.info t) with
        | [] -> false
        | _ -> true)
    in
    Path.Set.union (c_include_paths with_dlls lib_config) (dll_dir_paths with_dlls)
  ;;

  let toplevel_include_paths ts lib_config =
    Path.Set.union
      (include_paths ts (Lib_mode.Ocaml Byte) lib_config)
      (toplevel_ld_paths ts lib_config)
  ;;
end

module Lib_and_module = struct
  type t =
    | Lib of Lib.t
    | Module of Path.t Obj_dir.t * Module.t

  module L = struct
    type nonrec t = t list

    let link_flags sctx ts ~(lib_config : Lib_config.t) ~mode =
      let open Action_builder.O in
      Command.Args.Dyn
        (let+ l =
           Action_builder.List.map ts ~f:(function
             | Lib t ->
               let+ { Link_params.hidden_deps; include_dirs; deps } =
                 Action_builder.of_memo (Link_params.get sctx t mode lib_config)
               in
               Command.Args.S
                 (Deps deps
                  :: Hidden_deps (Dep.Set.of_files hidden_deps)
                  :: List.map include_dirs ~f:(fun dir ->
                    Command.Args.S [ A "-I"; Path dir ]))
             | Module (obj_dir, m) ->
               Command.Args.S
                 (Dep
                    (Obj_dir.Module.cm_file_exn
                       obj_dir
                       m
                       ~kind:(Ocaml (Mode.cm_kind (Link_mode.mode mode))))
                  ::
                  (match mode with
                   | Byte | Byte_for_jsoo | Byte_with_stubs_statically_linked_in -> []
                   | Native ->
                     [ Command.Args.Hidden_deps
                         ([ Obj_dir.Module.o_file_exn
                              obj_dir
                              m
                              ~ext_obj:lib_config.ext_obj
                          ]
                          |> Dep.Set.of_files)
                     ]))
               |> Action_builder.return)
         in
         Command.Args.S l)
    ;;
  end
end
