open Import

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

  let get sctx (t : Lib.t) (mode : Link_mode.t) =
    let open Memo.O in
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
      | Byte | Byte_for_jsoo -> Memo.return dll_files
      | Byte_with_stubs_statically_linked_in ->
        Memo.return @@ select_lib_files Mode.Byte
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
        | Byte | Byte_for_jsoo -> dll_files
        | Byte_with_stubs_statically_linked_in | Native ->
          select_lib_files Mode.Native
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
      | Some m -> (
        let obj_name =
          Path.relative (Lib_info.src_dir info) (Module_name.uncapitalize m)
        in
        match mode with
        | Byte_for_jsoo | Byte | Byte_with_stubs_statically_linked_in ->
          Path.extend_basename obj_name ~suffix:(Cm_kind.ext Cmo) :: hidden_deps
        | Native ->
          Path.extend_basename obj_name ~suffix:(Cm_kind.ext Cmx)
          :: Path.extend_basename obj_name ~suffix:(Lib.lib_config t).ext_obj
          :: hidden_deps)
    in
    { deps; hidden_deps; include_dirs }
end

let link_deps sctx t mode =
  let open Memo.O in
  let+ x = Link_params.get sctx t mode in
  List.rev_append x.hidden_deps x.deps

module L = struct
  type nonrec t = Lib.t list

  let to_iflags dirs =
    Command.Args.S
      (Path.Set.fold dirs ~init:[] ~f:(fun dir acc ->
           Command.Args.Path dir :: A "-I" :: acc)
      |> List.rev)

  let remove_stdlib dirs libs =
    match libs with
    | [] -> dirs
    | lib :: _ -> Path.Set.remove dirs (Lib.lib_config lib).stdlib_dir

  let include_paths =
    let add_public_dir ~visible_cmi obj_dir acc mode =
      match visible_cmi with
      | false -> acc
      | true ->
        let public_cmi_dir =
          (match mode with
          | `Byte -> Obj_dir.public_cmi_ocaml_dir
          | `Melange -> Obj_dir.public_cmi_melange_dir)
            obj_dir
        in
        Path.Set.add acc public_cmi_dir
    in
    fun ?project ts mode ->
      let visible_cmi =
        match project with
        | None -> fun _ -> true
        | Some project -> (
          let check_project lib =
            match Lib.project lib with
            | None -> false
            | Some project' -> Dune_project.equal project project'
          in
          fun lib ->
            match Lib_info.status (Lib.info lib) with
            | Private (_, Some _) | Installed_private -> check_project lib
            | _ -> true)
      in
      let dirs =
        List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
            let obj_dir = Lib_info.obj_dir (Lib.info t) in
            match mode with
            | Lib_mode.Ocaml mode -> (
              let acc =
                add_public_dir ~visible_cmi:(visible_cmi t) obj_dir acc `Byte
              in
              match mode with
              | Byte -> acc
              | Native ->
                let native_dir = Obj_dir.native_dir obj_dir in
                Path.Set.add acc native_dir)
            | Melange ->
              add_public_dir ~visible_cmi:(visible_cmi t) obj_dir acc `Melange)
      in
      remove_stdlib dirs ts

  let include_flags ?project ts mode =
    to_iflags (include_paths ?project ts mode)

  let c_include_paths ts =
    let dirs =
      List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
          let src_dir = Lib_info.src_dir (Lib.info t) in
          Path.Set.add acc src_dir)
    in
    remove_stdlib dirs ts

  let c_include_flags ts = to_iflags (c_include_paths ts)

  let toplevel_include_paths ts =
    let with_dlls =
      List.filter ts ~f:(fun t ->
          match Lib_info.foreign_dll_files (Lib.info t) with
          | [] -> false
          | _ -> true)
    in
    Path.Set.union
      (include_paths ts (Lib_mode.Ocaml Byte))
      (c_include_paths with_dlls)
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
           Action_builder.all
             (List.map ts ~f:(function
               | Lib t ->
                 let+ p =
                   Action_builder.of_memo (Link_params.get sctx t mode)
                 in
                 Command.Args.S
                   (Deps p.deps
                   :: Hidden_deps (Dep.Set.of_files p.hidden_deps)
                   :: List.map p.include_dirs ~f:(fun dir ->
                          Command.Args.S [ A "-I"; Path dir ]))
               | Module (obj_dir, m) ->
                 Action_builder.return
                   (Command.Args.S
                      (Dep
                         (Obj_dir.Module.cm_file_exn obj_dir m
                            ~kind:(Ocaml (Mode.cm_kind (Link_mode.mode mode))))
                      ::
                      (match mode with
                      | Native ->
                        [ Command.Args.Hidden_deps
                            (Dep.Set.of_files
                               [ Obj_dir.Module.o_file_exn obj_dir m
                                   ~ext_obj:lib_config.ext_obj
                               ])
                        ]
                      | Byte
                      | Byte_for_jsoo
                      | Byte_with_stubs_statically_linked_in -> [])))))
         in
         Command.Args.S l)
  end
end
