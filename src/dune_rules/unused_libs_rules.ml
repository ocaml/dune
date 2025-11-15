open Import
open Memo.O

let classify_libs sctx libs =
  Memo.parallel_map libs ~f:(fun lib ->
    let+ modules = Dir_contents.modules_of_lib sctx lib in
    lib, modules)
  >>| List.partition_map ~f:(fun (lib, modules) ->
    match modules with
    | Some modules ->
      let module_set =
        Modules.With_vlib.obj_map modules
        |> Module_name.Unique.Map.keys
        |> Module_name.Unique.Set.of_list
      in
      Left (lib, module_set)
    | None ->
      (match
         let archives = Lib.info lib |> Lib_info.archives in
         Mode.Dict.get archives Byte
       with
       | [] -> Left (lib, Module_name.Unique.Set.empty)
       | archive :: _ -> Right (lib, archive)))
;;

let gen_rules
      sctx
      toolchain
      loc
      ~obj_dir
      ~modules
      ~dir
      ~direct_requires
      ~allow_unused_libraries
  =
  match
    let modules =
      Modules.With_vlib.drop_vlib modules
      |> Modules.fold ~init:[] ~f:(fun m acc -> m :: acc)
    in
    let cmis = Obj_dir.Module.L.cm_files obj_dir modules ~kind:(Ocaml Cmi) in
    let cmos = Obj_dir.Module.L.cm_files obj_dir modules ~kind:(Ocaml Cmo) in
    cmis @ cmos
  with
  | [] -> Memo.return ()
  | units ->
    let action =
      let open Action_builder.O in
      let build_dir = Obj_dir.dir obj_dir in
      let* local_modules, external_lib_archives =
        let* direct_requires = Resolve.Memo.read direct_requires in
        classify_libs sctx direct_requires |> Action_builder.of_memo
      in
      let* results =
        Ocamlobjinfo.rules
          toolchain
          ~dir:build_dir
          ~sandbox:(Some Sandbox_config.needs_sandboxing)
          ~units
      and* external_modules =
        List.map external_lib_archives ~f:(fun (lib, archive) ->
          let+ modules =
            Ocamlobjinfo.archive_rules
              toolchain
              ~dir:build_dir
              ~sandbox:(Some Sandbox_config.needs_sandboxing)
              ~archive
          in
          lib, modules)
        |> Action_builder.all
      in
      let* allowed_libs = Resolve.Memo.read allow_unused_libraries in
      let allowed_set = Lib.Set.of_list allowed_libs in
      let unused_libs =
        let all_imported =
          List.fold_left results ~init:Module_name.Unique.Set.empty ~f:(fun acc result ->
            let intf_deps = Ml_kind.Dict.get result Intf in
            let impl_deps = Ml_kind.Dict.get result Impl in
            Module_name.Unique.Set.union
              acc
              (Module_name.Unique.Set.union intf_deps impl_deps))
        in
        external_modules @ local_modules
        |> Lib.Map.of_list_exn
        |> Lib.Map.foldi ~init:[] ~f:(fun lib lib_modules acc ->
          (* Skip libraries with no modules *)
          if Module_name.Unique.Set.is_empty lib_modules
          then acc
          else (
            (* Check if any module from this library is imported *)
            let is_used =
              Module_name.Unique.Set.exists lib_modules ~f:(fun mod_name ->
                Module_name.Unique.Set.mem all_imported mod_name)
            in
            (* Check if library is in the allow list *)
            let is_allowed = Lib.Set.mem allowed_set lib in
            if is_used then acc else if is_allowed then acc else lib :: acc))
      in
      match unused_libs with
      | [] -> Action_builder.return (Action.progn [])
      | libs ->
        Action_builder.fail
          { fail =
              (fun () ->
                (* CR-someday rgrinberg: ideally, we'd use the locations of the
                   unused libraries, but they've already been discarded. *)
                User_error.raise
                  ~loc
                  [ Pp.text "Unused libraries:"
                  ; Pp.enumerate libs ~f:(fun lib ->
                      Lib.name lib |> Lib_name.to_string |> Pp.verbatim)
                  ])
          }
    in
    let unused_libs_alias = Alias.make Alias0.unused_libs ~dir in
    Rules.Produce.Alias.add_action
      unused_libs_alias
      ~loc
      (action |> Action_builder.map ~f:Action.Full.make)
;;
