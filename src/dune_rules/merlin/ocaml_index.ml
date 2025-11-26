open Import
open Memo.O

let ocaml_index_dev_tool_exe_path_building_if_necessary () =
  let open Action_builder.O in
  let path = Path.build (Pkg_dev_tool.exe_path Ocaml_index) in
  let+ () = Action_builder.path path in
  Ok path
;;

let ocaml_index_dev_tool_exists () =
  Lock_dir.dev_tool_external_lock_dir Ocaml_index
  |> Path.external_
  |> Path.Untracked.exists
;;

let ocaml_index sctx ~dir =
  match ocaml_index_dev_tool_exists () with
  | true -> ocaml_index_dev_tool_exe_path_building_if_necessary ()
  | false ->
    Super_context.resolve_program
      sctx
      ~dir
      "ocaml-index"
      ~loc:None
      ~hint:"opam install ocaml-index"
;;

let index_file_name = "cctx.ocaml-index"

let index_path_in_obj_dir obj_dir =
  let dir = Obj_dir.obj_dir obj_dir in
  Path.Build.relative dir index_file_name
;;

let cctx_rules cctx =
  (* Indexing is performed by the external binary [ocaml-index] which performs
     full shape reduction to compute the actual definition of all the elements in
     the typedtree. This step is therefore dependent on all the cmts of those
     definitions are used by all the cmts of modules in this cctx. *)
  let sctx = Compilation_context.super_context cctx in
  let dir = Compilation_context.dir cctx in
  let aggregate =
    let obj_dir = Compilation_context.obj_dir cctx in
    let target = index_path_in_obj_dir obj_dir in
    let additional_libs =
      let scope = Compilation_context.scope cctx in
      if Dune_project.dune_version (Scope.project scope) >= (3, 17)
      then
        (* Dune language >= 3.17 correctly passes the `-H` flag to the compiler. *)
        Resolve.Memo.return Command.Args.empty
      else
        let open Resolve.Memo.O in
        let+ non_compile_libs =
          let* req_compile = Compilation_context.requires_compile cctx in
          Compilation_context.requires_link cctx
          >>| List.filter ~f:(fun l -> not (List.exists req_compile ~f:(Lib.equal l)))
        in
        Lib_flags.L.include_flags
          ~direct_libs:non_compile_libs
          ~hidden_libs:[]
          (Lib_mode.Ocaml Byte)
          (Compilation_context.ocaml cctx).lib_config
    in
    (* Indexing depends (recursively) on [required_compile] libs:
       - These libs's cmt files should be built before indexing starts
       - If these libs are rebuilt a re-indexation is needed *)
    let other_indexes_deps =
      let open Resolve.Memo.O in
      let+ deps =
        Compilation_context.requires_compile cctx
        >>| List.filter_map ~f:(fun lib ->
          Lib.Local.of_lib lib
          |> Option.map ~f:(fun lib ->
            Lib.Local.obj_dir lib |> index_path_in_obj_dir |> Path.build))
        >>| Dep.Set.of_files
      in
      Command.Args.Hidden_deps deps
    in
    let context_dir =
      Compilation_context.context cctx
      |> Context.name
      |> Context_name.build_dir
      |> Path.build
    in
    (* Indexation also depends on the current stanza's modules *)
    let modules_deps =
      let modes = Compilation_context.modes cctx in
      let cm_kind =
        if modes.ocaml.native || modes.ocaml.byte
        then Lib_mode.Cm_kind.(Ocaml Cmi)
        else Lib_mode.Cm_kind.(Melange Cmi)
      in
      (* We only index occurrences in user-written modules *)
      Compilation_context.modules cctx
      |> Modules.With_vlib.drop_vlib
      |> Modules.fold_user_written ~init:[] ~f:(fun module_ acc ->
        let cmts =
          [ Ml_kind.Intf; Impl ]
          |> List.filter_map ~f:(fun ml_kind ->
            Obj_dir.Module.cmt_file obj_dir ~ml_kind ~cm_kind module_
            |> Option.map ~f:Path.build)
        in
        List.rev_append cmts acc)
    in
    Command.run_dyn_prog
      ~dir:context_dir
      (ocaml_index sctx ~dir)
      [ A "aggregate"
      ; A "-o"
      ; Target target
      ; Deps modules_deps
      ; Dyn (Resolve.Memo.read additional_libs)
      ; Dyn (Resolve.Memo.read other_indexes_deps)
      ]
  in
  Super_context.add_rule sctx ~dir aggregate
;;

let context_indexes sctx =
  let ctx = Super_context.context sctx in
  Context.name ctx
  |> Dune_load.dune_files
  >>| Dune_file.fold_static_stanzas ~init:[] ~f:(fun dune_file stanza acc ->
    let obj =
      let dir =
        let build_dir = Context.build_dir ctx in
        Path.Build.append_source build_dir (Dune_file.dir dune_file)
      in
      match Stanza.repr stanza with
      | Executables.T exes | Tests.T { exes; _ } -> Some (Executables.obj_dir ~dir exes)
      | Library.T lib -> Some (Library.obj_dir ~dir lib)
      | Melange_stanzas.Emit.T { target; _ } ->
        Some (Obj_dir.make_melange_emit ~dir ~name:target)
      | _ -> None
    in
    match obj with
    | None -> acc
    | Some obj_dir -> Path.build (index_path_in_obj_dir obj_dir) :: acc)
;;

let project_rule sctx project =
  let ocaml_index_alias =
    let dir =
      let build_dir =
        let ctx = Super_context.context sctx in
        Context.build_dir ctx
      in
      Path.Build.append_source build_dir @@ Dune_project.root project
    in
    Alias.make Alias0.ocaml_index ~dir
  in
  let* indexes = context_indexes sctx in
  Rules.Produce.Alias.add_deps ocaml_index_alias (Action_builder.paths_existing @@ indexes)
;;
