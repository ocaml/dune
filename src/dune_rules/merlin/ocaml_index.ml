open Import

let ocaml_index sctx ~dir =
  Super_context.resolve_program ~loc:None ~dir sctx "ocaml-index"
;;

let index_file_name = "cctx.ocaml-index"

let index_path_in_obj_dir obj_dir =
  let dir = Obj_dir.obj_dir obj_dir in
  Path.Build.relative dir index_file_name
;;

let project_index ~build_dir = Path.Build.relative build_dir "project.ocaml-index"

let cctx_rules cctx =
  (* Indexing is performed by the external binary [ocaml-index] which performs
     full shape reduction to compute the actual definition of all the elements in
     the typedtree. This step is therefore dependent on all the cmts of those
     definitions are used by all the cmts of modules in this cctx. *)
  let sctx = Compilation_context.super_context cctx in
  let dir = Compilation_context.dir cctx in
  let aggregate =
    let obj_dir = Compilation_context.obj_dir cctx in
    let fn = index_path_in_obj_dir obj_dir in
    let additional_libs =
      let open Resolve.Memo.O in
      let+ non_compile_libs =
        (* The indexer relies on the load_path of cmt files. When
           [implicit_transitive_deps] is set to [false] some necessary paths will
           be missing.These are passed to the indexer with the `-I` flag.

           The implicit transitive libs correspond to the set:
           (requires_link \ req_compile) *)
        let* req_link = Compilation_context.requires_link cctx in
        let+ req_compile = Compilation_context.requires_compile cctx in
        List.filter req_link ~f:(fun l -> not (List.exists req_compile ~f:(Lib.equal l)))
      in
      Lib_flags.L.include_flags non_compile_libs (Lib_mode.Ocaml Byte)
    in
    (* Indexing depends (recursively) on [required_compile] libs:
       - These libs's cmt files should be built before indexing starts
       - If these libs are rebuilt a re-indexation is needed *)
    let other_indexes_deps =
      let open Resolve.Memo.O in
      let+ requires_compile = Compilation_context.requires_compile cctx in
      let deps =
        List.filter_map requires_compile ~f:(fun l ->
          match Lib.info l |> Lib_info.obj_dir |> Obj_dir.obj_dir with
          | In_build_dir dir -> Some (Path.relative (Path.build dir) index_file_name)
          | _ -> None)
      in
      Command.Args.Hidden_deps (Dep.Set.of_files deps)
    in
    let context_dir =
      Compilation_context.context cctx
      |> Context.name
      |> Context_name.build_dir
      |> Path.build
    in
    (* Indexation also depends on the current stanza's modules *)
    let modules_deps =
      let cm_kind = Lib_mode.Cm_kind.(Ocaml Cmi) in
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
      ; Target fn
      ; Deps modules_deps
      ; Dyn (Resolve.Memo.read additional_libs)
      ; Dyn (Resolve.Memo.read other_indexes_deps)
      ]
  in
  Super_context.add_rule sctx ~dir aggregate
;;

let context_indexes sctx =
  let open Memo.O in
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
      | _ -> None
    in
    match obj with
    | None -> acc
    | Some obj_dir -> Path.build (index_path_in_obj_dir obj_dir) :: acc)
;;

let project_rule sctx project =
  let open Memo.O in
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
