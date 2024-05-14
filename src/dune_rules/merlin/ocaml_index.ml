open Import

let ocaml_index sctx ~dir =
  Super_context.resolve_program ~loc:None ~dir sctx "ocaml-index"
;;

let index_path_in_obj_dir obj_dir =
  let dir = Obj_dir.obj_dir obj_dir in
  Path.Build.relative dir "cctx.ocaml-index"
;;

let project_index ~build_dir = Path.Build.relative build_dir "project.ocaml-index"

let cctx_rules cctx =
  let open Memo.O in
  (* Indexing is performed by the external binary [ocaml-index] which performs
     full shape reduction to compute the actual definition of all the elements in
     the typedtree. This step is therefore dependent on all the cmts of those
     definitions are used by all the cmts of modules in this cctx. *)
  let sctx = Compilation_context.super_context cctx in
  let* additional_libs =
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
  let dir = Compilation_context.dir cctx in
  let aggregate =
    let obj_dir = Compilation_context.obj_dir cctx in
    let fn = index_path_in_obj_dir obj_dir in
    let includes =
      Resolve.peek additional_libs |> Result.value ~default:Command.Args.empty
    in
    let context_dir =
      Compilation_context.context cctx
      |> Context.name
      |> Context_name.build_dir
      |> Path.build
    in
    let modules_with_cmts, modules_with_cmtis =
      let cm_kind = Lib_mode.Cm_kind.(Ocaml Cmi) in
      let modules =
        (* We only index occurrences in user-written modules *)
        Compilation_context.modules cctx
        |> Modules.With_vlib.drop_vlib
        |> Modules.fold_user_written ~init:[] ~f:List.cons
      in
      let modules_with_cmtis =
        List.filter_map modules ~f:(fun module_ ->
          Obj_dir.Module.cmt_file obj_dir ~ml_kind:Intf ~cm_kind module_
          |> Option.map ~f:(fun cmti -> Path.build cmti))
      in
      let modules_with_cmts =
        List.filter_map modules ~f:(fun module_ ->
          Obj_dir.Module.cmt_file obj_dir ~ml_kind:Impl ~cm_kind module_
          |> Option.map ~f:(fun cmt -> Path.build cmt))
      in
      modules_with_cmts, modules_with_cmtis
    in
    Command.run_dyn_prog
      ~dir:context_dir
      (ocaml_index sctx ~dir)
      [ A "aggregate"
      ; A "--root"
      ; A Path.(Source.root |> source |> to_absolute_filename)
      ; A "-o"
      ; Target fn
      ; Deps modules_with_cmts
      ; Deps modules_with_cmtis
      ; includes
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
