open Import
module CC = Compilation_context
module SC = Super_context

let ocaml_index sctx ~dir =
  Super_context.resolve_program ~loc:None ~dir sctx "ocaml-index"
;;

let index_path_in_obj_dir obj_dir =
  let dir = Obj_dir.obj_dir obj_dir in
  Path.Build.relative dir "cctx.ocaml-index"
;;

let project_index ~build_dir = Path.Build.relative build_dir "project.ocaml-index"

let cctx_rules cctx () =
  let open Memo.O in
  (* Indexing is performed by the external binary [ocaml-index] which performs
     full shape reduction to compute the actual definition of all the elements in
     the typedtree. This step is therefore dependent on all the cmts of those
     definitions are used by all the cmts of modules in this cctx. *)
  let dir = CC.dir cctx in
  let modules =
    CC.modules cctx
    |> Modules.With_vlib.drop_vlib
    |> Modules.fold ~init:[] ~f:(fun x acc -> x :: acc)
  in
  let sctx = CC.super_context cctx in
  let obj_dir = CC.obj_dir cctx in
  let cm_kind = Lib_mode.Cm_kind.(Ocaml Cmi) in
  let modules_with_cmts =
    List.filter_map
      ~f:(fun module_ ->
        Obj_dir.Module.cmt_file obj_dir ~ml_kind:Impl ~cm_kind module_
        |> Option.map ~f:(fun cmt -> Path.build cmt))
      modules
  in
  let modules_with_cmtis =
    List.filter_map
      ~f:(fun module_ ->
        Obj_dir.Module.cmt_file obj_dir ~ml_kind:Intf ~cm_kind module_
        |> Option.map ~f:(fun cmti -> Path.build cmti))
      modules
  in
  let context_dir =
    CC.context cctx |> Context.name |> Context_name.build_dir |> Path.build
  in
  let* additional_libs =
    (* The indexer relies on the load_path of cmt files. When
       [implicit_transitive_deps] is set to [false] some necessary paths will
       be missing.These are passed to the indexer with the `-I` flag.

       The implicit transitive libs correspond to the set:
       (requires_link \ req_compile)
    *)
    let open Resolve.Memo.O in
    let* req_link = CC.requires_link cctx in
    let+ req_compile = CC.requires_compile cctx in
    let non_compile_libs =
      List.filter req_link ~f:(fun l -> not (List.exists req_compile ~f:(Lib.equal l)))
    in
    Lib_flags.L.include_flags non_compile_libs (Lib_mode.Ocaml Byte)
  in
  let fn = index_path_in_obj_dir obj_dir in
  let includes =
    Resolve.peek additional_libs |> Result.value ~default:Command.Args.empty
  in
  let aggregate =
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
  SC.add_rule sctx ~dir aggregate
;;

let context_indexes sctx =
  let open Memo.O in
  let ctx = Super_context.context sctx in
  let build_dir = Context.build_dir ctx in
  let+ stanzas = Dune_load.dune_files (Context.name ctx) in
  Dune_file.fold_static_stanzas stanzas ~init:[] ~f:(fun dune_file stanza acc ->
    let dir = Path.Build.append_source build_dir (Dune_file.dir dune_file) in
    let obj =
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
  let* indexes = context_indexes sctx in
  let ctx = Super_context.context sctx in
  let build_dir = Context.build_dir ctx in
  let dir = Path.Build.append_source build_dir @@ Dune_project.root project in
  let ocaml_index_alias = Alias.make Alias0.ocaml_index ~dir in
  Rules.Produce.Alias.add_deps ocaml_index_alias (Action_builder.paths_existing @@ indexes)
;;
