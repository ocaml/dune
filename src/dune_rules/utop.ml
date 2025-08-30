open Import
open Memo.O

let exe_name = "utop"
let utop_dir_basename = ".utop"

let utop_exe =
  (* Use the [.exe] version. As the utop executable is declared with [(modes
     (byte))], the [.exe] correspond the bytecode linked in custom mode. We do
     that so that it works without hassle when generating a utop for a library
     with C stubs. *)
  Filename.concat utop_dir_basename (exe_name ^ Mode.exe_ext Mode.Byte)
;;

let source ~dir =
  Toplevel.Source.make
    ~dir:(Path.Build.relative dir utop_dir_basename)
    ~loc:(Loc.in_dir (Path.build dir))
    ~main:"UTop_main.main ();"
    ~name:exe_name
;;

module Libs_and_ppxs =
  Monoid.Product
    (Monoid.Appendable_list (struct
      type t = Lib.t
    end))
    (Monoid.Appendable_list (struct
         type t = Loc.t * Lib_name.t
       end))

module Source_tree_map_reduce = Source_tree.Dir.Make_map_reduce (Memo) (Libs_and_ppxs)

let add_stanza db ~dir (acc, pps) stanza =
  match Stanza.repr stanza with
  | Library.T l ->
    let+ lib =
      let+ resolve =
        Lib.DB.resolve_when_exists db (l.buildable.loc, Library.best_name l)
      in
      Option.map resolve ~f:Resolve.peek
      (* external lib with a name matching our private name *)
    in
    (match lib with
     | None | Some (Error ()) ->
       acc, pps (* library is defined but outside our scope or is disabled *)
     | Some (Ok lib) ->
       (* still need to make sure that it's not coming from an external
          source *)
       let info = Lib.info lib in
       let src_dir = Lib_info.src_dir info in
       (* Only select libraries that are not implementations.
          Implementations are selected using the default implementation
          feature. *)
       let not_impl = Option.is_none (Lib_info.implements info) in
       if not_impl && Path.is_descendant ~of_:(Path.build dir) src_dir
       then (
         match (Lib_info.kind info : Lib_kind.t) with
         | Virtual | Parameter | Dune_file Normal -> Appendable_list.cons lib acc, pps
         (* CR @maiste or @art-w: the parametrized libraries in utop follows
             the same schema as Normal library but it needs to be verified once
             parametrized libraries are fully supported. *)
         | Dune_file (Ppx_rewriter _ | Ppx_deriver _) ->
           ( Appendable_list.cons lib acc
           , Appendable_list.cons (Lib_info.loc info, Lib_info.name info) pps ))
       else acc, pps)
  | Executables.T exes ->
    let+ libs =
      let* compile_info =
        let* scope = Scope.DB.find_by_dir dir in
        let dune_version =
          let project = Scope.project scope in
          Dune_project.dune_version project
        in
        let+ pps =
          Instrumentation.with_instrumentation
            exes.buildable.preprocess
            ~instrumentation_backend:(Lib.DB.instrumentation_backend (Scope.libs scope))
          |> Resolve.Memo.read_memo
          >>| Preprocess.Per_module.pps
        in
        Lib.DB.resolve_user_written_deps
          db
          (`Exe exes.names)
          exes.buildable.libraries
          ~pps
          ~dune_version
          ~allow_overlaps:exes.buildable.allow_overlapping_dependencies
          ~forbidden_libraries:exes.forbidden_libraries
      in
      let+ available = Lib.Compile.direct_requires compile_info in
      Resolve.peek available
    in
    (match libs with
     | Error () -> acc, pps
     | Ok libs ->
       List.fold_left libs ~init:(acc, pps) ~f:(fun (acc, pps) lib ->
         let info = Lib.info lib in
         match (Lib_info.kind info : Lib_kind.t) with
         | Virtual | Parameter | Dune_file Normal -> Appendable_list.cons lib acc, pps
         (* CR @maiste or @art-w: the parametrized libraries in utop follows
             the same schema as Normal library but it needs to be verified once
             parametrized libraries are fully supported. *)
         | Dune_file (Ppx_rewriter _ | Ppx_deriver _) ->
           ( Appendable_list.cons lib acc
           , Appendable_list.cons (Lib_info.loc info, Lib_info.name info) pps )))
  | _ -> Memo.return (acc, pps)
;;

let libs_and_ppx_under_dir sctx ~db ~dir =
  (match Path.drop_build_context dir with
   | None -> Memo.return None
   | Some dir -> Source_tree.find_dir dir)
  >>= function
  | None -> Memo.return ([], [])
  | Some dir ->
    let+ libs, pps =
      Source_tree_map_reduce.map_reduce
        dir
        ~traverse:Source_dir_status.Set.all
        ~trace_event_name:"Utop rules loading"
        ~f:(fun dir ->
          let dir =
            Path.Build.append_source
              (Context.build_dir (Super_context.context sctx))
              (Source_tree.Dir.path dir)
          in
          Dune_load.stanzas_in_dir dir
          >>= function
          | None -> Memo.return Libs_and_ppxs.empty
          | Some (d : Dune_file.t) ->
            Dune_file.stanzas d
            >>= Memo.List.fold_left ~init:Libs_and_ppxs.empty ~f:(add_stanza db ~dir))
    in
    Appendable_list.to_list libs, Appendable_list.to_list pps
;;

let libs_under_dir sctx ~db ~dir = libs_and_ppx_under_dir sctx ~db ~dir >>| fst

let requires ~loc ~db ~libs =
  let open Resolve.Memo.O in
  (loc, Lib_name.of_string "utop")
  |> Lib.DB.resolve db
  >>| (fun utop -> utop :: libs)
  >>= Lib.closure ~linking:true
;;

let utop_dev_tool_lock_dir_exists =
  Memo.Lazy.create (fun () ->
    let path = Dune_pkg.Lock_dir.dev_tool_lock_dir_path Utop in
    Fs_memo.dir_exists (Path.as_outside_build_dir_exn path))
;;

let utop_findlib_conf = Filename.concat utop_dir_basename "findlib.conf"

(* The lib directory of the utop package and of each of its dependencies within
   the _build directory (or the toolchains directory in the case of the OCaml
   compiler). *)
let utop_ocamlpath = Memo.Lazy.create (fun () -> Pkg_rules.dev_tool_ocamlpath Utop)

(* Creates a rule that generates a custom findlib.conf containing the path to
   the utop library as well as all of its dependencies in the _build directory
   (or the toolchains directory in the case of the OCaml compiler). Utop uses
   findlib to locate libraries at runtime. When utop is running as a devtool,
   libraries are not in the location suggested by the default findlib.conf
   (there may not even be a default findlib.conf on the current system) and so
   we need to tell findlib where to look for libraries by means of a custom
   findlib.conf file. *)
let findlib_conf sctx ~dir =
  Memo.Lazy.force utop_dev_tool_lock_dir_exists
  >>= function
  | false ->
    (* If there isn't lockdir don't create the findlib.conf rule. *)
    Memo.return ()
  | true ->
    let path = Path.Build.relative dir utop_findlib_conf in
    let contents =
      Memo.Lazy.force utop_ocamlpath
      >>| List.map ~f:Path.to_absolute_filename
      >>| String.concat ~sep:":"
      >>| sprintf "path=\"%s\""
      |> Action_builder.of_memo
    in
    Action_builder.write_file_dyn path contents |> Super_context.add_rule sctx ~dir
;;

let lib_db sctx ~dir =
  let* scope = Scope.DB.find_by_dir dir in
  let* lock_dir_exists = Memo.Lazy.force utop_dev_tool_lock_dir_exists in
  match lock_dir_exists with
  | false -> Memo.return (Scope.libs scope)
  | true ->
    let* ocamlpath = Memo.Lazy.force utop_ocamlpath in
    Lib.DB.of_paths (Super_context.context sctx) ~paths:ocamlpath
    >>| Lib.DB.with_parent ~parent:(Some (Scope.libs scope))
;;

let setup sctx ~dir =
  let* expander = Super_context.expander sctx ~dir in
  let* scope = Scope.DB.find_by_dir dir in
  let* db = lib_db sctx ~dir in
  let* libs, pps = libs_and_ppx_under_dir sctx ~db ~dir:(Path.build dir) in
  let pps =
    if List.is_empty pps
    then Preprocess.No_preprocessing
    else Preprocess.Pps { loc = Loc.none; pps; flags = []; staged = false }
  in
  let* preprocessing =
    let preprocess = Module_name.Per_item.for_all pps in
    Pp_spec_rules.make
      sctx
      ~dir
      ~expander
      ~scope
      ~lib_name:None
      ~lint:Lint.no_lint
      ~preprocess
      ~preprocessor_deps:[]
      ~instrumentation_deps:[]
  in
  let source = source ~dir in
  let obj_dir = Toplevel.Source.obj_dir source in
  let loc = Toplevel.Source.loc source in
  let* modules = Toplevel.Source.modules source preprocessing in
  let requires = requires ~loc ~db ~libs in
  let flags =
    let project = Scope.project scope in
    let dune_version = Dune_project.dune_version project in
    let profile = Super_context.context sctx |> Context.profile in
    Ocaml_flags.append_common (Ocaml_flags.default ~dune_version ~profile) [ "-w"; "-24" ]
  in
  let* cctx =
    let requires_link = Memo.lazy_ (fun () -> requires) in
    Compilation_context.create
      ()
      ~super_context:sctx
      ~scope
      ~obj_dir
      ~modules
      ~opaque:(Explicit false)
      ~requires_link
      ~requires_compile:requires
      ~flags
      ~js_of_ocaml:(Js_of_ocaml.Mode.Pair.make None)
      ~melange_package_name:None
      ~package:None
      ~preprocessing
  in
  let* () = findlib_conf sctx ~dir in
  let toplevel = Toplevel.make ~cctx ~source ~preprocess:pps expander in
  Toplevel.setup_rules toplevel ~linkage:Exe.Linkage.byte
;;

let requires_under_dir sctx ~dir =
  let* db = lib_db sctx ~dir in
  let* libs = libs_under_dir sctx ~db ~dir:(Path.build dir) in
  let loc = Toplevel.Source.loc (source ~dir) in
  requires ~loc ~db ~libs
;;
