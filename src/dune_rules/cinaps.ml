open Import

type t =
  { loc : Loc.t
  ; files : Predicate_lang.Glob.t
  ; libraries : Lib_dep.t list
  ; preprocess : Preprocess.Without_instrumentation.t Preprocess.Per_module.t
  ; preprocessor_deps : Dep_conf.t list
  ; runtime_deps : Dep_conf.t list
  ; cinaps_version : Syntax.Version.t
  ; alias : Alias.Name.t option
  ; link_flags : Link_flags.Spec.t
  }

let name = "cinaps"
let cinaps_alias = Alias.Name.of_string name

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let syntax =
  Dune_lang.Syntax.create
    ~name
    ~desc:"the cinaps extension"
    [ (1, 0), `Since (1, 11)
    ; (1, 1), `Since (3, 5)
    ; (1, 2), `Since (3, 7)
    ; (1, 3), `Since (3, 8)
    ]
;;

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ loc = loc
     and+ files = field "files" Predicate_lang.Glob.decode ~default:Predicate_lang.true_
     and+ preprocess, preprocessor_deps = Preprocess.preprocess_fields
     and+ libraries =
       field "libraries" (Lib_dep.L.decode ~allow_re_export:false) ~default:[]
     and+ runtime_deps =
       field
         ~default:[]
         "runtime_deps"
         (Dune_lang.Syntax.since syntax (1, 1) >>> repeat Dep_conf.decode)
     and+ cinaps_version = Dune_lang.Syntax.get_exn syntax
     and+ alias = field_o "alias" Dune_lang.Alias.decode
     and+ link_flags =
       Link_flags.Spec.decode ~check:(Some (Dune_lang.Syntax.since syntax (1, 3)))
     (* TODO use this field? *)
     and+ _flags = Ocaml_flags.Spec.decode in
     { loc
     ; files
     ; libraries
     ; preprocess
     ; preprocessor_deps
     ; runtime_deps
     ; cinaps_version
     ; alias
     ; link_flags
     })
;;

let () =
  let open Dune_lang.Decoder in
  Dune_project.Extension.register_simple
    syntax
    (return
       [ ( name
         , let+ stanza = decode in
           [ make_stanza stanza ] )
       ])
;;

let gen_rules sctx t ~dir ~scope =
  let open Memo.O in
  let loc = t.loc in
  (* Files checked by cinaps *)
  let* cinapsed_files =
    Source_tree.files_of (Path.Build.drop_build_context_exn dir)
    >>| Path.Source.Set.to_list
    >>| List.filter_map ~f:(fun p ->
      if Predicate_lang.Glob.test
           t.files
           (Path.Source.basename p)
           ~standard:Predicate_lang.true_
      then
        Some
          (Path.Build.append_source (Super_context.context sctx |> Context.build_dir) p)
      else None)
  in
  let cinaps_dir =
    let stamp =
      let digest =
        if cinapsed_files = []
        then Digest.generic (t.loc, t.libraries, t.preprocess, t.preprocessor_deps)
        else
          Digest.generic (cinapsed_files, t.libraries, t.preprocess, t.preprocessor_deps)
      in
      String.take (Digest.to_string digest) 8
    in
    Path.Build.relative dir ("." ^ name ^ "." ^ stamp)
  in
  let main_module_name = Module_name.of_string name in
  let module_ = Module.generated ~kind:Impl [ main_module_name ] ~src_dir:cinaps_dir in
  let cinaps_ml =
    Module.source ~ml_kind:Ml_kind.Impl module_
    |> Option.value_exn
    |> Module.File.path
    |> Path.as_in_build_dir_exn
  in
  let cinaps_exe = Path.Build.relative cinaps_dir (name ^ ".exe") in
  let* () =
    (* Ask cinaps to produce a .ml file to build *)
    let sandbox =
      if t.cinaps_version >= (1, 1)
      then Sandbox_config.needs_sandboxing
      else Sandbox_config.default
    in
    Super_context.add_rule
      sctx
      ~loc:t.loc
      ~dir
      (let prog =
         Super_context.resolve_program
           sctx
           ~dir
           ~where:Original_path
           ~loc:(Some loc)
           name
           ~hint:"opam install cinaps"
       in
       Command.run_dyn_prog
         ~dir:(Path.build dir)
         prog
         ~sandbox
         [ A "-staged"; Target cinaps_ml; Deps (List.map cinapsed_files ~f:Path.build) ])
  and* expander = Super_context.expander sctx ~dir in
  let* preprocess =
    Pp_spec_rules.make
      sctx
      ~dir
      ~expander
      ~lint:(Preprocess.Per_module.no_preprocessing ())
      ~preprocess:t.preprocess
      ~preprocessor_deps:t.preprocessor_deps
      ~instrumentation_deps:[]
      ~lib_name:None
      ~scope
  in
  let* modules =
    Pp_spec.pp_module preprocess module_ >>| Modules.With_vlib.singleton_exe
  in
  let dune_version = Scope.project scope |> Dune_project.dune_version in
  let names = [ t.loc, name ] in
  let merlin_ident = Merlin_ident.for_exes ~names:(List.map ~f:snd names) in
  let compile_info =
    Lib.DB.resolve_user_written_deps
      (Scope.libs scope)
      (`Exe names)
      (Lib_dep.Direct (loc, Lib_name.of_string "cinaps.runtime") :: t.libraries)
      ~pps:(Preprocess.Per_module.pps t.preprocess)
      ~dune_version
      ~merlin_ident
      ~allow_overlaps:false
      ~forbidden_libraries:[]
  in
  let obj_dir = Obj_dir.make_exe ~dir:cinaps_dir ~name in
  let* cctx =
    let requires_compile = Lib.Compile.direct_requires compile_info in
    let requires_link = Lib.Compile.requires_link compile_info in
    Compilation_context.create
      ()
      ~super_context:sctx
      ~scope
      ~obj_dir
      ~modules
      ~opaque:(Explicit false)
      ~requires_compile
      ~requires_link
      ~flags:(Ocaml_flags.of_list [ "-w"; "-24" ])
      ~js_of_ocaml:None
      ~melange_package_name:None
      ~package:None
  in
  let* (_ : Exe.dep_graphs) =
    let link_args =
      let open Action_builder.O in
      let* link_flags =
        Action_builder.of_memo (Ocaml_flags_db.link_flags sctx ~dir t.link_flags)
      in
      let+ link_args = Link_flags.get ~use_standard_cxx_flags:false link_flags in
      Command.Args.As link_args
    in
    Exe.build_and_link
      cctx
      ~link_args
      ~program:{ name; main_module_name; loc }
      ~linkages:[ Exe.Linkage.native_or_custom (Compilation_context.ocaml cctx) ]
      ~promote:None
  in
  let action =
    let open Action_builder.O in
    let module A = Action in
    let cinaps_exe = Path.build cinaps_exe in
    let runtime_deps, sandbox =
      let sandbox =
        if t.cinaps_version >= (1, 1)
        then Sandbox_config.needs_sandboxing
        else Sandbox_config.no_special_requirements
      in
      Dep_conf_eval.unnamed ~sandbox ~expander t.runtime_deps
    in
    let* () = runtime_deps in
    let+ () =
      Action_builder.deps
        (Dep.Set.of_files (cinaps_exe :: List.rev_map cinapsed_files ~f:Path.build))
    in
    Action.Full.make ~sandbox
    @@ A.chdir
         (Path.build dir)
         (A.progn
            [ A.run (Ok cinaps_exe) [ "-diff-cmd"; "-" ]
            ; A.concurrent
              @@ List.map cinapsed_files ~f:(fun fn ->
                A.diff
                  ~optional:true
                  (Path.build fn)
                  (Path.Build.extend_basename fn ~suffix:".cinaps-corrected"))
            ])
  in
  let cinaps_alias = Alias.make ~dir @@ Option.value t.alias ~default:cinaps_alias in
  let* () = Super_context.add_alias_action sctx ~dir ~loc cinaps_alias action in
  match t.alias with
  | Some _ -> Memo.return ()
  | None ->
    Rules.Produce.Alias.add_deps
      (Alias.make Alias0.runtest ~dir)
      (Alias_builder.alias cinaps_alias)
;;
