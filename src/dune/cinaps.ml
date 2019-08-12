open Import
open! No_io
open Build.O

type t =
  { loc : Loc.t
  ; files : Predicate_lang.t
  ; libraries : Dune_file.Lib_dep.t list
  ; preprocess : Dune_file.Preprocess_map.t
  ; preprocessor_deps : Dune_file.Dep_conf.t list
  ; flags : Ocaml_flags.Spec.t
  }

type Stanza.t += T of t

let syntax =
  Syntax.create ~name:"cinaps" ~desc:"the cinaps extension" [ (1, 0) ]

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ loc = loc
     and+ files =
       field "files" Predicate_lang.decode ~default:Predicate_lang.true_
     and+ preprocess =
       field "preprocess" Dune_file.Preprocess_map.decode
         ~default:Dune_file.Preprocess_map.default
     and+ preprocessor_deps =
       field "preprocessor_deps" (repeat Dune_file.Dep_conf.decode) ~default:[]
     and+ libraries = field "libraries" Dune_file.Lib_deps.decode ~default:[]
     and+ flags = Ocaml_flags.Spec.decode in
     { loc; files; libraries; preprocess; preprocessor_deps; flags })

let () =
  let open Dune_lang.Decoder in
  Dune_project.Extension.register_simple syntax
    (return [ ("cinaps", decode >>| fun x -> [ T x ]) ])

let gen_rules sctx t ~dir ~scope =
  let loc = t.loc in
  let name = "cinaps" in
  let cinaps_dir = Path.Build.relative dir ".cinaps" in
  let cinaps_ml = Path.Build.relative cinaps_dir "_cinaps.ml-gen" in
  let cinaps_exe = Path.Build.relative cinaps_dir "cinaps.exe" in
  let main_module_name = Module_name.of_string "_cinaps" in
  (* Files checked by cinaps *)
  let cinapsed_files =
    File_tree.files_of
      (Super_context.file_tree sctx)
      (Path.Build.drop_build_context_exn dir)
    |> Path.Source.Set.to_list
    |> List.filter_map ~f:(fun p ->
           if
             Predicate_lang.exec t.files (Path.Source.basename p)
               ~standard:Predicate_lang.true_
           then
             Some (Path.Build.append_source (Super_context.build_dir sctx) p)
           else
             None)
  in
  (* Ask cinaps to produce a .ml file to build *)
  Super_context.add_rule sctx ~loc:t.loc ~dir
    (Command.run ~dir:(Path.build dir)
       (Super_context.resolve_program sctx ~dir ~loc:(Some loc) "cinaps"
          ~hint:"opam pin add --dev cinaps")
       [ A "-staged"
       ; Target cinaps_ml
       ; Deps (List.map cinapsed_files ~f:Path.build)
       ]);
  let obj_dir = Obj_dir.make_exe ~dir:cinaps_dir ~name:"cinaps" in
  let modules =
    Module_name.Map.singleton main_module_name
      (Module.generated main_module_name ~src_dir:(Path.build cinaps_dir))
  in
  let expander = Super_context.expander sctx ~dir in
  let preprocess =
    Preprocessing.make sctx ~dir ~expander ~dep_kind:Required
      ~lint:Dune_file.Preprocess_map.no_preprocessing ~preprocess:t.preprocess
      ~preprocessor_deps:
        (Super_context.Deps.interpret sctx ~expander t.preprocessor_deps)
      ~lib_name:None ~scope
  in
  let modules =
    Modules.exe_unwrapped modules
    |> Modules.map_user_written ~f:(Preprocessing.pp_module preprocess)
  in
  let compile_info =
    Lib.DB.resolve_user_written_deps_for_exes (Scope.libs scope)
      [ (t.loc, name) ]
      ( Dune_file.Lib_dep.Direct
          (loc, Lib_name.of_string_exn "cinaps.runtime" ~loc:None)
      :: t.libraries )
      ~pps:(Dune_file.Preprocess_map.pps t.preprocess)
      ~variants:None ~optional:false
  in
  let cctx =
    Compilation_context.create () ~super_context:sctx ~expander ~scope ~obj_dir
      ~modules ~opaque:false
      ~requires_compile:(Lib.Compile.direct_requires compile_info)
      ~requires_link:(Lib.Compile.requires_link compile_info)
      ~flags:(Ocaml_flags.of_list [ "-w"; "-24" ])
      ~js_of_ocaml:None ~dynlink:false ~package:None
  in
  Exe.build_and_link cctx
    ~program:{ name; main_module_name; loc }
    ~linkages:[ Exe.Linkage.native_or_custom (Super_context.context sctx) ]
    ~promote:None;
  Super_context.add_alias_action sctx ~dir ~loc:(Some loc) ~stamp:"cinaps"
    (Alias.runtest ~dir)
    (let module A = Action in
    let cinaps_exe = Path.build cinaps_exe in
    Build.path cinaps_exe
    >>^ fun () ->
    A.chdir (Path.build dir)
      (A.progn
         ( A.run (Ok cinaps_exe) [ "-diff-cmd"; "-" ]
         :: List.map cinapsed_files ~f:(fun fn ->
                let fn = Path.build fn in
                A.diff ~optional:true fn
                  (Path.extend_basename fn ~suffix:".cinaps-corrected")) )))
