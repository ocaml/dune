open Import
open Memo.O

let toplevel_dir_prefix = ".toplevel."

module Source = struct
  type t =
    { name : string
    ; dir : Path.Build.t
    ; loc : Loc.t
    ; main : string
    }

  let main_module t =
    let main_module_name = Module_name.of_string_allow_invalid (t.loc, t.name) in
    Module.generated ~kind:Impl ~src_dir:t.dir [ main_module_name ]
  ;;

  let source_path t =
    Module.file (main_module t) ~ml_kind:Impl
    |> Option.value_exn
    |> Path.as_in_build_dir_exn
  ;;

  let obj_dir { dir; name; _ } = Obj_dir.make_exe ~dir ~name

  let modules t pp =
    main_module t |> Pp_spec.pp_module pp >>| Modules.With_vlib.singleton_exe
  ;;

  let make ~dir ~loc ~main ~name = { dir; main; name; loc }

  let of_stanza ~dir ~(toplevel : Toplevel_stanza.t) =
    { dir = Path.Build.relative dir (toplevel_dir_prefix ^ toplevel.name)
    ; name = toplevel.name
    ; loc = toplevel.loc
    ; main = "Topmain.main ()"
    }
  ;;

  let program t =
    { Exe.Program.loc = t.loc
    ; name = t.name
    ; main_module_name = Module.name (main_module t)
    }
  ;;

  let pp_ml t ~include_dirs =
    let open Pp.O in
    let include_dirs =
      Dyn.list (fun d -> Dyn.string (Path.to_absolute_filename d)) include_dirs
    in
    (* CR-soon rgrinberg: This is buggy and needs to use [hidden_include_dirs]
       when available *)
    Pp.vbox
      ~indent:2
      (Pp.verbatim "Clflags.include_dirs :=" ++ Pp.cut ++ Dyn.pp include_dirs)
    ++ Pp.verbatim ";"
    ++ Pp.newline
    ++ Pp.verbatim t.main
  ;;

  let loc t = t.loc
end

type t =
  { cctx : Compilation_context.t
  ; source : Source.t
  ; expander : Expander.t
  ; preprocess : Preprocess.Without_instrumentation.t Preprocess.t
  }

let make ~cctx ~source ~preprocess expander = { cctx; source; preprocess; expander }

let pp_flags t =
  let open Action_builder.O in
  let open Pp.O in
  match t.preprocess with
  | Pps { loc; pps; flags; staged = _ } ->
    let+ ppx =
      let+ exe, flags =
        let scope = Compilation_context.scope t.cctx in
        let ctx = Compilation_context.context t.cctx in
        Ppx_driver.get_ppx_driver
          ctx
          ~loc
          ~expander:t.expander
          ~lib_name:None
          ~flags
          ~scope
          pps
      in
      Dyn.list
        Dyn.string
        [ Path.to_absolute_filename (Path.build exe) :: "--as-ppx" :: flags
          |> String.concat ~sep:" "
        ]
    in
    (* Set Clflags.all_ppx for dune utop, and Compenv.first_ppx for custom
       toplevels because Topmain.main() resets Clflags.all_ppx. *)
    Pp.vbox ~indent:2 (Pp.verbatim "Clflags.all_ppx :=" ++ Pp.cut ++ Dyn.pp ppx)
    ++ Pp.verbatim ";"
    ++ Pp.newline
    ++ Pp.verbatim "Compenv.first_ppx :="
    ++ Pp.cut
    ++ Dyn.pp ppx
    ++ Pp.verbatim ";"
    ++ Pp.newline
  | Action _ | Future_syntax _ -> assert false (* Error in parsing *)
  | No_preprocessing -> Action_builder.return Pp.nop
;;

let setup_module_rules t =
  let main_ml =
    (let open Action_builder.O in
     let+ pp =
       let* pp_dirs =
         let+ include_dirs =
           let lib_config = (Compilation_context.ocaml t.cctx).lib_config in
           let+ libs =
             let requires_compile = Compilation_context.requires_compile t.cctx in
             Resolve.Memo.read requires_compile
           in
           Lib_flags.L.include_paths libs (Ocaml Byte) lib_config
           |> Lib_flags.L.include_only
         in
         Source.pp_ml t.source ~include_dirs
       in
       let+ pp_ppx = pp_flags t in
       Pp.seq pp_ppx pp_dirs
     in
     Format.asprintf "%a@." Pp.to_fmt pp)
    |> Action_builder.write_file_dyn (Source.source_path t.source)
  in
  let sctx = Compilation_context.super_context t.cctx in
  let dir = Compilation_context.dir t.cctx in
  Super_context.add_rule sctx ~dir main_ml
;;

let setup_rules_and_return_exe_path t ~linkage =
  let program = Source.program t.source in
  let* (_ : Exe.dep_graphs) =
    Exe.build_and_link
      t.cctx
      ~program
      ~linkages:[ linkage ]
      ~link_args:
        (Action_builder.return (Command.Args.As [ "-linkall"; "-warn-error"; "-31" ]))
      ~promote:None
  in
  let+ () = setup_module_rules t in
  Exe.exe_path t.cctx ~program ~linkage
;;

let setup_rules t ~linkage =
  Memo.map (setup_rules_and_return_exe_path t ~linkage) ~f:ignore
;;

type directives =
  { include_paths : Path.Set.t
  ; files_to_load : Path.t list
  ; uses : Path.t list
  ; pp : string option
  ; ppx : string option
  ; code : string list
  }

let print_toplevel_init_file { include_paths; files_to_load; uses; pp; ppx; code } =
  Path.Set.iter include_paths ~f:(fun p ->
    Printf.printf "#directory %S;;\n" (Path.to_absolute_filename p));
  List.iter files_to_load ~f:(fun p ->
    Printf.printf "#load %S;;\n" (Path.to_absolute_filename p));
  Option.iter pp ~f:(Printf.printf "#pp %S;;\n");
  Option.iter ppx ~f:(Printf.printf "#ppx %S;;\n");
  List.iter uses ~f:(fun p -> Printf.printf "#use %S;;\n" (Path.to_absolute_filename p));
  match code with
  | [] -> ()
  | code ->
    List.iter code ~f:print_endline;
    print_endline ";;"
;;

module Stanza = struct
  let setup ~sctx ~dir ~(toplevel : Toplevel_stanza.t) =
    let source = Source.of_stanza ~dir ~toplevel in
    let* exe =
      let* scope = Scope.DB.find_by_dir dir in
      let* expander = Super_context.expander sctx ~dir in
      let* cctx =
        let dune_version = Scope.project scope |> Dune_project.dune_version in
        let* preprocessing =
          let preprocess = Module_name.Per_item.for_all toplevel.pps in
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
        let* modules = Source.modules source preprocessing in
        let compile_info =
          let compiler_libs =
            Lib_name.parse_string_exn (source.loc, "compiler-libs.toplevel")
          in
          let names = Nonempty_list.[ source.loc, source.name ] in
          let pps =
            match toplevel.pps with
            | Preprocess.Pps pps -> pps.pps
            | Action _ | Future_syntax _ -> assert false (* Error in parsing *)
            | No_preprocessing -> []
          in
          Lib.DB.resolve_user_written_deps
            (Scope.libs scope)
            (`Exe names)
            ~forbidden_libraries:[]
            (Lib_dep.Direct (source.loc, compiler_libs)
             :: List.map toplevel.libraries ~f:(fun d -> Lib_dep.Direct d))
            ~pps
            ~dune_version
            ~allow_overlaps:false
        in
        let flags =
          let profile = Super_context.context sctx |> Context.profile in
          Ocaml_flags.append_common
            (Ocaml_flags.default ~dune_version ~profile)
            [ "-w"; "-24" ]
        in
        let requires_compile = Lib.Compile.direct_requires compile_info in
        let requires_link = Lib.Compile.requires_link compile_info in
        let obj_dir = Source.obj_dir source in
        Compilation_context.create
          ()
          ~super_context:sctx
          ~scope
          ~obj_dir
          ~modules
          ~opaque:(Explicit false)
          ~requires_compile
          ~requires_link
          ~flags
          ~js_of_ocaml:(Js_of_ocaml.Mode.Pair.make None)
          ~melange_package_name:None
          ~package:None
          ~preprocessing
      in
      let linkage = Exe.Linkage.custom (Compilation_context.ocaml cctx).version in
      let resolved = make ~cctx ~source ~preprocess:toplevel.pps expander in
      setup_rules_and_return_exe_path resolved ~linkage
    in
    let symlink = Path.Build.relative dir (Path.Build.basename exe) in
    Super_context.add_rule
      sctx
      ~dir
      ~loc:source.loc
      (Action_builder.symlink ~src:(Path.build exe) ~dst:symlink)
  ;;
end
