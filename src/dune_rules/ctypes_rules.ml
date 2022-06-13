open Import

(* This module expands either a (library ... (ctypes ...)) rule or an
   (executables ... (ctypes ...)) rule into the generated set of .ml and .c
   files needed to conveniently write OCaml bindings for C libraries.

   Aside from perhaps providing an "header.h" include line, you should be able
   to wrap an entire C library without writing a single line of C code.

   This stanza requires the user to specify the names of 4 (or more) modules:

   (type_description Type_description) (generated_types Types_generated)
   (function_description Function_description) ; can be repeated
   (generated_entry_point C)

   The user must also implement two of the modules:

   (1) $type_description.ml must have the following top-level functor:

   module Types (T : Ctypes.TYPE) = struct (* put calls to Ctypes.TYPE.constant
   and Ctypes.TYPE.typedef here to wrap C constants and structs *) end

   (2) $function_description.ml must have the following two definitions:

   modules Types = $generated_types

   module Functions (F : Ctypes.FOREIGN) = struct (* put calls to F.foreign here
   to wrap C functions *) end

   Once the above modules are provided, the ctypes stanza will:

   * generate a types generator

   * generate a functions generator

   * set up a discovery program to query pkg-config for compile and link flags,
   if you decided to use pkg-config instead of vendored c-flags

   * use the types/data and functions modules you filled in with a functor to
   tie everything into your library.

   The user must also specify the name of a final "Entry point" output module
   ($generated_entry_point) that will be generated and added to your executable
   or library. Suggest calling it "C" and accessing the instantiated functors
   from your project as C.Types and C.Functions.

   It may help to view a real world example of all of the boilerplate that is
   being replaced by a [ctypes] stanza:

   https://github.com/mbacarella/mpg123/blob/077a72d922931eb46d4b4e5842b0426fa3c161b5/c/dune

   This implementation is not, however, a naive translation of the boilerplate
   above. This module uses dune internal features to simplify the stub
   generation. As a result, there are no intermediate libraries (or
   packages). *)

module Buildable = Dune_file.Buildable
module Library = Dune_file.Library
module Ctypes = Ctypes_stanza

let modules_of_list ~dir ~modules =
  let name_map =
    let build_dir = Path.build dir in
    let modules =
      List.map modules ~f:(fun name ->
          let module_name = Module_name.of_string name in
          let path =
            Path.relative build_dir (Ctypes.ml_of_module_name module_name)
          in
          let impl = Module.File.make Dialect.ocaml path in
          let source = Module.Source.make ~impl module_name in
          Module.of_source ~visibility:Public ~kind:Impl source)
    in
    Module.Name_map.of_list_exn modules
  in
  Modules.exe_unwrapped name_map
(* Modules.exe_wrapped ~src_dir:dir ~modules:name_map *)

let pp_write_file path pp =
  Action_builder.write_file path @@ Format.asprintf "%a" Pp.to_fmt pp

let verbatimf fmt =
  Printf.ksprintf (fun s -> Pp.concat [ Pp.verbatim s; Pp.newline ]) fmt

let write_c_types_includer_module ~sctx ~dir ~filename ~type_description_functor
    ~c_generated_types_module =
  let path = Path.Build.relative dir filename in
  let contents =
    verbatimf "include %s.Types (%s)"
      (Module_name.to_string type_description_functor)
      (Module_name.to_string c_generated_types_module)
  in
  Super_context.add_rule ~loc:Loc.none sctx ~dir (pp_write_file path contents)

let write_entry_point_module ~ctypes ~sctx ~dir ~filename
    ~type_description_instance ~function_description ~c_types_includer_module =
  let path = Path.Build.relative dir filename in
  let contents =
    Pp.concat
      [ verbatimf "module %s = %s"
          (Module_name.to_string type_description_instance)
          (Module_name.to_string c_types_includer_module)
      ; Pp.concat_map function_description ~f:(fun fd ->
            let c_generated_functions_module =
              Ctypes.c_generated_functions_module ctypes fd
            in
            verbatimf "module %s = %s.Functions (%s)"
              (fd.instance |> Module_name.to_string)
              (fd.functor_ |> Module_name.to_string)
              (Module_name.to_string c_generated_functions_module))
      ]
  in
  Super_context.add_rule ~loc:Loc.none sctx ~dir (pp_write_file path contents)

let discover_gen ~external_library_name:lib ~cflags_sexp ~c_library_flags_sexp =
  Pp.concat
    [ verbatimf "module C = Configurator.V1"
    ; verbatimf "let () ="
    ; verbatimf "  C.main ~name:\"%s\" (fun c ->"
        (External_lib_name.to_string lib)
    ; verbatimf "    let default : C.Pkg_config.package_conf ="
    ; verbatimf "      { libs   = [\"-l%s\"];" (External_lib_name.to_string lib)
    ; verbatimf "        cflags = [\"-I/usr/include\"] }"
    ; verbatimf "    in"
    ; verbatimf "    let conf ="
    ; verbatimf "      match C.Pkg_config.get c with"
    ; verbatimf "      | None -> default"
    ; verbatimf "      | Some pc ->"
    ; verbatimf "        match C.Pkg_config.query pc ~package:\"%s\" with"
        (External_lib_name.to_string lib)
    ; verbatimf "        | None -> default"
    ; verbatimf "        | Some deps -> deps"
    ; verbatimf "    in"
    ; verbatimf "    C.Flags.write_sexp \"%s\" conf.cflags;" cflags_sexp
    ; verbatimf "    C.Flags.write_sexp \"%s\" conf.libs;" c_library_flags_sexp
    ; verbatimf "  )"
    ]

let write_discover_script ~filename ~sctx ~dir ~external_library_name
    ~cflags_sexp ~c_library_flags_sexp =
  let path = Path.Build.relative dir filename in
  let script =
    discover_gen ~external_library_name ~cflags_sexp ~c_library_flags_sexp
  in
  Super_context.add_rule ~loc:Loc.none sctx ~dir (pp_write_file path script)

let gen_headers ~expander (headers : Ctypes.Headers.t) =
  let open Action_builder.O in
  match headers with
  | Include lst ->
    let+ lst =
      Expander.expand_and_eval_set expander lst
        ~standard:(Action_builder.return [])
    in
    Pp.concat_map lst ~f:(fun h ->
        verbatimf "  print_endline \"#include <%s>\";" h)
  | Preamble s ->
    let+ s = Expander.expand_str expander s in
    verbatimf "  print_endline %S;" s

let type_gen_gen ~expander ~headers ~type_description_functor =
  let open Action_builder.O in
  let+ headers = gen_headers ~expander headers in
  Pp.concat
    [ verbatimf "let () ="
    ; headers
    ; verbatimf "  Cstubs_structs.write_c Format.std_formatter"
    ; verbatimf "    (module %s.Types)"
        (Module_name.to_string type_description_functor)
    ]

let function_gen_gen ~expander ~(concurrency : Ctypes.Concurrency_policy.t)
    ~errno_policy ~headers ~function_description_functor =
  let open Action_builder.O in
  let module_name = Module_name.to_string function_description_functor in
  let concurrency =
    match concurrency with
    | Unlocked -> "Cstubs.unlocked"
    | Sequential -> "Cstubs.sequential"
    | Lwt_jobs -> "Cstubs.lwt_jobs"
    | Lwt_preemptive -> "Cstubs.lwt_preemptive"
  in
  let errno_policy =
    match errno_policy with
    | Ctypes.Errno_policy.Ignore_errno -> "Cstubs.ignore_errno"
    | Ctypes.Errno_policy.Return_errno -> "Cstubs.return_errno"
  in
  let+ headers = gen_headers ~expander headers in
  Pp.concat
    [ verbatimf "let () ="
    ; verbatimf "  let concurrency = %s in" concurrency
    ; verbatimf "  let errno = %s in" errno_policy
    ; verbatimf "  let prefix = Sys.argv.(2) in"
    ; verbatimf "  match Sys.argv.(1) with"
    ; verbatimf "  | \"ml\" ->"
    ; verbatimf "    Cstubs.write_ml ~concurrency Format.std_formatter ~prefix"
    ; verbatimf "      ~errno"
    ; verbatimf "      (module %s.Functions)" module_name
    ; verbatimf "  | \"c\" ->"
    ; headers
    ; verbatimf "    Cstubs.write_c ~concurrency Format.std_formatter ~prefix"
    ; verbatimf "      ~errno"
    ; verbatimf "      (module %s.Functions)" module_name
    ; verbatimf "  | s -> failwith (\"unknown functions \"^s)"
    ]

let add_rule_gen ~sctx ~dir ~filename f =
  let path = Path.Build.relative dir filename in
  let script =
    let open Action_builder.O in
    let* expander =
      Action_builder.of_memo @@ Super_context.expander sctx ~dir
    in
    let+ pp = f ~expander in
    Format.asprintf "%a" Pp.to_fmt pp
  in
  let action =
    Action_builder.With_targets.write_file_dyn path
      (Action_builder.with_no_targets script)
  in
  Super_context.add_rule ~loc:Loc.none sctx ~dir action

let write_type_gen_script ~headers ~dir ~filename ~sctx
    ~type_description_functor =
  add_rule_gen ~dir ~filename ~sctx
    (type_gen_gen ~headers ~type_description_functor)

let write_function_gen_script ~headers ~sctx ~dir ~name
    ~function_description_functor ~concurrency ~errno_policy =
  add_rule_gen ~dir ~filename:(name ^ ".ml") ~sctx
    (function_gen_gen ~concurrency ~errno_policy ~headers
       ~function_description_functor)

let rule ?(deps = []) ?stdout_to ?(args = []) ?(targets = []) ~exe ~sctx ~dir ()
    =
  let build =
    let exe = Ok (Path.build (Path.Build.relative dir exe)) in
    let args =
      let targets = List.map targets ~f:(Path.Build.relative dir) in
      let deps =
        List.map deps ~f:(Path.relative (Path.build dir)) |> Dep.Set.of_files
      in
      let open Command.Args in
      [ Hidden_targets targets; Hidden_deps deps; As args ]
    in
    let stdout_to = Option.map stdout_to ~f:(Path.Build.relative dir) in
    Command.run exe ~dir:(Path.build dir) ?stdout_to args
  in
  Super_context.add_rule sctx ~dir build

let build_c_program ~foreign_archives_deps ~sctx ~dir ~source_files ~scope
    ~cflags_sexp ~output ~deps () =
  let ctx = Super_context.context sctx in
  let open Memo.O in
  let* exe =
    Ocaml_config.c_compiler ctx.ocaml_config
    |> Super_context.resolve_program ~loc:None ~dir sctx
  in
  let include_args =
    let ocaml_where = Path.to_string ctx.stdlib_dir in
    (* XXX: need glob dependency *)
    let open Resolve.Memo.O in
    let+ ctypes_include_dirs =
      let+ lib =
        let ctypes = Lib_name.of_string "ctypes" in
        Lib.DB.resolve (Scope.libs scope) (Loc.none, ctypes)
        (* | Ok lib -> lib | Error _res -> User_error.raise [ Pp.textf "the
           'ctypes' library needs to be installed to use the ctypes stanza"] *)
      in
      Lib_flags.L.include_paths [ lib ] Mode.Native
      |> Path.Set.to_list |> List.map ~f:Path.to_string
    in
    let include_dirs = ocaml_where :: ctypes_include_dirs in
    List.concat_map include_dirs ~f:(fun dir -> [ "-I"; dir ])
  in
  let deps =
    let source_file_deps =
      List.map source_files ~f:(Path.relative (Path.build dir))
      |> Dep.Set.of_files
    in

    let foreign_archives_deps =
      List.map foreign_archives_deps ~f:Path.build |> Dep.Set.of_files
    in
    let open Action_builder.O in
    let* () =
      Dep.Set.union source_file_deps foreign_archives_deps
      |> Action_builder.deps
    in
    deps
  in
  let build =
    let cflags_args =
      let contents =
        Action_builder.contents (Path.relative (Path.build dir) cflags_sexp)
      in
      Action_builder.map contents ~f:(fun sexp ->
          let fail s = User_error.raise [ Pp.textf s ] in
          let ast =
            Dune_lang.Parser.parse_string ~mode:Dune_lang.Parser.Mode.Single
              ~fname:cflags_sexp sexp
          in
          match ast with
          | Atom (_loc, atom) -> [ Dune_lang.Atom.to_string atom ]
          | Template _ -> fail "'template' not supported in ctypes c_flags"
          | Quoted_string (_loc, s) -> [ s ]
          | List (_loc, lst) ->
            List.map lst ~f:(function
              | Dune_lang.Ast.Atom (_loc, atom) -> Dune_lang.Atom.to_string atom
              | Quoted_string (_loc, s) -> s
              | Template _ -> fail "'template' not supported in ctypes c_flags"
              | List _ -> fail "nested lists not supported in ctypes c_flags"))
    in
    let absolute_path_hack p =
      (* These normal path builder things construct relative paths like
         _build/default/your/project/file.c but before dune runs gcc it actually
         cds into _build/default, which fails, so we turn them into absolutes to
         hack around it. *)
      Path.relative (Path.build dir) p |> Path.to_absolute_filename
    in
    let action =
      let open Action_builder.O in
      let* include_args = Resolve.Memo.read include_args in
      deps
      >>> Action_builder.map cflags_args ~f:(fun cflags_args ->
              let source_files = List.map source_files ~f:absolute_path_hack in
              let output = absolute_path_hack output in
              let args =
                cflags_args @ include_args @ source_files @ [ "-o"; output ]
              in
              Action.run exe args)
    in
    Action_builder.with_file_targets action
      ~file_targets:[ Path.Build.relative dir output ]
  in
  Super_context.add_rule sctx ~dir
    (Action_builder.With_targets.map ~f:Action.Full.make build)

let cctx_with_substitutions ?(libraries = []) ~modules ~dir ~loc ~scope ~cctx ()
    =
  let compile_info =
    let dune_version = Scope.project scope |> Dune_project.dune_version in
    Lib.DB.resolve_user_written_deps_for_exes (Scope.libs scope)
      [ (loc, "ctypes") ]
      (Ctypes.lib_deps_of_strings ~loc libraries)
      ~dune_version ~pps:[]
  in
  let modules = modules_of_list ~dir ~modules in
  let module Cctx = Compilation_context in
  Cctx.create ~super_context:(Cctx.super_context cctx) ~scope:(Cctx.scope cctx)
    ~expander:(Cctx.expander cctx) ~js_of_ocaml:(Cctx.js_of_ocaml cctx)
    ~package:(Cctx.package cctx) ~flags:(Cctx.flags cctx)
    ~requires_compile:(Lib.Compile.direct_requires compile_info)
    ~requires_link:(Lib.Compile.requires_link compile_info)
    ~obj_dir:(Cctx.obj_dir cctx)
    ~opaque:(Cctx.Explicit (Cctx.opaque cctx))
    ~modules ()

let program_of_module_and_dir ~dir program =
  let build_dir = Path.build dir in
  { Exe.Program.name = program
  ; main_module_name = Module_name.of_string program
  ; loc = Loc.in_file (Path.relative build_dir program)
  }

let exe_build_and_link ?libraries ?(modules = []) ~scope ~loc ~dir ~cctx
    ~sandbox program =
  let open Memo.O in
  let* cctx =
    cctx_with_substitutions ?libraries ~loc ~scope ~dir ~cctx
      ~modules:(program :: modules) ()
  in
  let program = program_of_module_and_dir ~dir program in
  Exe.build_and_link ~program ~linkages:[ Exe.Linkage.native ] ~promote:None
    ~sandbox cctx

let exe_link_only ~dir ~shared_cctx ~sandbox program ~deps =
  let link_args =
    let open Action_builder.O in
    let+ () = deps in
    Command.Args.empty
  in
  let program = program_of_module_and_dir ~dir program in
  Exe.link_many ~link_args ~programs:[ program ]
    ~linkages:[ Exe.Linkage.native ] ~promote:None shared_cctx ~sandbox

let write_osl_to_sexp_file ~sctx ~dir ~filename ~expand_flag flags =
  let build =
    let sexp =
      let open Action_builder.O in
      let* expander =
        Action_builder.of_memo @@ Super_context.expander sctx ~dir
      in
      let+ flags = expand_flag ~expander flags in
      let sexp = Sexp.List (List.map ~f:(fun x -> Sexp.Atom x) flags) in
      Sexp.to_string sexp
    in
    let path = Path.Build.relative dir filename in
    Action_builder.write_file_dyn path sexp
  in
  Super_context.add_rule ~loc:Loc.none sctx ~dir build

let gen_rules ~cctx ~(buildable : Buildable.t) ~loc ~scope ~dir ~sctx =
  let ctypes = Option.value_exn buildable.ctypes in
  let external_library_name = ctypes.external_library_name in
  let type_description_functor = ctypes.type_description.functor_ in
  let c_types_includer_module = ctypes.generated_types in
  let c_generated_types_module = Ctypes.c_generated_types_module ctypes in
  let rule = rule ~sctx ~dir in
  let open Memo.O in
  let foreign_archives_deps =
    let ctx = Super_context.context sctx in
    let ext_lib = ctx.lib_config.ext_lib in
    let ext_dll = ctx.lib_config.ext_dll in
    List.concat_map buildable.foreign_archives ~f:(fun (_loc, archive) ->
        [ Foreign.Archive.lib_file ~archive ~dir ~ext_lib
        ; Foreign.Archive.dll_file ~archive ~dir ~ext_dll
        ])
  in
  let* expander = Super_context.expander sctx ~dir in
  let deps, sandbox = Dep_conf_eval.unnamed ~expander ctypes.deps in
  let* () =
    write_c_types_includer_module ~sctx ~dir
      ~filename:(Ctypes.ml_of_module_name c_types_includer_module)
      ~c_generated_types_module ~type_description_functor
  in
  (* The output of this process is to generate a cflags sexp and a c library
     flags sexp file. We can probe these flags by using the system pkg-config,
     if it's an external system library. The user could also tell us what they
     are, if the library is vendored.

     https://dune.readthedocs.io/en/stable/quick-start.html#defining-a-library-with-c-stubs-using-pkg-config *)
  let c_library_flags_sexp = Ctypes.c_library_flags_sexp ctypes in
  let cflags_sexp = Ctypes.cflags_sexp ctypes in
  let* () =
    match ctypes.build_flags_resolver with
    | Vendored { c_flags; c_library_flags } ->
      let* () =
        write_osl_to_sexp_file ~sctx ~dir ~filename:cflags_sexp c_flags
          ~expand_flag:(fun ~expander flags ->
            Super_context.foreign_flags sctx ~dir ~expander ~flags ~language:C)
      in
      write_osl_to_sexp_file ~sctx ~dir ~filename:c_library_flags_sexp
        c_library_flags ~expand_flag:(fun ~expander flags ->
          Expander.expand_and_eval_set expander flags
            ~standard:(Action_builder.return []))
    | Pkg_config ->
      let cflags_sexp = Ctypes.cflags_sexp ctypes in
      let discover_script =
        sprintf "%s__ctypes_discover"
          (ctypes.external_library_name |> External_lib_name.clean
         |> External_lib_name.to_string)
      in
      let* () =
        write_discover_script ~sctx ~dir ~filename:(discover_script ^ ".ml")
          ~cflags_sexp ~c_library_flags_sexp ~external_library_name
      in
      let* () =
        exe_build_and_link ~scope ~loc ~dir ~cctx ~sandbox
          ~libraries:[ "dune.configurator" ] discover_script
      in
      rule
        ~targets:[ cflags_sexp; c_library_flags_sexp ]
        ~exe:(discover_script ^ ".exe") ()
  in
  let generated_entry_module = ctypes.generated_entry_point in
  let headers = ctypes.headers in
  let exe_link_only = exe_link_only ~deps ~dir ~shared_cctx:cctx ~sandbox in
  (* Type_gen produces a .c file, taking your type description module above as
     an input. The .c file is compiled into an .exe. The .exe, when run produces
     an .ml file. The .ml file is compiled into a module that will have the
     user's ML-wrapped C data/types.

     Note the similar function_gen process below depends on the ML-wrapped C
     data/types produced in this step. *)
  let* () =
    let c_generated_types_cout_c =
      sprintf "%s__c_cout_generated_types.c"
        (External_lib_name.to_string external_library_name)
    in
    let c_generated_types_cout_exe =
      sprintf "%s__c_cout_generated_types.exe"
        (External_lib_name.to_string external_library_name)
    in
    let type_gen_script = Ctypes.type_gen_script ctypes in
    let* () =
      write_type_gen_script ~headers ~sctx ~dir
        ~filename:(type_gen_script ^ ".ml") ~type_description_functor
    in
    let* () = exe_link_only type_gen_script in
    let* () =
      rule ~stdout_to:c_generated_types_cout_c ~exe:(type_gen_script ^ ".exe")
        ()
    in
    let* () =
      build_c_program ~foreign_archives_deps ~sctx ~dir ~scope ~cflags_sexp
        ~source_files:[ c_generated_types_cout_c ]
        ~output:c_generated_types_cout_exe ~deps ()
    in
    rule
      ~stdout_to:(c_generated_types_module |> Ctypes.ml_of_module_name)
      ~exe:c_generated_types_cout_exe ()
  in
  (* Function_gen is similar to type_gen above, though it produces both an .ml
     file and a .c file. These files correspond to the files you would have to
     write by hand to wrap C code (if ctypes didn't exist!)

     Also the user can repeat the 'function_description' stanza to do this more
     than once. This is needed for generating blocking and non-blocking sets of
     functions, for example, which requires a different 'concurrency' parameter
     in the code generator. *)
  let* () =
    Memo.parallel_iter ctypes.function_description ~f:(fun fd ->
        let stubs_prefix =
          External_lib_name.(external_library_name |> clean |> to_string)
          ^ "_stubs"
        in
        let c_generated_functions_cout_c =
          Ctypes.c_generated_functions_cout_c ctypes fd
        in
        let function_gen_script = Ctypes.function_gen_script ctypes fd in
        let* () =
          write_function_gen_script ~headers ~sctx ~dir
            ~name:function_gen_script ~concurrency:fd.concurrency
            ~errno_policy:fd.errno_policy
            ~function_description_functor:fd.functor_
        in
        let* () = exe_link_only function_gen_script in
        let* () =
          rule ~stdout_to:c_generated_functions_cout_c
            ~exe:(function_gen_script ^ ".exe")
            ~args:[ "c"; stubs_prefix ] ()
        in
        rule
          ~stdout_to:
            (Ctypes.c_generated_functions_module ctypes fd
            |> Ctypes.ml_of_module_name)
          ~exe:(function_gen_script ^ ".exe")
          ~args:[ "ml"; stubs_prefix ] ())
  in
  (* The entry point module binds the instantiated Types and Functions functors
     to the entry point module name and instances the user specified. *)
  write_entry_point_module ~ctypes ~sctx ~dir
    ~filename:(generated_entry_module |> Ctypes.ml_of_module_name)
    ~type_description_instance:ctypes.type_description.instance
    ~function_description:ctypes.function_description ~c_types_includer_module

let ctypes_cclib_flags ~standard ~scope ~expander ~(buildable : Buildable.t) =
  match buildable.ctypes with
  | None -> standard
  | Some ctypes ->
    let ctypes_c_library_flags =
      let path_to_sexp_file =
        Ctypes_stubs.c_library_flags
          ~external_library_name:ctypes.external_library_name
      in
      let parsing_context =
        let project = Scope.project scope in
        Dune_project.parsing_context project
      in
      Ordered_set_lang.Unexpanded.include_single ~context:parsing_context
        ~pos:("", 0, 0, 0) path_to_sexp_file
    in
    Expander.expand_and_eval_set expander ctypes_c_library_flags ~standard
