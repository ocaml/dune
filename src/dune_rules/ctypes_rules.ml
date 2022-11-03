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

let verbatimf fmt =
  Printf.ksprintf (fun s -> Pp.concat [ Pp.verbatim s; Pp.newline ]) fmt

let write_c_types_includer_module ~type_description_functor
    ~c_generated_types_module =
  let contents =
    verbatimf "include %s.Types (%s)"
      (Module_name.to_string type_description_functor)
      (Module_name.to_string c_generated_types_module)
  in
  Format.asprintf "%a@." Pp.to_fmt contents

let write_entry_point_module ~ctypes ~type_description_instance
    ~function_description ~c_types_includer_module =
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
  Format.asprintf "%a@." Pp.to_fmt contents

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
  Format.asprintf "%a@." Pp.to_fmt
    (Pp.concat
       [ verbatimf "let () ="
       ; headers
       ; verbatimf "  Cstubs_structs.write_c Format.std_formatter"
       ; verbatimf "    (module %s.Types)"
           (Module_name.to_string type_description_functor)
       ])

let function_gen_gen ~expander ~(concurrency : Ctypes.Concurrency_policy.t)
    ~(errno_policy : Ctypes.Errno_policy.t) ~headers
    ~function_description_functor =
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
    | Ignore_errno -> "Cstubs.ignore_errno"
    | Return_errno -> "Cstubs.return_errno"
  in
  let+ headers = gen_headers ~expander headers in
  Format.asprintf "%a@." Pp.to_fmt
    (Pp.concat
       [ verbatimf "let () ="
       ; verbatimf "  let concurrency = %s in" concurrency
       ; verbatimf "  let errno = %s in" errno_policy
       ; verbatimf "  let prefix = Sys.argv.(2) in"
       ; verbatimf "  match Sys.argv.(1) with"
       ; verbatimf "  | \"ml\" ->"
       ; verbatimf
           "    Cstubs.write_ml ~concurrency Format.std_formatter ~prefix"
       ; verbatimf "      ~errno"
       ; verbatimf "      (module %s.Functions)" module_name
       ; verbatimf "  | \"c\" ->"
       ; headers
       ; verbatimf
           "    Cstubs.write_c ~concurrency Format.std_formatter ~prefix"
       ; verbatimf "      ~errno"
       ; verbatimf "      (module %s.Functions)" module_name
       ; verbatimf "  | s -> failwith (\"unknown functions \"^s)"
       ])

let build_c_program ~foreign_archives_deps ~sctx ~dir ~source_files ~scope
    ~cflags ~output ~deps =
  let ctx = Super_context.context sctx in
  let open Memo.O in
  let* exe =
    Ocaml_config.c_compiler ctx.ocaml_config
    |> Super_context.resolve_program ~loc:None ~dir sctx
  in
  let project = Scope.project scope in
  let with_user_and_std_flags =
    let base_flags =
      let use_standard_flags =
        Dune_project.use_standard_c_and_cxx_flags project
      in
      let cfg = ctx.ocaml_config in
      match use_standard_flags with
      | Some true -> Fdo.c_flags ctx
      | None | Some false ->
        (* In dune < 2.8 flags from ocamlc_config are always added *)
        List.concat
          [ Ocaml_config.ocamlc_cflags cfg
          ; Ocaml_config.ocamlc_cppflags cfg
          ; Fdo.c_flags ctx
          ]
    in
    let open Action_builder.O in
    let* expander = Action_builder.of_memo (Super_context.expander sctx ~dir) in
    Super_context.foreign_flags sctx ~dir ~expander
      ~flags:Ordered_set_lang.Unexpanded.standard ~language:C
    |> Action_builder.map ~f:(List.append base_flags)
  in
  let include_args =
    let ocaml_where = Path.to_string ctx.stdlib_dir in
    (* XXX: need glob dependency *)
    let open Resolve.Memo.O in
    let+ ctypes_include_dirs =
      let+ lib =
        let ctypes = Lib_name.of_string "ctypes" in
        Lib.DB.resolve (Scope.libs scope) (Loc.none, ctypes)
      in
      Lib_flags.L.include_paths [ lib ] (Ocaml Native)
      |> Path.Set.to_list_map ~f:Path.to_string
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
      let* base_args = with_user_and_std_flags in
      deps
      >>> Action_builder.map cflags ~f:(fun cflags_args ->
              let source_files = List.map source_files ~f:absolute_path_hack in
              let output = absolute_path_hack output in
              let args =
                base_args @ cflags_args @ include_args @ source_files
                @ [ "-o"; output ]
              in
              Action.run exe args)
    in
    Action_builder.with_file_targets action
      ~file_targets:[ Path.Build.relative dir output ]
  in
  Super_context.add_rule sctx ~dir
    (Action_builder.With_targets.map ~f:Action.Full.make build)

let program_of_module_and_dir ~dir program =
  let build_dir = Path.build dir in
  { Exe.Program.name = program
  ; main_module_name = Module_name.of_string program
  ; loc = Loc.in_file (Path.relative build_dir program)
  }

let exe_link_only ~dir ~shared_cctx ~sandbox program ~deps =
  let link_args =
    let open Action_builder.O in
    let+ () = deps in
    Command.Args.empty
  in
  let program = program_of_module_and_dir ~dir program in
  Exe.link_many ~link_args ~programs:[ program ]
    ~linkages:[ Exe.Linkage.native ] ~promote:None shared_cctx ~sandbox

let gen_rules ~cctx ~(buildable : Buildable.t) ~loc ~scope ~dir ~sctx =
  let ctypes = Option.value_exn buildable.ctypes in
  let external_library_name = ctypes.external_library_name in
  let type_description_functor = ctypes.type_description.functor_ in
  let c_types_includer_module = ctypes.generated_types in
  let c_generated_types_module = Ctypes.c_generated_types_module ctypes in
  let open Memo.O in
  let foreign_archives_deps =
    let ctx = Super_context.context sctx in
    let ext_lib = ctx.lib_config.ext_lib in
    let ext_dll = ctx.lib_config.ext_dll in
    List.concat_map buildable.foreign_archives ~f:(fun (_loc, archive) ->
        let mode = Mode.Select.All in
        [ Foreign.Archive.lib_file ~mode ~archive ~dir ~ext_lib
        ; Foreign.Archive.dll_file ~mode ~archive ~dir ~ext_dll
        ])
  in
  let* expander = Super_context.expander sctx ~dir in
  let deps, sandbox = Dep_conf_eval.unnamed ~expander ctypes.deps in
  let* () =
    Super_context.add_rule sctx ~loc:Loc.none ~dir
    @@
    let target =
      Path.Build.relative dir (Ctypes.ml_of_module_name c_types_includer_module)
    in
    Action_builder.write_file target
      (write_c_types_includer_module ~c_generated_types_module
         ~type_description_functor)
  in
  (* The output of this process is to generate a cflags sexp and a c library
     flags sexp file. We can probe these flags by using the system pkg-config,
     if it's an external system library. The user could also tell us what they
     are, if the library is vendored.

     https://dune.readthedocs.io/en/stable/quick-start.html#defining-a-library-with-c-stubs-using-pkg-config *)
  let* cflags =
    match ctypes.build_flags_resolver with
    | Vendored { c_flags; c_library_flags = _ } ->
      Super_context.foreign_flags sctx ~dir ~expander ~flags:c_flags ~language:C
      |> Memo.return
    | Pkg_config ->
      let+ () =
        let open Memo.O in
        let setup query =
          let* res = Pkg_config.gen_rule sctx ~dir ~loc query in
          match res with
          | Ok () -> Memo.return ()
          | Error `Not_found -> Memo.return ()
        in
        let lib = External_lib_name.to_string external_library_name in
        let* () = setup (Libs lib) in
        setup (Cflags lib)
      in
      Pkg_config.Query.read ~dir
        (Cflags (External_lib_name.to_string external_library_name))
        sctx
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
      Super_context.add_rule ~loc:Loc.none sctx ~dir
      @@
      let script = type_gen_gen ~headers ~type_description_functor ~expander in
      let target = Path.Build.relative dir (type_gen_script ^ ".ml") in
      Action_builder.With_targets.write_file_dyn target
        (Action_builder.with_no_targets script)
    in
    let* (_ : Exe.dep_graphs) = exe_link_only type_gen_script in
    let* () =
      Super_context.add_rule sctx ~dir ~loc:Loc.none
        (let exe =
           Ok (Path.build (Path.Build.relative dir (type_gen_script ^ ".exe")))
         in
         let stdout_to = Path.Build.relative dir c_generated_types_cout_c in
         Command.run ~stdout_to ~dir:(Path.build dir) exe [])
    in
    let* () =
      build_c_program ~foreign_archives_deps ~sctx ~dir ~scope
        ~source_files:[ c_generated_types_cout_c ]
        ~output:c_generated_types_cout_exe ~deps ~cflags
    in
    Super_context.add_rule sctx ~loc:Loc.none ~dir
      (let stdout_to =
         Path.Build.relative dir
           (c_generated_types_module |> Ctypes.ml_of_module_name)
       in
       let exe =
         Ok (Path.build (Path.Build.relative dir c_generated_types_cout_exe))
       in
       Command.run ~stdout_to ~dir:(Path.build dir) exe [])
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
          Super_context.add_rule ~loc:Loc.none sctx ~dir
          @@
          let target = Path.Build.relative dir (function_gen_script ^ ".ml") in
          let script =
            function_gen_gen ~concurrency:fd.concurrency
              ~errno_policy:fd.errno_policy ~headers
              ~function_description_functor:fd.functor_ ~expander
          in
          Action_builder.With_targets.write_file_dyn target
            (Action_builder.with_no_targets script)
        in
        let* (_ : Exe.dep_graphs) = exe_link_only function_gen_script in
        let exe =
          Ok
            (Path.build
               (Path.Build.relative dir (function_gen_script ^ ".exe")))
        in
        let command ~stdout_to =
          Command.run ~stdout_to ~dir:(Path.build dir) exe
        in
        let* () =
          Super_context.add_rule sctx ~dir ~loc:Loc.none
            (let stdout_to =
               Path.Build.relative dir c_generated_functions_cout_c
             in
             command ~stdout_to [ A "c"; A stubs_prefix ])
        in
        Super_context.add_rule sctx ~dir ~loc:Loc.none
          (let stdout_to =
             Path.Build.relative dir
               (Ctypes.c_generated_functions_module ctypes fd
               |> Ctypes.ml_of_module_name)
           in
           command ~stdout_to [ A "ml"; A stubs_prefix ]))
  in
  (* The entry point module binds the instantiated Types and Functions functors
     to the entry point module name and instances the user specified. *)
  Super_context.add_rule sctx ~loc:Loc.none ~dir
    (let target =
       Path.Build.relative dir
         (generated_entry_module |> Ctypes.ml_of_module_name)
     in
     Action_builder.write_file target
       (write_entry_point_module ~ctypes
          ~type_description_instance:ctypes.type_description.instance
          ~function_description:ctypes.function_description
          ~c_types_includer_module))

let ctypes_cclib_flags sctx ~expander ~(buildable : Buildable.t) =
  let standard = Action_builder.return [] in
  match buildable.ctypes with
  | None -> standard
  | Some ctypes -> (
    let external_library_name =
      External_lib_name.to_string ctypes.external_library_name
    in
    match ctypes.build_flags_resolver with
    | Pkg_config ->
      let dir = Expander.dir expander in
      Pkg_config.Query.read (Libs external_library_name) sctx ~dir
    | Vendored { c_library_flags; c_flags = _ } ->
      Expander.expand_and_eval_set expander c_library_flags ~standard)
