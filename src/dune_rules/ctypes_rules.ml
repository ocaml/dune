open! Dune_engine
open! Stdune

module Buildable = Dune_file.Buildable
module Library =  Dune_file.Library
module Ctypes = Dune_file.Ctypes

(* This module expands a [(library ... (ctypes ...))] rule into the set of
   [library], [executable], [rule] rules and .ml files needed to more
   conveniently build OCaml bindings for C libraries.  Aside from perhaps
   providing an '#include "header.h"' line, you should be able to wrap an
   entire C library without writing a single line of C code.

   This stanza requires the user to define (and specify) two modules:

   (1) A "Type Descriptions" .ml file with the following top-level module:

    module Types (T : Ctypes.TYPE) = struct
      (* put calls to Ctypes.TYPE.constant and Ctypes.TYPE.typedef here
         to wrap C constants and structs *)
    end

   (2) A 'Function Descriptions' .ml file with the following top-level module:

    module Functions (F : Ctypes.FOREIGN) = struct
      (* put calls to F.foreign here to wrap C functions *)
    end

   The instantiated functor that was defined in 'Types' can be accessed from
   the Function Descriptions module as Library_name__c_types.

   e.g. module Types = Library_name__c_types

   Once the above two modules are provided, the ctypes stanza will:
   - generate a types/data generator
   - generate a functions generator
   - set up a discovery program to query pkg-config for compile and link flags
   - use the types/data and functions modules you filled in with a functor
   to tie everything into your library.

   The result of using a ctypes stanza is that it will introduce into your
   project a library that provides interfaces to all of the types and functions
   you described earlier, with the rather involved compilation and linking
   details handled for you.

   It may help to view a real world example of all of the boilerplate that is
   being replaced by a [ctypes] stanza:

   https://github.com/mbacarella/mpg123/blob/077a72d922931eb46d4b4e5842b0426fa3c161b5/c/dune
*)

module Stanza_util = struct

  let sprintf = Printf.sprintf

  let type_description_module ctypes =
    ctypes.Ctypes.type_descriptions

  let type_description_library ctypes =
    type_description_module ctypes
    |> Module_name.to_string
    |> String.lowercase

  let function_description_module ctypes =
    ctypes.Ctypes.function_descriptions

  let function_description_library ctypes =
    function_description_module ctypes
    |> Module_name.to_string
    |> String.lowercase

  let entry_module ctypes =
    ctypes.Ctypes.generated_entry_point
(*
     let entry_library ctypes =
     entry_module ctypes |> Module_name.to_string |> String.lowercase
  *)

  let cflags_sexp ctypes =
    sprintf "%s__c_flags.sexp" ctypes.Ctypes.external_library_name

  let c_library_flags_sexp ctypes =
    sprintf "%s__c_library_flags.sexp" ctypes.Ctypes.external_library_name

  let c_generated_types_module ctypes =
    sprintf "%s__c_generated_types" ctypes.Ctypes.external_library_name
    |> Module_name.of_string

  let c_generated_functions_module ctypes =
    sprintf "%s__c_generated_functions" ctypes.Ctypes.external_library_name
    |> Module_name.of_string

  let c_types_includer_module ctypes =
    ctypes.Ctypes.generated_types

  let c_generated_types_cout_c ctypes =
    sprintf "%s__c_cout_generated_types.c" ctypes.Ctypes.external_library_name

  let c_generated_types_cout_exe ctypes =
    sprintf "%s__c_cout_generated_types.exe" ctypes.Ctypes.external_library_name

  let c_generated_functions_cout_c ctypes =
    sprintf "%s__c_cout_generated_functions.c" ctypes.Ctypes.external_library_name

  let generated_modules ctypes =
    List.map ~f:Module_name.to_string
      [ c_generated_functions_module ctypes
      ; c_generated_types_module ctypes
      ; c_types_includer_module ctypes ]
      (*
      ; entry_module ctypes ] *)

  let generated_ml_and_c_files ctypes =
    let ml_files =
      generated_modules ctypes
      |> List.map ~f:String.lowercase
      |> List.map ~f:(fun m -> m ^ ".ml")
    in
    let c_files =
      [ c_generated_functions_cout_c ctypes ]
    in
    ml_files @ c_files
end

let osl_pos () = "", 0, 0, 0

let sprintf = Printf.sprintf

let ml_of_module_name mn =
  Module_name.to_string mn ^ ".ml"
  |> String.lowercase

let modules_of_list ~dir ~modules =
  let name_map =
    let build_dir = Path.build dir in
    let modules =
      List.map modules ~f:(fun name ->
        let module_name = Module_name.of_string name in
        let path = Path.relative build_dir (String.lowercase name ^ ".ml") in
        let impl = Module.File.make Dialect.ocaml path in
        let source = Module.Source.make ~impl module_name in
        Module.of_source ~visibility:Visibility.Public
          ~kind:Module.Kind.Impl source)
    in
    Module.Name_map.of_list_exn modules
  in
  Modules.exe_unwrapped name_map
  (*
  Modules.exe_wrapped ~src_dir:dir ~modules:name_map
     *)

let write_c_types_includer_module ~sctx ~dir ~filename ~type_description_module
      ~c_generated_types_module =
  let path = Path.Build.relative dir filename in
  let contents =
    let buf = Buffer.create 1024 in
    let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n") in
    pr buf "include %s.Types (%s)"
        (Module_name.to_string type_description_module)
        (Module_name.to_string c_generated_types_module);
    Buffer.contents buf
  in
  Super_context.add_rule ~loc:Loc.none sctx ~dir
    (Action_builder.write_file path contents)

let write_entry_point_module ~sctx ~dir ~filename ~function_description_module
      ~c_generated_functions_module ~c_types_includer_module =
  let path = Path.Build.relative dir filename in
  let contents =
    let buf = Buffer.create 1024 in
    let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n") in
    pr buf "module Types = %s" (Module_name.to_string c_types_includer_module);
    pr buf "module Functions = %s.Functions (%s)"
        (Module_name.to_string function_description_module)
        (Module_name.to_string c_generated_functions_module);
    Buffer.contents buf
  in
  Super_context.add_rule ~loc:Loc.none sctx ~dir
    (Action_builder.write_file path contents)

let discover_gen ~external_library_name:lib ~cflags_sexp ~c_library_flags_sexp =
  let buf = Buffer.create 1024 in
  let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n") in
  pr buf "module C = Configurator.V1";
  pr buf "let () =";
  pr buf "  C.main ~name:\"%s\" (fun c ->" lib;
  pr buf "    let default : C.Pkg_config.package_conf =";
  pr buf "      { libs   = [\"-l%s\"];" lib;
  pr buf "        cflags = [\"-I/usr/include\"] }";
  pr buf "    in";
  pr buf "    let conf =";
  pr buf "      match C.Pkg_config.get c with";
  pr buf "      | None -> default";
  pr buf "      | Some pc ->";
  pr buf "        match C.Pkg_config.query pc ~package:\"%s\" with" lib;
  pr buf "        | None -> default";
  pr buf "        | Some deps -> deps";
  pr buf "    in";
  pr buf "    C.Flags.write_sexp \"%s\" conf.cflags;" cflags_sexp;
  pr buf "    C.Flags.write_sexp \"%s\" conf.libs;" c_library_flags_sexp;
  pr buf "  )";
  Buffer.contents buf

let write_discover_script ~filename ~sctx ~dir ~external_library_name
      ~cflags_sexp ~c_library_flags_sexp =
  let path = Path.Build.relative dir filename in
  let script =
    discover_gen ~external_library_name ~cflags_sexp ~c_library_flags_sexp
  in
  Super_context.add_rule ~loc:Loc.none sctx ~dir
    (Action_builder.write_file path script)

let gen_headers headers buf =
  let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n") in
  begin match headers with
  | Ctypes.Headers.Include lst ->
    List.iter lst ~f:(fun h -> pr buf "  print_endline \"#include <%s>\";" h)
  | Preamble s ->
    pr buf "  print_endline \"%S\";" s
  end

let type_gen_gen ~headers ~type_description_module =
  let buf = Buffer.create 1024 in
  let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n") in
  pr buf "let () =";
  gen_headers headers buf;
  pr buf "  Cstubs_structs.write_c Format.std_formatter";
  pr buf "    (module %s.Types)" (Module_name.to_string type_description_module);
  Buffer.contents buf

let function_gen_gen ~concurrency ~headers ~function_description_module =
  let function_description_module =
    Module_name.to_string function_description_module
  in
  let concurrency =
    match concurrency with
    | Ctypes.Concurrency_policy.Unlocked -> "Cstubs.unlocked"
    | Sequential -> "Cstubs.sequential"
    | Lwt_jobs -> "Cstubs.lwt_jobs"
    | Lwt_preemptive -> "Cstubs.lwt_preemptive"
  in
  let buf = Buffer.create 1024 in
  let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n") in
  pr buf "let () =";
  pr buf "  let concurrency = %s in" concurrency;
  pr buf "  let prefix = Sys.argv.(2) in";
  pr buf "  match Sys.argv.(1) with";
  pr buf "  | \"ml\" ->";
  pr buf "    Cstubs.write_ml ~concurrency Format.std_formatter ~prefix";
  pr buf "      (module %s.Functions)" function_description_module;
  pr buf "  | \"c\" ->";
  gen_headers headers buf;
  pr buf "    Cstubs.write_c ~concurrency Format.std_formatter ~prefix";
  pr buf "      (module %s.Functions)" function_description_module;
  pr buf "  | s -> failwith (\"unknown functions \"^s)";
  Buffer.contents buf

let write_type_gen_script ~headers ~dir ~filename ~sctx
      ~type_description_module =
  let path = Path.Build.relative dir filename in
  let script = type_gen_gen ~headers ~type_description_module in
  Super_context.add_rule ~loc:Loc.none sctx ~dir
    (Action_builder.write_file path script)

let write_function_gen_script ~headers ~sctx ~dir ~name
      ~function_description_module ~concurrency =
  let path = Path.Build.relative dir (name ^ ".ml") in
  let script = function_gen_gen ~concurrency ~headers ~function_description_module in
  Super_context.add_rule ~loc:Loc.none sctx ~dir (Action_builder.write_file path script)

let rule ?(deps=[]) ?stdout_to ?(args=[]) ?(targets=[]) ~exe ~sctx ~dir () =
  let build =
    let exe = Ok (Path.build (Path.Build.relative dir exe)) in
    let args =
      let targets = List.map targets ~f:(Path.Build.relative dir) in
      let deps =
        List.map deps ~f:(Path.relative (Path.build dir))
        |> Dep.Set.of_files
      in
      let open Command.Args in
      [ Hidden_targets targets
      ; Hidden_deps deps
      ; As args ]
    in
    let stdout_to = Option.map stdout_to ~f:(Path.Build.relative dir) in
    Command.run exe ~dir:(Path.build dir) ?stdout_to args
  in
  Super_context.add_rule sctx ~dir build

let build_c_program ~sctx ~dir ~source_files ~scope ~cflags_sexp ~output () =
  let ctx = Super_context.context sctx in
  let exe =
    Ocaml_config.c_compiler ctx.Context.ocaml_config
    |> Super_context.resolve_program ~loc:None ~dir sctx
  in
  let include_args =
    (* XXX: need glob dependency *)
    let ocaml_where = Path.to_string ctx.Context.stdlib_dir in
    (* XXX: need glob dependency *)
    let ctypes_include_dirs =
      let lib =
        let ctypes = Lib_name.of_string "ctypes" in
        match Lib.DB.resolve (Scope.libs scope) (Loc.none, ctypes) with
        | Ok lib -> lib
        | Error _res ->
          User_error.raise
            [ Pp.textf "the 'ctypes' library needs to be installed to use the ctypes stanza"]
      in
      Lib.L.include_paths [lib] Mode.Native
      |> Path.Set.to_list
      |> List.map ~f:Path.to_string
    in
    let include_dirs = ocaml_where :: ctypes_include_dirs in
    List.concat_map include_dirs ~f:(fun dir -> ["-I"; dir])
  in
  let deps =
    List.map source_files ~f:(Path.relative (Path.build dir))
    |> Dep.Set.of_files
  in
  let build =
    let cflags_args =
      let contents = Action_builder.contents (Path.relative (Path.build dir) cflags_sexp) in
      Action_builder.map contents ~f:(fun sexp ->
        let fail s = User_error.raise [ Pp.textf s ] in
        let ast =
          Dune_lang.Parser.parse_string ~mode:Dune_lang.Parser.Mode.Single
            ~fname:cflags_sexp sexp
        in
        match ast with
        | Dune_lang.Ast.Atom (_loc, atom) -> [Dune_lang.Atom.to_string atom]
        | Template _ -> fail "'template' not supported in ctypes c_flags"
        | Quoted_string (_loc, s) -> [s]
        | List (_loc, lst) ->
          List.map lst ~f:(function
            | Dune_lang.Ast.Atom (_loc, atom) -> Dune_lang.Atom.to_string atom
            | Quoted_string (_loc, s) -> s
            | Template _ -> fail "'template' not supported in ctypes c_flags"
            | List _ -> fail "nested lists not supported in ctypes c_flags"))
    in
    let action =
      let open Action_builder.O in
      Action_builder.deps deps
      >>> Action_builder.map cflags_args ~f:(fun cflags_args ->
        let args = cflags_args @ include_args @ source_files @ ["-o"; output] in
        Action.run exe args)
    in
    Action_builder.with_targets action ~targets:[Path.Build.relative dir output]
  in
  Super_context.add_rule sctx ~dir build

let cctx_with_substitutions ?flags ?(libraries=[]) ~modules ~dir ~loc ~scope ~cctx =
  let compile_info =
    let dune_version = Scope.project scope |> Dune_project.dune_version in
    Lib.DB.resolve_user_written_deps_for_exes (Scope.libs scope)
      [ (loc, "ctypes") ]
      (List.map libraries ~f:(fun lib ->
         Lib_dep.Direct (loc, Lib_name.of_string lib)))
      ~dune_version ~pps:[]
  in
  let modules = modules_of_list ~dir ~modules in
  let module Cctx = Compilation_context in
  Cctx.create
    ~super_context:(Cctx.super_context cctx)
    ~scope:(Cctx.scope cctx)
    ~expander:(Cctx.expander cctx)
    ~js_of_ocaml:(Cctx.js_of_ocaml cctx)
    ~package:(Cctx.package cctx)
    ~flags:(match flags with
      | Some flags -> flags
      | None -> Cctx.flags cctx)
    ~requires_compile:(Lib.Compile.direct_requires compile_info)
    ~requires_link:(Lib.Compile.requires_link compile_info)
    ~obj_dir:(Cctx.obj_dir cctx)
    ~opaque:(Cctx.Explicit (Cctx.opaque cctx))
    ~modules ()

let executable ?flags ?libraries ?(modules=[]) ~scope ~loc ~dir ~cctx program =
  let cctx =
    cctx_with_substitutions ?flags ?libraries ~modules:(program :: modules)
      ~loc ~scope ~dir ~cctx
  in
  let program =
    let build_dir = Path.build dir in
    Exe.Program.{
      name = program;
      main_module_name = Module_name.of_string program;
      loc = Loc.in_file (Path.relative build_dir program)
    }
  in
  Exe.build_and_link ~program ~linkages:[Exe.Linkage.native]
    ~promote:None cctx

let executable_with_shared_cctx ~dir ~shared_cctx ~dep_graphs program =
  let program =
    let build_dir = Path.build dir in
    Exe.Program.{
      name = program;
      main_module_name = Module_name.of_string program;
      loc = Loc.in_file (Path.relative build_dir program)
    }
  in
  Exe.link_many ~programs:[program] ~linkages:[Exe.Linkage.native]
    ~dep_graphs ~promote:None shared_cctx

let write_osl_to_sexp_file ~sctx ~dir ~filename osl =
  let build =
    let path = Path.Build.relative dir filename in
    let sexp =
      let encoded =
        match Ordered_set_lang.Unexpanded.encode osl with
        | [s] -> s
        | _lst -> failwith "unexpected multi-element list"
      in
      Dune_lang.to_string encoded
    in
    Action_builder.write_file path sexp
  in
  Super_context.add_rule ~loc:Loc.none sctx ~dir build

let gen_rules ~cctx ~buildable ~loc ~scope ~dir ~sctx =
  let ctypes =
    match buildable.Buildable.ctypes with
    | Some ctypes -> ctypes
    | None -> assert false
  in
  let external_library_name = ctypes.Ctypes.external_library_name in
  let discover_script = sprintf "%s__ctypes_discover" external_library_name in
  let type_gen_script = sprintf "%s__type_gen" external_library_name in
  let function_gen_script = sprintf "%s__function_gen" external_library_name in
  let type_description_module = Stanza_util.type_description_module ctypes in
  let function_description_module = Stanza_util.function_description_module ctypes in
  (* This includer module is simply some glue to instantiate the Types functor
     that the user provides in the type description module. *)
  let c_types_includer_module = Stanza_util.c_types_includer_module ctypes in
  let c_generated_types_module = Stanza_util.c_generated_types_module ctypes in
  let rule = rule ~sctx ~dir in
  let executable = executable ~scope ~loc ~dir ~cctx in
  let () =
    write_c_types_includer_module
      ~sctx ~dir
      ~filename:(ml_of_module_name c_types_includer_module)
      ~c_generated_types_module
      ~type_description_module
  in
  (* The output of this process is to generate a cflags sexp and a c library
     flags sexp file. We can probe these flags by using the system pkg-config,
     if it's an external system library.  The user could also tell us what
     they are, if the library is vendored.

     https://dune.readthedocs.io/en/stable/quick-start.html#defining-a-library-with-c-stubs-using-pkg-config *)
  let c_library_flags_sexp = Stanza_util.c_library_flags_sexp ctypes in
  let cflags_sexp = Stanza_util.cflags_sexp ctypes in
  let () =
    let open Ctypes.Build_flags_resolver in
    match ctypes.Ctypes.build_flags_resolver with
    | Vendored { c_flags; c_library_flags } ->
      write_osl_to_sexp_file ~sctx ~dir ~filename:cflags_sexp c_flags;
      write_osl_to_sexp_file ~sctx ~dir ~filename:c_library_flags_sexp
        c_library_flags
    | Pkg_config ->
      let cflags_sexp = Stanza_util.cflags_sexp ctypes in
      write_discover_script
        ~sctx ~dir ~filename:(discover_script ^ ".ml") ~cflags_sexp
        ~c_library_flags_sexp ~external_library_name;
      executable ~libraries:["dune.configurator"] discover_script;
      rule
        ~targets:[cflags_sexp; c_library_flags_sexp]
        ~exe:(discover_script ^ ".exe")
        ()
  in
  let headers = ctypes.Ctypes.headers in
  let executable_with_shared_cctx =
    let shared_cctx =
      let flags =
        (* ctypes stubgen emits ocaml code with warnings which can make
           compilation impossible if you're in warnings-as-errors mode;
           disable the warnings *)
        Ocaml_flags.of_list ["-w"; "-27"; "-w"; "-9"]
      in
      cctx_with_substitutions
        ~cctx ~dir ~loc ~scope ~flags
        ~libraries:["ctypes"; "ctypes.foreign"; "ctypes.stubs"]
        ~modules:[ function_gen_script
                 ; type_gen_script
                 ; Module_name.to_string type_description_module
                 ; Module_name.to_string function_description_module
                 ; Module_name.to_string c_types_includer_module
                 ; Module_name.to_string c_generated_types_module
                 ]
    in
    (* Process the build rules once, to avoid bombing out on duplicate rules
       errors when building executables that share modules between them. *)
    let dep_graphs =
      Dep_rules.rules shared_cctx ~modules:(Compilation_context.modules shared_cctx)
    in
    let () = Module_compilation.build_all shared_cctx ~dep_graphs in
    executable_with_shared_cctx ~dir ~shared_cctx ~dep_graphs
  in
  (* Type_gen produces a .c file, taking your type description module above
     as an input.
     The .c file is compiled into an .exe.
     The .exe, when run produces an .ml file.
     The .ml file is compiled into a module that will have the user's
     ML-wrapped C data/types.

     Note the similar function_gen process below depends on the ML-wrapped C
     data/types produced in this step. *)
  let () =
    let c_generated_types_cout_c =
      Stanza_util.c_generated_types_cout_c ctypes
    in
    let c_generated_types_cout_exe =
      Stanza_util.c_generated_types_cout_exe ctypes
    in
    write_type_gen_script ~headers ~sctx ~dir
      ~filename:(type_gen_script ^ ".ml")
      ~type_description_module;
    executable_with_shared_cctx type_gen_script;
    rule
      ~stdout_to:c_generated_types_cout_c
      ~exe:(type_gen_script ^ ".exe")
      ();
    build_c_program
      ~sctx ~dir ~scope ~cflags_sexp
      ~source_files:[c_generated_types_cout_c]
      ~output:c_generated_types_cout_exe
      ();
    rule
      ~stdout_to:(c_generated_types_module |> ml_of_module_name)
      ~exe:(c_generated_types_cout_exe)
      ()
  in
  (* Function_gen is similar to type_gen above, though it produces both an
     .ml file and a .c file.  These files correspond to the files you would
     have to write by hand to wrap C code (if ctypes didn't exist!) *)
  let () =
    let stubs_prefix = external_library_name ^ "_stubs" in
    let c_generated_functions_cout_c =
      Stanza_util.c_generated_functions_cout_c ctypes
    in
    write_function_gen_script ~headers ~sctx ~dir
      ~name:function_gen_script ~function_description_module
      ~concurrency:ctypes.Ctypes.concurrency;
    executable_with_shared_cctx function_gen_script;
    rule
      ~stdout_to:c_generated_functions_cout_c
      ~exe:(function_gen_script ^ ".exe")
      ~args:["c"; stubs_prefix]
      ();
    rule
      ~stdout_to:(Stanza_util.c_generated_functions_module ctypes
                  |> ml_of_module_name)
      ~exe:(function_gen_script ^ ".exe")
      ~args:["ml"; stubs_prefix]
      ()
  in
  (*
  (* The entry point module binds the instantiated Types and Functions functors
     to the entry point module name the user specified. *)
  let () =
    write_entry_point_module
      ~sctx ~dir
      ~filename:(Stanza_util.entry_module ctypes |> ml_of_module_name)
      ~function_description_module:(Stanza_util.function_description_module ctypes)
      ~c_generated_functions_module:(Stanza_util.c_generated_functions_module ctypes)
      ~c_types_includer_module
  in
     *)
  ()
