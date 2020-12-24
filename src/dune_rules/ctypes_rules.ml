open! Dune_engine
open! Stdune

module Library = Dune_file.Library
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

let sprintf = Printf.sprintf

let ml_of_module_name mn =
  Module_name.to_string mn ^ ".ml"
  |> String.lowercase

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
    (Build.write_file path contents)

let discover_gen ~external_lib_name:lib ~cflags_sexp ~cflags_txt
      ~c_library_flags_sexp =
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
  pr buf "    let oc = open_out \"%s\" in" cflags_txt;
  pr buf "    List.iter (Printf.fprintf oc \"%%s\") conf.cflags;";
  pr buf "    close_out oc)";
  Buffer.contents buf

let write_discover_script ~filename ~sctx ~dir ~external_lib_name ~cflags_sexp
      ~cflags_txt ~c_library_flags_sexp =
  let path = Path.Build.relative dir filename in
  let script =
    discover_gen ~external_lib_name ~cflags_sexp ~cflags_txt
      ~c_library_flags_sexp
  in
  Super_context.add_rule ~loc:Loc.none sctx ~dir (Build.write_file path script)

let type_gen_gen ~include_headers ~type_description_module =
  let buf = Buffer.create 1024 in
  let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n") in
  pr buf "let () =";
  List.iter include_headers ~f:(fun h ->
    pr buf "  print_endline \"#include <%s>\";" h);
  pr buf "  Cstubs_structs.write_c Format.std_formatter";
  pr buf "    (module %s.Types)" (Module_name.to_string type_description_module);
  Buffer.contents buf

let function_gen_gen ~include_headers ~function_description_module =
  (* XXX: make concurrency configurable *)
  let function_description_module =
    Module_name.to_string function_description_module
  in
  let concurrency = "Cstubs.unlocked" in
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
  List.iter include_headers ~f:(fun h ->
    pr buf "    print_endline \"#include <%s>\";" h);
  pr buf "    Cstubs.write_c ~concurrency Format.std_formatter ~prefix";
  pr buf "      (module %s.Functions)" function_description_module;
  pr buf "  | s -> failwith (\"unknown functions \"^s)";
  Buffer.contents buf

let write_type_gen_script ~include_headers ~dir ~filename ~sctx
      ~type_description_module =
  let path = Path.Build.relative dir filename in
  let script = type_gen_gen ~include_headers ~type_description_module in
  Super_context.add_rule ~loc:Loc.none sctx ~dir (Build.write_file path script)

let write_function_gen_script ~include_headers ~sctx ~dir ~name
      ~function_description_module =
  let path = Path.Build.relative dir (name ^ ".ml") in
  let script = function_gen_gen ~include_headers ~function_description_module in
  Super_context.add_rule ~loc:Loc.none sctx ~dir (Build.write_file path script)

let rule ?(deps=[]) ?stdout_to ?(args=[]) ?(targets=[]) ~exe ~sctx ~dir () =
  let build =
    let exe =
      match exe with
      | `relative exe -> Ok (Path.build (Path.Build.relative dir exe))
      | `unresolved exe -> Super_context.resolve_program ~loc:None ~dir sctx exe
    in
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

let build_c_program ~sctx ~dir ~source_files ~scope ~cflags_txt ~output () =
  let ctx = Super_context.context sctx in
  let exe = `unresolved (Ocaml_config.c_compiler ctx.Context.ocaml_config) in
  let include_args =
    (* XXX: need glob dependency *)
    let ocaml_where = Path.to_string ctx.Context.stdlib_dir in
    (* XXX: need glob dependency *)
    let ctypes_include_dirs =
      let ctypes = Lib_name.of_string "ctypes" in
      let lib =
        match Lib.DB.resolve (Scope.libs scope) (Loc.none, ctypes) with
        | Ok lib -> lib
        | Error _res ->
          (* XXX: User_error.raise *)
          failwith "error resolving 'ctypes' lib"
      in
      Lib.L.include_paths [lib]
      |> Path.Set.to_list
      |> List.map ~f:Path.to_string
    in
    let include_dirs = ocaml_where :: ctypes_include_dirs in
    List.concat_map include_dirs ~f:(fun dir -> ["-I"; dir])
  in
  let cflags_args =
    (* XXX: can we read the semantically identical cflags_sexp file and eliminate creating
       this extra cflags_txt file?  We do this in ctypes_stanzas:
       Ordered_set_lang.Unexpanded.of_strings ~pos [":include"; cflags_sexp ... ] *)
    let build = Build.contents (Path.relative (Path.build dir) cflags_txt) in
    let contents, deps = Build.exec build in
    assert (Dep.Set.is_empty deps);
    String.split ~on:' ' contents
  in
  rule ~deps:source_files ~targets:[output] ~exe ~sctx ~dir
    ~args:(cflags_args @ include_args @ source_files @ ["-o"; output]) ()

let cctx ~base_lib ?(libraries=[]) ~loc ~dir ~scope ~expander ~sctx =
  let compile_info =
    let dune_version = Scope.project scope |> Dune_project.dune_version in
    Lib.DB.resolve_user_written_deps_for_exes (Scope.libs scope)
      [ (loc, "ctypes") ]
      (List.map libraries ~f:(fun lib ->
         Lib_dep.Direct (loc, Lib_name.of_string lib)))
      ~dune_version ~optional:false
      ~pps:[]
  in
  let dynlink =
    let ctx = Super_context.context sctx in
    Dynlink_supported.get base_lib.Library.dynlink
      ctx.Context.supports_shared_libraries
  in
  Compilation_context.create
    ~super_context:sctx ~scope ~expander
    ~js_of_ocaml:None
    ~dynlink
    ~package:None
    ~flags:(Super_context.ocaml_flags sctx ~dir base_lib.buildable.flags)
    ~requires_compile:(Lib.Compile.direct_requires compile_info)
    ~requires_link:(Lib.Compile.requires_link compile_info)
    ~obj_dir:(Library.obj_dir ~dir base_lib)
    ~opaque:Compilation_context.Inherit_from_settings

let executable ?(modules=[]) ~base_lib ~loc ~dir ~sctx ~scope ~expander
      ~program ~libraries () =
  let build_dir = Path.build dir in
  let cctx =
    let modules =
      let name_map =
        List.map (program :: modules) ~f:(fun name ->
          let module_name = Module_name.of_string name in
          let path = Path.relative build_dir (name ^ ".ml") in
          let impl = Module.File.make Dialect.ocaml path in
          let source = Module.Source.make ~impl module_name in
          Module.of_source ~visibility:Visibility.Public
            ~kind:Module.Kind.Impl source)
        |> Module.Name_map.of_list_exn
      in
      Modules.exe_wrapped ~src_dir:dir ~modules:name_map
    in
    cctx ~base_lib ~dir ~loc ~scope ~sctx ~expander ~modules ~libraries ()
  in
  let program =
    Exe.Program.{
      name = program;
      main_module_name = Module_name.of_string program;
      loc = Loc.in_file (Path.relative build_dir program)
    }
  in
  Exe.build_and_link ~program ~linkages:[Exe.Linkage.native] ~promote:None cctx

let gen_rules ~base_lib ~scope ~expander ~dir ~sctx =
  let loc, _name = base_lib.Library.name in
  let rule = rule ~sctx ~dir in
  let executable = executable ~base_lib ~loc ~dir ~sctx ~scope ~expander in
  let ctypes =
    match base_lib.Library.ctypes with
    | Some ctypes -> ctypes
    | None -> assert false
  in
  let external_lib_name = ctypes.Ctypes.lib_name in
  let type_description_module = Ctypes_stanzas.type_description_module ctypes in
  let type_description_library = Ctypes_stanzas.type_description_library ctypes in
  let function_description_module = Ctypes_stanzas.function_description_module ctypes in
  let function_description_library = Ctypes_stanzas.function_description_library ctypes in
  (* This includer module is simply some glue to instantiate the Types functor
     that the user provides in the type description module. *)
  let () =
    write_c_types_includer_module
      ~sctx ~dir
      ~filename:(Ctypes_stanzas.c_types_includer_module ctypes
                 |> ml_of_module_name)
      ~c_generated_types_module:(Ctypes_stanzas.c_generated_types_module ctypes)
      ~type_description_module;
  in
  (* The discover script uses dune configurator / pkg_config to figure out
     how to invoke the compiler and linker for your external C library.

     https://dune.readthedocs.io/en/stable/quick-start.html#defining-a-library-with-c-stubs-using-pkg-config *)
  let c_library_flags_sexp = Ctypes_stanzas.c_library_flags_sexp ctypes in
  let cflags_txt = Ctypes_stanzas.cflags_txt ctypes in
  let () =
    let cflags_sexp = Ctypes_stanzas.cflags_sexp ctypes in
    let discover_script = sprintf "%s__ctypes_discover" external_lib_name in
    write_discover_script
      ~sctx ~dir ~filename:(discover_script ^ ".ml") ~cflags_sexp
      ~cflags_txt ~c_library_flags_sexp ~external_lib_name;
    executable
      ~program:discover_script
      ~libraries:["dune.configurator"]
      ();
    rule
      ~targets:[cflags_sexp; cflags_txt; c_library_flags_sexp]
      ~exe:(`relative (discover_script ^ ".exe"))
      ()
  in
  let include_headers = ctypes.Ctypes.includes in
  (* Type_gen produces a .c file, taking your type description module above
     as an input.
     The .c file is compiled into an .exe.
     The .exe, when run produces an .ml file.
     The .ml file is compiled into a module that will have the user's
     ML-wrapped C data/types.

     Note the similar function_gen process below depends on the ML-wrapped C
     data/types produced in this step. *)
  let () =
    let type_gen_script = sprintf "%s__type_gen" external_lib_name in
    let c_generated_types_cout_c =
      Ctypes_stanzas.c_generated_types_cout_c ctypes
    in
    let c_generated_types_cout_exe =
      Ctypes_stanzas.c_generated_types_cout_exe ctypes
    in
    write_type_gen_script ~include_headers ~sctx ~dir
      ~filename:(type_gen_script ^ ".ml")
      ~type_description_module;
    executable
      ~program:type_gen_script
      ~libraries:["ctypes.stubs"; "ctypes.foreign"; type_description_library]
      ();
    rule
      ~stdout_to:c_generated_types_cout_c
      ~exe:(`relative (type_gen_script ^ ".exe"))
      ();
    build_c_program
      ~sctx ~dir ~scope
      ~source_files:[c_generated_types_cout_c]
      ~cflags_txt (*" %{read:c_flags.txt}" *)
      ~output:c_generated_types_cout_exe
      ();
    rule
      ~stdout_to:(Ctypes_stanzas.c_generated_types_module ctypes
                  |> ml_of_module_name)
      ~exe:(`relative c_generated_types_cout_exe)
      ()
  in
  (* Function_gen is similar to type_gen above, though it produces both an
     .ml file and a .c file.  These files correspond to the files you would
     have to write by hand to wrap C code (if ctypes didn't exist!) *)
  let () =
    let stubs_prefix = external_lib_name ^ "_stubs" in
    let function_gen_script = sprintf "%s__function_gen" external_lib_name in
    let c_generated_functions_cout_c =
      Ctypes_stanzas.c_generated_functions_cout_c ctypes
    in
    write_function_gen_script ~include_headers ~sctx ~dir
      ~name:function_gen_script ~function_description_module;
    executable
      ~program:function_gen_script
      ~libraries:["ctypes.stubs"; function_description_library]
      ();
    rule
      ~stdout_to:c_generated_functions_cout_c
      ~exe:(`relative (function_gen_script ^ ".exe"))
      ~args:["c"; stubs_prefix]
      ();
    rule
      ~stdout_to:(Ctypes_stanzas.c_generated_functions_module ctypes
                  |> ml_of_module_name)
      ~exe:(`relative (function_gen_script ^ ".exe"))
      ~args:["ml"; stubs_prefix]
      ()
  in
  ()
