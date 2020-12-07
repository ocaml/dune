open! Dune_engine
open! Stdune

module Library = Dune_file.Library
module Ctypes = Dune_file.Ctypes

let osl_pos _base_lib = "", 0, 0, 0 ;;

let library_stanza ?public_name ?(foreign_stubs=[]) ?c_library_flags
      ~base_lib:lib ~name ~modules ~libraries ~wrapped () =
  let loc, _libname = lib.Library.name in
  let open Dune_file in
  let visibility =
    match public_name with
    | None -> Library.Private None
    | Some _public_name ->
      Library.Private None
      (* XXX: can only do this if the base library is public as well *)
      (*
      let plib =
        match
          Public_lib.make ~allow_deprecated_names:false
            lib.Library.project (loc, Lib_name.of_string public_name)
        with
        | Ok plib -> plib
        | Error e ->
          (* XXX: present this as a proper error *)
          failwith (sprintf "user message: %s" (User_message.to_string e))
      in
      Library.Public plib
         *)
  in
  let buildable =
    let libraries =
      List.map libraries ~f:(fun library ->
        Lib_dep.Direct (loc, Lib_name.of_string library))
    in
    let modules = List.map modules ~f:Module_name.to_string in
    { Buildable.loc
    ; modules = Ordered_set_lang.of_atoms ~loc modules
    ; modules_without_implementation = Ordered_set_lang.of_atoms ~loc []
    ; libraries
    ; foreign_archives= []
    ; foreign_stubs
    ; preprocess = Preprocess.Per_module.default ()
    ; preprocessor_deps = []
    ; lint = Lint.no_lint
    ; flags = Ocaml_flags.Spec.standard
    ; js_of_ocaml = Js_of_ocaml.default
    ; allow_overlapping_dependencies = false
    }
  in
  let c_library_flags =
    match c_library_flags with
    | None -> Ordered_set_lang.Unexpanded.standard
    | Some lst ->
      let pos = osl_pos lib in
      Ordered_set_lang.Unexpanded.of_strings ~pos lst
  in
  { Library.name = (loc, Lib_name.of_string name |> Lib_name.to_local_exn)
  ; visibility
  ; synopsis = None
  ; install_c_headers = []
  ; ppx_runtime_libraries = []
  ; modes = Mode_conf.Set.of_list [Mode_conf.Native, Mode_conf.Kind.Inherited]
  ; kind = Lib_kind.Normal
  ; library_flags = Ordered_set_lang.Unexpanded.standard
  ; c_library_flags
  ; virtual_deps = []
  ; wrapped = Lib_info.Inherited.This (Wrapped.Simple wrapped)
  ; optional = false
  ; buildable
  ; dynlink = Dynlink_supported.of_bool false
  ; project = lib.Library.project
  ; sub_systems = lib.Library.sub_systems
  ; dune_version = lib.Library.dune_version
  ; virtual_modules = None
  ; implements = None
  ; default_implementation = None
  ; private_modules = None
  ; stdlib = None
  ; special_builtin_support = None
  ; enabled_if = Blang.true_
  ; instrumentation_backend = None
  ; ctypes = None }

let sprintf = Printf.sprintf

let type_description_module ctypes =
  sprintf "%s__c_type_descriptions" ctypes.Ctypes.lib_name
  |> Module_name.of_string

let type_description_library ctypes =
  sprintf "%s__c_type_descriptions" ctypes.Ctypes.lib_name

let type_description_library_public ctypes =
  sprintf "%s.c_type_descriptions" ctypes.Ctypes.lib_name

let function_description_module ctypes =
  sprintf "%s__c_function_descriptions" ctypes.Ctypes.lib_name
  |> Module_name.of_string

let function_description_library ctypes =
  sprintf "%s__c_function_descriptions" ctypes.Ctypes.lib_name

let function_description_library_public ctypes =
  sprintf "%s.c_function_descriptions" ctypes.Ctypes.lib_name

let entry_module ctypes =
  sprintf "%s_c" ctypes.Ctypes.lib_name
  |> Module_name.of_string

let entry_library ctypes =
  sprintf "%s_c" ctypes.Ctypes.lib_name

let entry_library_public ctypes =
  sprintf "%s.c" ctypes.Ctypes.lib_name

let cflags_sexp ctypes =
  sprintf "%s__c_flags.sexp" ctypes.Ctypes.lib_name

let cflags_txt ctypes =
  sprintf "%s__c_flags.txt" ctypes.Ctypes.lib_name

let c_library_flags_sexp ctypes =
  sprintf "%s__c_library_flags.sexp" ctypes.Ctypes.lib_name

let c_generated_types_module ctypes =
  sprintf "%s__c_generated_types" ctypes.Ctypes.lib_name
  |> Module_name.of_string

let c_generated_functions_module ctypes =
  sprintf "%s__c_generated_functions" ctypes.Ctypes.lib_name
  |> Module_name.of_string

let c_types_includer_module ctypes =
  sprintf "%s__c_types" ctypes.Ctypes.lib_name
  |> Module_name.of_string

let c_generated_types_cout_c ctypes =
  sprintf "%s__c_cout_generated_types.c" ctypes.Ctypes.lib_name

let c_generated_types_cout_exe ctypes =
  sprintf "%s__c_cout_generated_types.exe" ctypes.Ctypes.lib_name

let c_generated_functions_cout_c ctypes =
  sprintf "%s__c_cout_generated_functions.c" ctypes.Ctypes.lib_name

let c_generated_functions_cout_no_ext ctypes =
  sprintf "%s__c_cout_generated_functions" ctypes.Ctypes.lib_name

(* Unlike for [executable] and [rule] generation which have neat convenience
   functions for creating new ones, the machinery for creating new [library]s
   does several passes to populate global data structures.

   Rather than attempting to teach each of those passes about ctypes, the
   approach here is to simply do a quasi-lexical expansion of the base library
   config stanza into several additional support library stanzas, right after
   the dune config file parsing is completed. *)
let library_stanzas base_lib =
  let ctypes =
    match base_lib.Library.ctypes with
    | Some ctypes -> ctypes
    | None -> assert false
  in
  let type_descriptions =
    library_stanza
      ~base_lib
      ~name:(type_description_library ctypes)
      ~public_name:(type_description_library_public ctypes)
      ~modules:[type_description_module ctypes]
      ~libraries:["ctypes"]
      ~wrapped:true ()
  in
  let function_descriptions =
    library_stanza
      ~base_lib
      ~name:(function_description_library ctypes)
      ~public_name:(function_description_library_public ctypes)
      ~modules:[ c_generated_types_module ctypes
               ; function_description_module ctypes
               ; c_types_includer_module ctypes ]
      (* ~flags:"-w -27 -w -9" ?*)
      ~libraries:["ctypes"; (type_description_library ctypes)]
      ~wrapped:false ()
  in
  let combined_final =
    let foreign_stub =
      let loc, _libname = base_lib.Library.name in
      let pos = osl_pos base_lib in
      Foreign.Stubs.make ~loc ~language:Foreign_language.C
        ~names:(Ordered_set_lang.of_atoms ~loc
                  [c_generated_functions_cout_no_ext ctypes])
        ~flags:(Ordered_set_lang.Unexpanded.of_strings
                  ~pos [":include"; cflags_sexp ctypes])
    in
    library_stanza
      ~base_lib
      ~name:(entry_library ctypes)
      ~public_name:(entry_library_public ctypes)
      ~libraries:["ctypes"; function_description_library ctypes]
      ~modules:[ entry_module ctypes
               ; c_generated_functions_module ctypes ]
      ~foreign_stubs:[foreign_stub]
      ~c_library_flags:[":include"; c_library_flags_sexp ctypes]
      ~wrapped:true
      ()
  in
  [ type_descriptions
  ; function_descriptions
  ; combined_final ]

let generated_ml_and_c_files ctypes =
  let ml_files =
    List.map [ c_generated_functions_module ctypes
             ; c_generated_types_module ctypes
             ; c_types_includer_module ctypes ]
      ~f:Module_name.to_string
    |> List.map ~f:String.lowercase
    |> List.map ~f:(fun m -> m ^ ".ml")
  in
  let c_files =
    [ c_generated_functions_cout_c ctypes ]
  in
  ml_files @ c_files
