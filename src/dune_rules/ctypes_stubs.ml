open Import

let cflags_sexp ~external_library_name =
  sprintf "%s__c_flags.sexp" (External_lib_name.to_string external_library_name)

let c_generated_functions_cout_no_ext ~external_library_name ~functor_ ~instance
    =
  sprintf "%s__c_cout_generated_functions__%s__%s"
    (External_lib_name.to_string external_library_name)
    (Module_name.to_string functor_ |> String.lowercase)
    (Module_name.to_string instance |> String.lowercase)

let c_library_flags ~external_library_name =
  sprintf "%s__c_library_flags.sexp"
    (External_lib_name.to_string external_library_name)

let lib_deps_of_strings ~loc lst =
  List.map lst ~f:(fun lib -> Lib_dep.Direct (loc, Lib_name.of_string lib))

let libraries_needed_for_ctypes ~loc =
  let libraries = [ "ctypes"; "ctypes.stubs" ] in
  lib_deps_of_strings ~loc libraries

let add ~loc ~parsing_context ~external_library_name ~add_stubs ~functor_
    ~instance ~foreign_stubs =
  let pos = ("", 0, 0, 0) in
  let flags =
    let cflags_sexp_include =
      Ordered_set_lang.Unexpanded.include_single ~context:parsing_context ~pos
        (cflags_sexp ~external_library_name)
    in
    Ordered_set_lang.Unexpanded.concat ~context:parsing_context ~pos
      Ordered_set_lang.Unexpanded.standard cflags_sexp_include
  in
  add_stubs Foreign_language.C ~loc
    ~names:
      (Some
         (Ordered_set_lang.of_atoms ~loc
            [ c_generated_functions_cout_no_ext ~external_library_name ~functor_
                ~instance
            ]))
    ~flags:(Some flags) foreign_stubs
