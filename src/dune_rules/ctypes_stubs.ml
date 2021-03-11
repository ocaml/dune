open! Dune_engine
open! Stdune

let cflags_sexp ~external_library_name =
  sprintf "%s__c_flags.sexp" external_library_name

let c_generated_functions_cout_no_ext ~external_library_name =
  sprintf "%s__c_cout_generated_functions" external_library_name

let c_library_flags ~external_library_name =
  sprintf "%s__c_library_flags.sexp" external_library_name

let add ~loc ~parsing_context ~external_library_name ~add_stubs ~foreign_stubs =
  add_stubs
    Foreign_language.C
    ~loc
    ~names:(Some (Ordered_set_lang.of_atoms ~loc
                    [c_generated_functions_cout_no_ext ~external_library_name]))
    ~flags:(Some (Ordered_set_lang.Unexpanded.include_single
                    ~context:parsing_context ~pos:("", 0, 0, 0)
                    (cflags_sexp ~external_library_name)))
    foreign_stubs
