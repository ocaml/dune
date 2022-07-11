type t = int * int * int

let make x = x

let of_ocaml_config ocfg = Ocaml_config.version ocfg

let supports_no_keep_locs version = version >= (4, 03, 0)

let supports_opaque_for_mli version = version >= (4, 03, 0)

let always_reads_alias_cmi version = version < (4, 03, 0)

let supports_color_in_ocamlparam version = version >= (4, 03, 0)

let supports_ocaml_color version = version >= (4, 05, 0)

let supports_response_file version = version >= (4, 05, 0)

let ocamlmklib_supports_response_file version = version >= (4, 08, 0)

let stdlib_includes_bigarray version = version >= (4, 07, 0)

let ooi_supports_no_approx version = version >= (4, 05, 0)

let ooi_supports_no_code version = version >= (4, 05, 0)

let supports_let_syntax version = version >= (4, 08, 0)

let supports_output_complete_exe version = version >= (4, 10, 1)

let supports_function_sections version = version > (4, 10, 0)

let supports_split_at_emit version = version >= (4, 11, 0)

let custom_or_output_complete_exe version =
  if supports_output_complete_exe version then "-output-complete-exe"
  else "-custom"

let ocamlopt_always_calls_library_linker version = version < (4, 12, 0)

let has_sys_opaque_identity version = version >= (4, 3, 0)

let has_vmthreads version = version < (4, 9, 0)

let has_bigarray_library version = version < (5, 0, 0)

let supports_alerts version = version >= (4, 8, 0)

let has_sandboxed_otherlibs version = version >= (5, 0, 0)

let has_META_files version = version >= (5, 0, 0)
