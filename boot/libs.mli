(* This file isn't actually needed by the bootstrap process.

   It is here to guard against accidental updates to [libs.ml]. If there's an
   interface mismatch error, it means that the changes to [libs.ml] must be
   undone and bootstrap to be done again.
*)

type root_module =
  { name : string
  ; entries : string list
  }

type library =
  { path : string
  ; main_module_name : string option
  ; include_subdirs_unqualified : bool
  ; special_builtin_support : string option
  ; root_module : root_module option
  }

val external_libraries : string list
val local_libraries : library list
val build_flags : (string list * string list) list
val link_flags : (string list * string list) list
