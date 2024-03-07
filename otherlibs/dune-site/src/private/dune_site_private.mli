val path_sep : char
val dune_dir_locations_env_var : string
val dune_ocaml_stdlib_env_var : string
val dune_ocaml_hardcoded_env_var : string
val dune_sourceroot_env_var : string

type entry =
  { package : string
  ; section : Dune_section.t
  ; dir : string
  }

val decode_dune_dir_locations : string -> entry list option
val encode_dune_dir_locations : entry list -> string
