(** Represents an in source package definition along with all of its associated
    stanzas required for installing *)

open Stdune

type t

val hash : t -> int

val to_sexp : t Sexp.Encoder.t

val build_dir : t -> Path.Build.t

val lib_stanzas : t -> Dune_file.Library.t Dir_with_dune.t list

val mlds : t -> Path.t list

val installs
  : t
  -> File_binding.Expanded.t Dune_file.Install_conf.t Dir_with_dune.t list

val odig_files : t -> Path.Build.t list

val of_sctx : Super_context.t -> t Package.Name.Map.t

val meta_file : t -> Path.Build.t

val opam_file : t -> Path.Build.t

val dune_package_file : t -> Path.Build.t

val name : t -> Package.Name.t

val install_paths : t -> Install.Section.Paths.t

val libs : t -> Lib.Set.t

val package : t -> Package.t

val virtual_lib : t -> Lib.t option

val meta_template : t -> Path.Build.t

val defined_in : Super_context.t -> dir:Path.t -> t list

val coqlibs : t -> Dune_file.Coq.t Dir_with_dune.t list
