open Import

type t

type extra_files =
  | Inside_files_dir
  | Git_files of Rev_store.File.t list

val package : t -> OpamPackage.t
val opam_file : t -> OpamFile.OPAM.t
val file : t -> Path.t
val extra_files : t -> extra_files
val source : t -> Source_backend.t

val create
  :  OpamFile.OPAM.t
  -> OpamPackage.t
  -> Path.Local.t
  -> Source_backend.t
  -> extra_files
  -> t
