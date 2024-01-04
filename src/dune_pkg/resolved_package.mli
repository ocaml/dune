open Import

type t

type extra_files =
  | Inside_files_dir of Path.t
  | Git_files of Rev_store.File.t list

val package : t -> OpamPackage.t
val opam_file : t -> OpamFile.OPAM.t
val file : t -> Path.t
val extra_files : t -> extra_files
val source : t -> Source_backend.t

val git_repo
  :  OpamPackage.t
  -> OpamFile.OPAM.t
  -> opam_file_path:Path.Local.t
  -> Source_backend.t
  -> extra_files:Rev_store.File.t list
  -> t

val local_fs
  :  OpamPackage.t
  -> dir:Path.t
  -> opam_file_path:Path.Local.t
  -> files_dir:Path.t
  -> t
