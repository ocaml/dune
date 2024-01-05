open Import

type t

val package : t -> OpamPackage.t
val opam_file : t -> OpamFile.OPAM.t
val file : t -> Path.t

val git_repo
  :  OpamPackage.t
  -> OpamFile.OPAM.t
  -> opam_file_path:Path.Local.t
  -> Rev_store.At_rev.t
  -> files_dir:Path.Local.t
  -> t

val local_fs
  :  OpamPackage.t
  -> dir:Path.t
  -> opam_file_path:Path.Local.t
  -> files_dir:Path.Local.t
  -> t

val get_opam_package_files : t list -> File_entry.t list list Fiber.t
