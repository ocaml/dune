open Import

type t

val package : t -> OpamPackage.t
val opam_file : t -> OpamFile.OPAM.t
val file : t -> Path.t
val set_url : t -> OpamUrl.t -> t

val git_repo
  :  OpamPackage.t
  -> opam_file:Rev_store.File.t
  -> opam_file_contents:string
  -> Rev_store.At_rev.t
  -> files_dir:Path.Local.t option
  -> t

val local_fs
  :  OpamPackage.t
  -> dir:Path.t
  -> opam_file_path:Path.Local.t
  -> files_dir:Path.Local.t option
  -> t

val get_opam_package_files : t list -> File_entry.t list list Fiber.t
