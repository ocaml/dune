open Import

type t

val package : t -> OpamPackage.t
val opam_file : t -> OpamFile.OPAM.t
val loc : t -> Loc.t
val dune_build : t -> bool
val dune : t

val git_repo
  :  OpamPackage.t
  -> opam_file:Path.Local.t
  -> opam_file_contents:string
  -> Rev_store.At_rev.t
  -> files_dir:Path.Local.t option
  -> url:OpamUrl.t option
  -> t

val local_fs
  :  OpamPackage.t
  -> dir:Path.t
  -> opam_file_path:Path.Local.t
  -> files_dir:Path.Local.t option
  -> url:OpamUrl.t option
  -> t

val local_package
  :  command_source:Local_package.command_source
  -> Loc.t
  -> OpamFile.OPAM.t
  -> OpamPackage.t
  -> t

val get_opam_package_files
  :  t list
  -> (File_entry.t list list, User_message.t) result Fiber.t

(** [digest t] computes a digest of the resolved package contents, excluding the
    source location. For directory-based extra files, the digest of the
    directory contents is included. For git-based extra files, the commit SHA is
    included.

    Raises [User_error] if extra files in a directory cannot be accessed or
    digested due to permission errors, filesystem errors. *)
val digest : t -> Dune_digest.t
