open Import

type t

val package : t -> OpamPackage.t
val opam_file : t -> OpamFile.OPAM.t
val loc : t -> Loc.t

(** Determines whether the package is to be built using Dune or not *)
val dune_build : t -> bool

(** A resolved package representing Dune itself. Dune is always handled in a special
    manner as we never want to build it so this value is a placeholder for cases
    where the code needs a resolved package for Dune.
 *)
val dune : t

(** Creates a resolved package from a source stored in Git, remote or local. *)
val git_repo
  :  OpamPackage.t
  -> Loc.t * OpamFile.OPAM.t
  -> Rev_store.At_rev.t
  -> dune_build:bool
  -> files_dir:Path.Local.t option
  -> url:OpamUrl.t option
  -> t

(** Creates a resolved package from a source stored in the local file system. *)
val local_fs
  :  OpamPackage.t
  -> Loc.t * OpamFile.OPAM.t
  -> dir:Path.t
  -> files_dir:Path.Local.t option
  -> url:OpamUrl.t option
  -> t

(** Creates a resolved package from a source stored in the workspace. *)
val local_package
  :  command_source:Local_package.command_source
  -> Loc.t * OpamFile.OPAM.t
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
