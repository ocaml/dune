(** The layout of the Dune cache storage, used by local and cloud build caches. *)

(* CR-someday amokhov: Jenga used "value" entries to store the standard output
   of anonymous actions, but Dune currently stores everything in "file" entries.
   We decided to keep support for values for now but will re-evaluate this
   decision in 6 months. *)

open Stdune
open Import

(** The path to the root directory of the cache. *)
val root_dir : Path.t

(** Create a few subdirectories in [root_dir]. We expose this function because
    we don't want to modify the file system when the cache is disabled. *)
val create_cache_directories : unit -> unit

(** This directory stores metadata files, one per each historically executed
    build rule or output-producing action. (While this is a convenient mental
    model, in reality we need to occasionally remove some outdated metadata
    files to free disk space.)

    A metadata file corresponding to a build rule is named by the rule digest
    and stores file names and content digests of all artifacts produced by the
    rule.

    A metadata file corresponding to an output-producing action is named by the
    action digest and stores the content digest of the resulting output. *)
val metadata_storage_dir : Path.t

(** Path to the metadata file corresponding to a build action or rule with the
    given [rule_or_action_digest]. *)
val metadata_path : rule_or_action_digest:Digest.t -> Path.t

(** This is a storage for artifacts, where files named by content digests store
    the matching contents. We will create hard links to these files from build
    directories and rely on the hard link count, as well as on the last access
    time as useful metrics during cache trimming. *)
val file_storage_dir : Path.t

(** Path to the artifact corresponding to a given [file_digest]. *)
val file_path : file_digest:Digest.t -> Path.t

(** This is a storage for outputs and, more generally, other values that the
    build system might choose to store in the cache in future. As in
    [files_path], we store the values in the files named by their content
    digests. However, these files will always have the hard link count equal to
    one because they do not appear anywhere in build directories. By storing
    them in a separate directory, we simplify the job of the cache trimmer. *)
val value_storage_dir : Path.t

(** Path to the value corresponding to a given [value_digest]. *)
val value_path : value_digest:Digest.t -> Path.t

(** This directory contains temporary files used for atomic file operations
    needed when storing new artifacts in the cache. See [write_atomically]. *)
val temp_dir : Path.t

(** Support for all versions of the layout, used by the cache trimmer. The
    functions provided by the top module are obtained by a partial application
    of the corresponding function defined here to a suitable current version. *)
module Versioned : sig
  val metadata_storage_dir : Version.Metadata.t -> Path.t

  val metadata_path :
    Version.Metadata.t -> rule_or_action_digest:Digest.t -> Path.t

  val file_storage_dir : Version.File.t -> Path.t

  val file_path : Version.File.t -> file_digest:Digest.t -> Path.t

  val value_storage_dir : Version.Value.t -> Path.t

  val value_path : Version.Value.t -> value_digest:Digest.t -> Path.t

  (** List all metadata entries currently stored in the cache. Note that there
      is no guarantee that the result is up-to-date, since files can be added or
      removed concurrently by other processes. *)
  val list_metadata_entries : Version.Metadata.t -> (Path.t * Digest.t) list

  (** List [list_metadata_entries] but for file entries. *)
  val list_file_entries : Version.File.t -> (Path.t * Digest.t) list

  (** List [list_metadata_entries] but for value entries. *)
  val list_value_entries : Version.Value.t -> (Path.t * Digest.t) list
end
