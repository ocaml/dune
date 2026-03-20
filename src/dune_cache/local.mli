(** This module implements a local cache of build results that are shared across
    multiple Dune instances running on the same machine. See [doc/dec/cache.md]
    for design and implementation notes.

    This is meant to be used by several Dune instances running concurrently, and
    with concurrent evictions from the cache.

    The files in the cache are assumed to be immutable, which is mostly enforced
    by removing write permissions to any file in the cache, but of course anyone
    could add back write permissions and corrupt the cache.

    See also the [dune_cache_storage] library that provides more functionality
    for manipulating the store, such as accessing individual metadata entries. *)

(* In case we do run into the problem of corrupted cache: we could actually
   store the mtime in the metadata and complain if it's not what we expected. *)

open Import

module Store_artifacts_result : sig
  (* Outcomes are ordered in the order of severity. *)
  type t =
    | Stored of Digest.t Targets.Produced.t
    | Already_present of Digest.t Targets.Produced.t
    | Error of exn
    (** [Error _] can happen due to genuine problems (cannot parse internal
        cache files) or harmless ones (race with a concurrent change to the
        cache). *)
    | Will_not_store_due_to_non_determinism of Sexp.t
end

module Target : sig
  type t

  (** Prepare the target for storing into shared cache.

      If the given file is not regular (e.g. a symbolic link), return [None]
      because such targets are not supported by the shared cache. Otherwise,
      remove the "write" permissions and record some additional information
      about the file, such as whether it is executable or not. *)
  val create : Path.Build.t -> t option
end

(** Store targets produced by a rule with a given digest. If successful, this
    operation will create one metadata entry plus one file entry per target in
    the cache.

    The [compute_digest] function is passed explicitly because the caller might
    want to memoize and/or throttle file digest computations. *)
val store_artifacts
  :  mode:Mode.t
  -> rule_digest:Digest.t
  -> Target.t Targets.Produced.t
  -> Store_artifacts_result.t Fiber.t

module Restore_result : sig
  (** Note: [Error _] can be returned due to genuine problems (e.g. if we cannot
      parse an internal cache file) or harmless ones (e.g. if another process
      deletes a cache file between the existence check and the load). *)

  type 'data t =
    | Restored of 'data
    | Not_found_in_cache
    | Error of exn
end

(** A [Value] entry corresponds to the standard output of an action. *)
module Value : sig
  module Metadata_file : sig
    type t =
      { metadata : Sexp.t list
      ; value_digest : Digest.t
      }
  end
end

(** An [Artifacts] entry corresponds to the targets produced by an action. *)
module Artifacts : sig
  module Metadata_entry : sig
    type t =
      { path : Path.Local.t (** Can have more than one component for directory targets *)
      ; digest : Digest.t option
        (** This digest is always present in case [file_path] points to a file, and absent when it's a directory. *)
      }
  end

  module Metadata_file : sig
    type t =
      { metadata : Sexp.t list
      ; (* The entries are listed in the same order that they were provided when
           storing artifacts in the cache. We keep the order to avoid confusion
           even though sorting the entres is tempting. *)
        entries : Metadata_entry.t list
      }
  end

  (** List entries of a metadata file produced by a rule with a given digest.
      The list of entries is restored only in memory, i.e. no new files will be
      created. *)
  val list : rule_digest:Dune_digest.t -> Metadata_entry.t list Restore_result.t
end

(** Some generic operations on metadata files. *)
module Metadata : sig
  type t =
    | Artifacts of Artifacts.Metadata_file.t
    | Value of Value.Metadata_file.t

  module Versioned : sig
    (** Same as the unversioned function but supports old metadata versions. *)
    val restore
      :  Version.Metadata.t
      -> rule_or_action_digest:Dune_digest.t
      -> t Restore_result.t
  end
end

(** Restore targets produced by a rule with a given digest. If successful, this
    operation will restore the targets on disk, in the [target_dir] directory,
    and will also return their paths and digests. The caller is responsible for
    removing stale versions of the targets, if any, before calling this
    function. *)
val restore_artifacts
  :  mode:Mode.t
  -> rule_digest:Digest.t
  -> target_dir:Path.Build.t
  -> Digest.t Targets.Produced.t Restore_result.t
