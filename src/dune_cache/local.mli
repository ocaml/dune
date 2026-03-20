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

module Store_result : sig
  type t =
    | Stored
    | Already_present
    | Error of exn
    | Will_not_store_due_to_non_determinism of Sexp.t

  val combine : t -> t -> t
  val empty : t
end

module Restore_result : sig
  (** Note: [Error _] can be returned due to genuine problems (e.g. if we cannot
      parse an internal cache file) or harmless ones (e.g. if another process
      deletes a cache file between the existence check and the load). *)

  type 'data t =
    | Restored of 'data
    | Not_found_in_cache
    | Error of exn

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
end

type metadata

(** A [Value] entry corresponds to the standard output of an action. *)
module Value : sig
  module Metadata_file : sig
    type t = private
      { metadata : metadata
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
      { metadata : metadata
      ; (* The entries are listed in the same order that they were provided when
           storing artifacts in the cache. We keep the order to avoid confusion
           even though sorting the entres is tempting. *)
        entries : Metadata_entry.t list
      }

    val store
      :  Metadata_entry.t list
      -> mode:Mode.t
      -> rule_digest:Dune_digest.t
      -> Store_result.t
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
