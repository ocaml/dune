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

open Stdune
open Import

module Store_artifacts_result : sig
  (* Outcomes are ordered in the order of severity. *)
  type t =
    | Stored of (Path.Build.t * Digest.t) list
    | Already_present of (Path.Build.t * Digest.t) list
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
val store_artifacts :
     mode:Dune_cache_storage.Mode.t
  -> rule_digest:Digest.t
  -> compute_digest:(executable:bool -> Path.t -> Digest.t Or_exn.t Fiber.t)
  -> Target.t list
  -> Store_artifacts_result.t Fiber.t

(** Restore targets produced by a rule with a given digest. If successful, this
    operation will restore the targets on disk, in the [target_dir] directory,
    and will also return their paths and digests. The caller is responsible for
    removing stale versions of the targets, if any, before calling this
    function. *)
val restore_artifacts :
     mode:Dune_cache_storage.Mode.t
  -> rule_digest:Digest.t
  -> target_dir:Path.Build.t
  -> (Path.Build.t * Digest.t) list Restore_result.t
