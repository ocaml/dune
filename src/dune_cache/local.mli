(** This module implements a local cache of build results that are shared across
    multiple Dune instances running on the same machine. See [doc/dec/cache.md]
    for design and implementation notes.

    This is meant to be used by several Dune instances running concurrently, and
    with concurrent evictions from the cache.

    The files in the cache are assumed to be immutable, which is mostly enforced
    by removing write permissions to any file in the cache, but of course anyone
    could add back write permissions and corrupt the cache. *)

(* In case we do run into the problem of corrupted cache: we could actually
   store the mtime in the metadata and complain if it's not what we expected. *)

open Stdune
module Store_result := Dune_cache_storage.Store_result
module Restore_result := Dune_cache_storage.Restore_result

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

module Check_artifacts_result : sig
  type t =
    | Missing
    | Already_present of (Path.Build.t * Digest.t) list
    | Error of exn
        (** [Error _] can happen due to genuine problems (cannot parse internal
            cache files) or harmless ones (race with a concurrent change to the
            cache). *)
    | Non_determinism_detected of Sexp.t
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

val store_output :
     mode:Dune_cache_storage.Mode.t
  -> action_digest:Digest.t
  -> string
  -> Store_result.t

val restore_output : action_digest:Digest.t -> string Restore_result.t

(** The [compute_digest] function is passed explicitly because the caller might
    want to memoize and/or throttle file digest computations. *)
val store_artifacts :
     mode:Dune_cache_storage.Mode.t
  -> rule_digest:Digest.t
  -> compute_digest:(executable:bool -> Path.t -> Digest.t Or_exn.t Fiber.t)
  -> Target.t list
  -> Store_artifacts_result.t Fiber.t

val restore_artifacts :
     mode:Dune_cache_storage.Mode.t
  -> rule_digest:Digest.t
  -> target_dir:Path.Build.t
  -> (Path.Build.t * Digest.t) list Restore_result.t
