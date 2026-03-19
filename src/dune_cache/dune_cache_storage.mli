(** Basic functionality for manipulating the Dune cache storage, used by the
    local and cloud caches. *)

open Import

module Store_result : sig
  (** Outcomes are ordered in the order of severity. *)
  type t =
    | Stored
    | Already_present
    | Error of exn
    (** [Error _] can happen due to genuine problems (cannot parse internal
        cache files) or harmless ones (race with a concurrent change to the
        cache). *)
    | Will_not_store_due_to_non_determinism of Sexp.t

  (** We consider [Will_not_store_due_to_non_determinism] as an error of higher
      severity compared to [Error], so we make sure to propagate it all the way
      up. *)
  val combine : t -> t -> t

  (** This is a neutral result with respect to the above function [combine], so
      it can be used as starting value when accumulating multiple results. *)
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

(** A [Value] entry corresponds to the standard output of an action. *)
module Value : sig
  module Metadata_file : sig
    type t =
      { metadata : Sexp.t list
      ; value_digest : Digest.t
      }

    (** Restore value metadata produced by an action with a given digest. The
        metadata is restored only in memory, i.e. no new files will be created. *)
    val restore : action_digest:Digest.t -> t Restore_result.t
  end

  (** Restore a [string] value produced by an action with a given digest. The
      value is restored only in memory, i.e. no new files will be created. *)
  val restore : action_digest:Digest.t -> string Restore_result.t
end

(** An [Artifacts] entry corresponds to the targets produced by an action. *)
module Artifacts : sig
  module Metadata_entry : sig
    type t =
      { path : string (** Can have more than one component for directory targets *)
      ; digest : Digest.t option
        (** This digest is always present in case [file_path] points to a file, and absent when it's a directory. *)
      }
  end

  module Metadata_file : sig
    type t =
      { metadata : Sexp.t list
      ; entries : Metadata_entry.t list
      }

    (** Store artifacts metadata produced by a rule with a given digest. If
        successful, this operation will create one metadata entry in the cache. *)
    val store : t -> mode:Mode.t -> rule_digest:Digest.t -> Store_result.t
  end

  (** List entries of a metadata file produced by a rule with a given digest.
      The list of entries is restored only in memory, i.e. no new files will be
      created. *)
  val list : rule_digest:Digest.t -> Metadata_entry.t list Restore_result.t
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
      -> rule_or_action_digest:Digest.t
      -> t Restore_result.t
  end
end

val clear : unit -> unit
