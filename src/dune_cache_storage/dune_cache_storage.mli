(** Basic functionality for manipulating the Dune cache storage, used by the
    local and cloud caches. *)

open Stdune
module Layout = Layout
module Mode = Mode
module Util = Util

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

    val restore : action_digest:Digest.t -> t Restore_result.t
  end

  val store : mode:Mode.t -> action_digest:Digest.t -> string -> Store_result.t

  val restore : action_digest:Digest.t -> string Restore_result.t
end

(** An [Artifacts] entry corresponds to the targets produced by an action. *)
module Artifacts : sig
  module Metadata_entry : sig
    type t =
      { file_name : string
      ; file_digest : Digest.t
      }
  end

  module Metadata_file : sig
    type t =
      { metadata : Sexp.t list
      ; entries : Metadata_entry.t list
      }

    val store : t -> mode:Mode.t -> rule_digest:Digest.t -> Store_result.t

    val restore : rule_digest:Digest.t -> t Restore_result.t
  end

  val list : rule_digest:Digest.t -> Metadata_entry.t list Restore_result.t
end

module Metadata : sig
  type t =
    | Artifacts of Artifacts.Metadata_file.t
    | Value of Value.Metadata_file.t

  val restore : rule_or_action_digest:Digest.t -> t Restore_result.t
end

(** [with_temp_path ?prefix ~suffix f] creates a file in [Layout.temp_path],
    then passes it to the callback [f], and makes sure the file is deleted when
    [f] completes or raises. The base name of the temporary file is formed by
    concatenating the [prefix] (which is set to "dune" by default), then a
    suitably chosen integer number, then [suffix]. *)
val with_temp_path :
     ?prefix:string
  -> suffix:string
  -> (Path.t Or_exn.t -> 'a Fiber.t)
  -> 'a Fiber.t

(** Like [with_temp_path] but creates a directory in [Layout.temp_path]. *)
val with_temp_dir :
     ?prefix:string
  -> suffix:string
  -> (Path.t Or_exn.t -> 'a Fiber.t)
  -> 'a Fiber.t
