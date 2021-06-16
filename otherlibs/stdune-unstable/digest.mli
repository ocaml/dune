(** Digests (MD5) *)

type t

module Set : Set.S with type elt = t

module Map : Map.S with type key = t

val to_dyn : t -> Dyn.t

val hash : t -> int

val equal : t -> t -> bool

val compare : t -> t -> Ordering.t

val to_string : t -> string

val from_hex : string -> t option

val file : Path.t -> t

val string : string -> t

val to_string_raw : t -> string

val generic : 'a -> t

(** The total time spent in the function [generic] during the current build. *)
val generic_timer : Metrics.Timer.t

(** Digest a file and its stats. Does something sensible for directories. *)
val file_with_stats : Path.t -> Unix.stats -> t

(** Digest a file taking its executable bit into account. Should not be called
    on a directory. *)
val file_with_executable_bit : executable:bool -> Path.t -> t

(** Override the implementations of digest computation. Can be used to record
    the reverse digest map. *)
val override_impl : file:(string -> t) -> string:(string -> t) -> unit

(** [Direct_impl] does a plain hashing, with no heed to the overrides given by
    [override_impl]. *)
module Direct_impl : sig
  val string : string -> t
end
