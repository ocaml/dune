open Import

(** Result of updating a cache entry. *)

type t =
  | Skipped  (** No need to update a given entry because it has no readers *)
  | Updated of { changed : bool }

(** [Skipped] is the "empty" update. *)
val empty : t

val combine : t -> t -> t

val to_dyn : t -> Dyn.t

val log_update : t -> name:string -> Path.Outside_build_dir.t -> unit
