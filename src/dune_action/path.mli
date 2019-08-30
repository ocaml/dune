(** Representation of paths for "dune_action" library. *)

(** We shouldn't use absolute paths when communicating with Dune, so this
  module allows user to represent only relative paths. *)

type t

module O : sig
  (** Concatenate two paths. *)
  val ( ^/ ) : t -> t -> t
end

(** Concatenate two paths. *)
val concat : t -> t -> t

val to_string : t -> string

(** Convert path to string. Throws an exception if passed string is not a
  relative path. *)
val of_string : string -> t
