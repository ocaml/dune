(** Representation of paths for "dune_action_plugin" library. *)

(** We shouldn't use absolute paths when communicating with Dune, so this module
    allows user to represent only relative paths. *)

type t

module O : sig
  val ( ^/ ) : t -> t -> t
  (** Concatenate two paths. *)
end

val concat : t -> t -> t
(** Concatenate two paths. *)

val to_string : t -> string

val of_string : string -> t
(** Convert path to string. Throws an Invalid_argument exception if passed
    string is not a relative path. *)
