(** This is a private library without a public_name. *)

(** A type in the private library. *)
type t = int

(** A function in the private library. *)
val identity : t -> t
