(* TODO jstaron: Add description of this module. *)
(* Mention that this module will only allow user to use relative paths. *)

type t

module O : sig
  val ( ^/ ) : t -> t -> t
end

val concat : t -> t -> t

val to_string : t -> string

(* TODO jstaron: Document and mention that throws exceptions. *)
val of_string : string -> t
