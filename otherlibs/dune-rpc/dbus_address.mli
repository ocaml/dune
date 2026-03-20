(* Taken from the ocaml-obus project with permission from Jeremie Dimino
   <jeremie@dimino.org> *)

(** Type of an address *)
type t =
  { name : string (** The transport name *)
  ; args : (string * string) list (** Arguments of the address *)
  }

(** {6 To/from string conversion} *)

type error =
  { position : int
  ; reason : string
  }

(** [of_string str] parse [str] and return the address in it. *)
val of_string : string -> (t, error) result

(** [to_string addresses] return a string representation of a list of addresses *)
val to_string : t -> string
