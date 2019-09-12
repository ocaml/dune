(** Simple glob support library. *)

type t

(** Tests if string matches the glob. *)
val test : t -> string -> bool

(** Returns textual representation of a glob. *)
val to_string : t -> string

(** Converts string to glob. Throws [Invalid_argument] exception if string is
    not a valid glob. *)
val of_string : string -> t
