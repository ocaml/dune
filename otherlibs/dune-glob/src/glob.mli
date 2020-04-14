(** Simple glob support library. *)

type t

val empty : t
(** A glob that matches nothing *)

val universal : t
(** A glob that matches anything (including the strings starting with a ".") *)

val test : t -> string -> bool
(** Tests if string matches the glob. *)

val to_string : t -> string
(** Returns textual representation of a glob. *)

val of_string : string -> t
(** Converts string to glob. Throws [Invalid_argument] exception if string is
    not a valid glob. *)

val of_string_result : string -> (t, int * string) result
