open Stdune

(** Simple glob support library. *)

type t

(** A glob that matches nothing *)
val empty : t

(** A glob that matches anything (including the strings starting with a ".") *)
val universal : t

(** Tests if string matches the glob. *)
val test : t -> string -> bool

(** Returns textual representation of a glob. *)
val to_string : t -> string

(** Converts string to glob. Throws [Invalid_argument] exception if string is
    not a valid glob. *)
val of_string : string -> t

val of_string_result : string -> (t, int * string) result
val to_dyn : t -> Dyn.t
val of_string_exn : Loc.t -> string -> t
val compare : t -> t -> Ordering.t
val hash : t -> int

(** [matching_extensions xs] return a glob that will match any of the extensions
    in [xs] *)
val matching_extensions : Filename.Extension.t list -> t
