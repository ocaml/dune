open! Stdune

type t

val pp : t Fmt.t

val decode : t Stanza.Decoder.t

val empty : t

val filter : t -> standard:t -> string list -> string list

val of_glob : Glob.t -> t

val compl : t -> t

val union : t list -> t

val diff : t -> t -> t

val of_string_set : String.Set.t -> t
