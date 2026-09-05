type t

val gen : unit -> t
val to_string : t -> string
val of_string : string -> t
val conv : t Conv.value

module Table : Stdune.Hashtbl.S with type key = t
