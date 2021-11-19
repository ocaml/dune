open Stdune

type t

val make : string -> t

val to_string : t -> string

include Comparable_intf.S with type key := t

module Table : Hashtbl.S with type key := t
