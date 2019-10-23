open Stdune

type ('a, 'b) t

val make : (module Store_intf.S with type key = 'a) -> ('a, 'b) t

val set : ('a, 'b) t -> 'a -> 'b -> unit

val clear : ('a, 'b) t -> unit

val find : ('a, 'b) t -> 'a -> 'b option

val of_table : ('a, 'b) Table.t -> ('a, 'b) t
