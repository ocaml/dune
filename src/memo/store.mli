type ('a, 'b) t

val make : (module Store_intf.S with type key = 'a) -> ('a, 'b) t
val set : ('a, 'b) t -> 'a -> 'b -> unit
val clear : ('a, 'b) t -> unit
val find : ('a, 'b) t -> 'a -> 'b option

(** [find_or_add t key ~f] returns the value associated with [key], adding [f key] if it
    is missing. A single hash-table lookup, unlike [find] then [set]. *)
val find_or_add : ('a, 'b) t -> 'a -> f:('a -> 'b) -> 'b

val iter : ('a, 'b) t -> f:('b -> unit) -> unit
val of_table : ('a, 'b) Stdune.Table.t -> ('a, 'b) t
