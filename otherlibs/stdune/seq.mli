type 'a t = 'a Stdlib.Seq.t

and +'a node = 'a Stdlib.Seq.node =
  | Nil
  | Cons of 'a * 'a t

val empty : 'a t
val return : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t
val append : 'a t -> 'a t -> 'a t
val concat : 'a t t -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val filter : 'a t -> f:('a -> bool) -> 'a t
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
val fold_left : 'b t -> init:'a -> f:('a -> 'b -> 'a) -> 'a
val iter : 'a t -> f:('a -> unit) -> unit
