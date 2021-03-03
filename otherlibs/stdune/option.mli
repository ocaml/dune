(** Optional values *)

type 'a t = 'a option =
  | None
  | Some of 'a

module O : sig
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

val map : 'a t -> f:('a -> 'b) -> 'b t

val bind : 'a t -> f:('a -> 'b t) -> 'b t

val iter : 'a t -> f:('a -> unit) -> unit

val forall : 'a t -> f:('a -> bool) -> bool

val value : 'a t -> default:'a -> 'a

val value_exn : 'a t -> 'a

val some : 'a -> 'a t

val some_if : bool -> 'a -> 'a t

val is_some : _ t -> bool

val is_none : _ t -> bool

val both : 'a t -> 'b t -> ('a * 'b) t

val split : ('a * 'b) t -> 'a t * 'b t

val to_list : 'a t -> 'a list

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val hash : ('a -> int) -> 'a t -> int

val compare : ('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t

val try_with : (unit -> 'a) -> 'a option

module List : sig
  val all : 'a option list -> 'a list option
end
