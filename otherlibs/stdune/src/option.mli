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

(** [first_some t1 t2] returns [t1] if it has an underlying value, or [t2]
    otherwise. *)
val first_some : 'a t -> 'a t -> 'a t

val compare : ('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t
val try_with : (unit -> 'a) -> 'a option

module List : sig
  val all : 'a option list -> 'a list option

  (** Like [all (List.map t ~f)] but short-circuits on the first [None]. *)
  val traverse : 'a list -> f:('a -> 'b option) -> 'b list option
end

val merge : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a t

(** A poor man's unboxed option type. The value stored must not be immediate,
    unless it is a non-negative integer. In particular, unboxed options cannot
    be nested. *)
module Unboxed : sig
  type 'a t

  val none : 'a t
  val some : 'a -> 'a t
  val is_none : 'a t -> bool
  val is_some : 'a t -> bool
  val value_exn : 'a t -> 'a
  val to_option : 'a t -> 'a option
  val iter : 'a t -> f:('a -> unit) -> unit
  val match_ : 'a t -> none:(unit -> 'b) -> some:('a -> 'b) -> 'b
  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
end
