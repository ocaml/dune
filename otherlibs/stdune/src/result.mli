(** Result type *)

type ('a, 'error) t = ('a, 'error) result =
  | Ok of 'a
  | Error of 'error

val return : 'a -> ('a, _) t
val ok : 'a -> ('a, _) t

(** [value r ~default] is [v] if [r] is [Ok v] and [default] otherwise. *)
val value : ('a, 'e) result -> default:'a -> 'a

val is_ok : _ t -> bool
val is_error : _ t -> bool
val iter : ('a, _) t -> f:('a -> unit) -> unit
val ok_exn : ('a, exn) t -> 'a
val try_with : (unit -> 'a) -> ('a, exn) t
val equal : ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
val hash : ('a -> int) -> ('b -> int) -> ('a, 'b) t -> int

module O : sig
  val ( >>| ) : ('a, 'error) t -> ('a -> 'b) -> ('b, 'error) t
  val ( >>= ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
  val ( let* ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
  val ( and+ ) : ('a, 'error) t -> ('b, 'error) t -> ('a * 'b, 'error) t
  val ( let+ ) : ('a, 'error) t -> ('a -> 'b) -> ('b, 'error) t
end

val map : ('a, 'error) t -> f:('a -> 'b) -> ('b, 'error) t
val bind : ('a, 'error) t -> f:('a -> ('b, 'error) t) -> ('b, 'error) t
val ( >>= ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
val map_error : ('a, 'error1) t -> f:('error1 -> 'error2) -> ('a, 'error2) t
val to_option : ('a, 'error) t -> 'a option
val to_dyn : 'a Dyn.builder -> 'error Dyn.builder -> ('a, 'error) t Dyn.builder

(** Produce [Error <message>] *)
val errorf : ('a, unit, string, (_, string) t) format4 -> 'a

(** For compatibility with some other code *)
type ('a, 'error) result = ('a, 'error) t

val to_either : ('a, 'b) t -> ('b, 'a) Either.t

module List : sig
  val map : 'a list -> f:('a -> ('b, 'e) t) -> ('b list, 'e) t
  val all : ('a, 'error) t list -> ('a list, 'error) t
  val iter : 'a list -> f:('a -> (unit, 'error) t) -> (unit, 'error) t
  val concat_map : 'a list -> f:('a -> ('b list, 'error) t) -> ('b list, 'error) t

  val fold_left
    :  'a list
    -> f:('acc -> 'a -> ('acc, 'c) result)
    -> init:'acc
    -> ('acc, 'c) result

  val filter_map : 'a list -> f:('a -> ('b option, 'error) t) -> ('b list, 'error) t
end

module Option : sig
  val iter : ('a, 'e) t option -> f:('a -> (unit, 'e) t) -> (unit, 'e) t
end
