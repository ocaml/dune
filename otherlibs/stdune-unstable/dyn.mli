type t =
  | Opaque
  | Unit
  | Int of int
  | Int64 of int64
  | Bool of bool
  | String of string
  | Bytes of bytes
  | Char of char
  | Float of float
  | Option of t option
  | List of t list
  | Array of t array
  | Tuple of t list
  | Record of (string * t) list
  | Variant of string * t list
  | Map of (t * t) list
  | Set of t list

module Encoder : sig
  type dyn

  type 'a t = 'a -> dyn

  val unit : unit t

  val char : char t

  val string : string t

  val int : int t

  val int64 : int64 t

  val float : float t

  val bool : bool t

  val pair : 'a t -> 'b t -> ('a * 'b) t

  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  val list : 'a t -> 'a list t

  val array : 'a t -> 'a array t

  val option : 'a t -> 'a option t

  val record : (string * dyn) list -> dyn

  val unknown : _ t

  val opaque : _ t

  val constr : string -> dyn list -> dyn
end
with type dyn := t

val pp : t -> _ Pp.t

val opaque : t

val compare : t -> t -> Ordering.t

val hash : t -> int

val to_string : t -> string

type dyn = t
