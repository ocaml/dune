(** Dynamic values *)

(** Representation of OCaml values such that they can be processed without
    knowing their type. *)
type t =
  | Opaque
  | Unit
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
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

val equal : t -> t -> bool
val compare : t -> t -> Ordering.t
val hash : t -> int
val pp : t -> _ Pp.t
val to_string : t -> string

(** {1 Constructors} *)

type 'a builder = 'a -> t

val unit : unit builder
val char : char builder
val string : string builder
val int : int builder
val int32 : int32 builder
val int64 : int64 builder
val nativeint : nativeint builder
val float : float builder
val bool : bool builder
val pair : 'a builder -> 'b builder -> ('a * 'b) builder
val triple : 'a builder -> 'b builder -> 'c builder -> ('a * 'b * 'c) builder
val list : 'a builder -> 'a list builder
val array : 'a builder -> 'a array builder
val option : 'a builder -> 'a option builder
val opaque : _ builder
val record : (string * t) list -> t
val variant : string -> t list -> t
val result : 'a builder -> 'error builder -> ('a, 'error) result builder
