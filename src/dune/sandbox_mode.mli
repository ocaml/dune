(** How to sandbox actions *)

(** This module describes the method used to sandbox actions. Choices include:

    - not sandboxing - sandboxing by symlinking dependencies - sandboxing by
      copying dependencies *)

open! Stdune

type some =
  | Symlink
  | Copy

type t = some option

val compare : t -> t -> Ordering.t

val equal : t -> t -> bool

module Dict : sig
  type key = t

  type 'a t =
    { none : 'a
    ; symlink : 'a
    ; copy : 'a
    }

  val compare : ('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t

  val of_func : (key -> 'a) -> 'a t

  val get : 'a t -> key -> 'a
end

module Set : sig
  type key = t

  type t = bool Dict.t

  val equal : t -> t -> bool

  val compare : t -> t -> Ordering.t

  val of_func : (key -> bool) -> t

  val mem : t -> key -> bool

  val inter : t -> t -> t
end

val all : t list

val none : t

val symlink : t

val copy : t

val of_string : string -> (t, string) Result.t

val to_string : t -> string
