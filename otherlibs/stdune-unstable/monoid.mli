module type Basic = Monoid_intf.Basic

module type Monoid = Monoid_intf.Monoid

(** This functor extends the basic definition of a monoid by adding a convenient
    operator synonym [( @ ) = combine], as well as derived functions [reduce],
    [map_reduce] and [times]. *)
module Make (M : Basic) : Monoid with type t = M.t

(** The monoid you get with [empty = false] and [combine = ( || )]. *)
module Exists : Monoid with type t = bool

(** The monoid you get with [empty = true] and [combine = ( && )]. *)
module Forall : Monoid with type t = bool

(** The string concatenation monoid with [empty = ""] and [combine = ( ^ )]. *)
module String : Monoid with type t = string

(** The list monoid with [empty = \[\]] and [combine = ( @ )]. *)
module List (M : sig
  type t
end) : Monoid with type t = M.t list

(** The trivial monoid with [empty = ()] and [combine () () = ()]. *)
module Unit : Monoid with type t = Unit.t

(** The addition monoid with [empty = zero] and [combine = ( + )]. *)
module Add (M : sig
  type t

  val zero : t

  val ( + ) : t -> t -> t
end) : Monoid with type t = M.t

(** The multiplication monoid with [empty = one] and [combine = ( * )]. *)
module Mul (M : sig
  type t

  val one : t

  val ( * ) : t -> t -> t
end) : Monoid with type t = M.t

(** The union monoid with [empty = M.empty] and [combine = M.union]. *)
module Union (M : sig
  type t

  val empty : t

  val union : t -> t -> t
end) : Monoid with type t = M.t

(** The product of monoids where pairs are combined component-wise. *)
module Product (A : Basic) (B : Basic) : Monoid with type t = A.t * B.t

(** Flip the order of arguments to [combine]:

    - empty = M.empty
    - combine x y = M.combine y x *)
module Dual (M : Basic) : Monoid with type t = M.t

(** Functions that return a monoid form the following monoid:

    - empty = fun _ -> M.empty
    - combine f g = fun x -> M.combine (f x) (g x) *)
module Function (A : sig
  type t
end)
(M : Basic) : Monoid with type t = A.t -> M.t

(** Endofunctions, i.e., functions of type [t -> t] form the following monoid:

    - empty = fun x -> x
    - combine f g = fun x -> f (g x) *)
module Endofunction (A : sig
  type t
end) : Monoid with type t = A.t -> A.t
