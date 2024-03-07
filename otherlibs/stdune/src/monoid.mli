(** Monoids and commutative monoids. *)
module type Basic = Monoid_intf.Basic

module type S = Monoid_intf.S

(** This functor extends the basic definition of a monoid by adding a convenient
    operator synonym [( @ ) = combine], as well as derived functions [reduce]
    and [map_reduce]. *)
module Make (M : Basic) : S with type t := M.t

(** The monoid you get with [empty = false] and [combine = ( || )]. *)
module Exists : S with type t = bool

(** The monoid you get with [empty = true] and [combine = ( && )]. *)
module Forall : S with type t = bool

(** The string concatenation monoid with [empty = ""] and [combine = ( ^ )]. *)
module String : S with type t = string

(** The list monoid with [empty = []] and [combine = ( @ )]. *)
module List (M : sig
    type t
  end) : S with type t = M.t list

(** The list monoid with [empty = []] and [combine = ( @ )]. *)
module Appendable_list (M : sig
    type t
  end) : S with type t = M.t Appendable_list.t

(** The trivial monoid with [empty = ()] and [combine () () = ()]. *)
module Unit : S with type t = Unit.t

(** The addition monoid with [empty = zero] and [combine = ( + )]. *)
module Add (M : sig
    type t

    val zero : t
    val ( + ) : t -> t -> t
  end) : S with type t = M.t

(** The multiplication monoid with [empty = one] and [combine = ( * )]. *)
module Mul (M : sig
    type t

    val one : t
    val ( * ) : t -> t -> t
  end) : S with type t = M.t

(** The union monoid with [empty = M.empty] and [combine = M.union]. *)
module Union (M : sig
    type t

    val empty : t
    val union : t -> t -> t
  end) : S with type t = M.t

(** The product of monoids where pairs are combined component-wise. *)
module Product (A : Basic) (B : Basic) : S with type t = A.t * B.t

(** Same as [Product] but for 3 monoids. *)
module Product3 (A : Basic) (B : Basic) (C : Basic) : S with type t = A.t * B.t * C.t

(** Functions that return a monoid form the following monoid:

    - empty = fun _ -> M.empty
    - combine f g = fun x -> M.combine (f x) (g x) *)
module Function
    (A : sig
       type t
     end)
    (M : Basic) : S with type t = A.t -> M.t

(** Endofunctions, i.e., functions of type [t -> t], form two monoids. *)
module Endofunction : sig
  (** The left-to-right function composition monoid, where the argument is first
      passed to the leftmost function:

      - empty = fun x -> x
      - combine f g = fun x -> g (f x) *)
  module Left (A : sig
      type t
    end) : S with type t = A.t -> A.t

  (** The right-to-left function composition monoid, where the argument is first
      passed to the rightmost function:

      - empty = fun x -> x
      - combine f g = fun x -> f (g x) *)
  module Right (A : sig
      type t
    end) : S with type t = A.t -> A.t
end

(** Commutative monoids. *)
module Commutative : sig
  module type Basic = Monoid_intf.Commutative.Basic
  module type S = Monoid_intf.Commutative.S

  (** This functor extends the basic definition of a commutative monoid by
      adding a convenient operator synonym [( @ ) = combine], as well as derived
      functions [reduce] and [map_reduce]. *)
  module Make (M : Basic) : S with type t := M.t

  (** The commutative monoid you get with [empty = false] and
      [combine = ( || )]. *)
  module Exists : S with type t = bool

  (** The commutative monoid you get with [empty = true] and [combine = ( && )]. *)
  module Forall : S with type t = bool

  (** The trivial commutative monoid with [empty = ()] and [combine () () = ()]. *)
  module Unit : S with type t = Unit.t

  (** The addition monoid with [empty = zero] and [combine = ( + )]. *)
  module Add (M : sig
      type t

      val zero : t
      val ( + ) : t -> t -> t
    end) : S with type t = M.t

  (** The multiplication monoid with [empty = one] and [combine = ( * )]. *)
  module Mul (M : sig
      type t

      val one : t
      val ( * ) : t -> t -> t
    end) : S with type t = M.t

  (** The union monoid with [empty = M.empty] and [combine = M.union]. *)
  module Union (M : sig
      type t

      val empty : t
      val union : t -> t -> t
    end) : S with type t = M.t

  (** The product of commutative monoids where pairs are combined
      component-wise. *)
  module Product (A : Basic) (B : Basic) : S with type t = A.t * B.t

  (** Same as [Product] but for 3 commutative monoids. *)
  module Product3 (A : Basic) (B : Basic) (C : Basic) : S with type t = A.t * B.t * C.t

  (** Functions that return a commutative monoid form the following commutative
      monoid:

      - empty = fun _ -> M.empty
      - combine f g = fun x -> M.combine (f x) (g x) *)
  module Function
      (A : sig
         type t
       end)
      (M : Basic) : S with type t = A.t -> M.t
end
