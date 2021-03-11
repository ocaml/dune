module type Basic = Monoid_intf.Basic

module type Monoid = Monoid_intf.Monoid

module Make (M : Basic) : Monoid with type t = M.t = struct
  include M

  module O = struct
    let ( @ ) = combine
  end

  let reduce = List.fold_left ~init:empty ~f:combine

  let map_reduce ~f =
    List.fold_left ~init:empty ~f:(fun acc a -> combine acc (f a))

  (* To compute [times t ~n] using only O(log n) operations, we can use the
     "repeated squaring" algorithm, where to compute [times t ~n:(2k)] we first
     compute [times t ~n:k] and then "square" the result, i.e. combine it with
     itself. This algorithm relies on the associativity property of monoids, to
     change the order in which operations are performed from

     (((t @ t) @ t) @ t) @ t

     as in the specification of [times t ~n:5], to

     ((t @ t) @ (t @ t)) @ t

     where the work done to compute the term [t @ t] can be shared, i.e.:

     let t2 = t @ t in let t4 = t2 @ t2 in t4 @ t

     This ability to reassociate parentheses is key to the usefulness of
     monoids. Let's see what happens if we try to reassociate parentheses when
     the operation @ is subtraction, which is not associative:

     (((1 - 1) - 1) - 1) - 1 = -3

     but

     ((1 - 1) - (1 - 1)) - 1 = -1

     Fortunately, many common operations are associative. *)
  let rec times t ~n =
    let open O in
    match n with
    | negative when negative < 0 -> failwith "exponent cannot be negative"
    | 0 -> empty
    | even when even mod 2 = 0 ->
      let t = times t ~n:(even / 2) in
      t @ t
    | odd -> times t ~n:(odd - 1) @ t
end

module Exists = Make (struct
  type t = bool

  let empty = false

  let combine = ( || )
end)

module Forall = Make (struct
  type t = bool

  let empty = true

  let combine = ( && )
end)

module String = Make (struct
  type t = string

  let empty = ""

  let combine = ( ^ )
end)

module List (M : sig
  type t
end) : Monoid with type t = M.t list = Make (struct
  type t = M.t list

  let empty = []

  let combine = ( @ )
end)

module Unit : Monoid with type t = Unit.t = Make (struct
  include Unit

  let empty = ()

  let combine () () = ()
end)

module Add (M : sig
  type t

  val zero : t

  val ( + ) : t -> t -> t
end) : Monoid with type t = M.t = Make (struct
  include M

  let empty = zero

  let combine = ( + )
end)

module Mul (M : sig
  type t

  val one : t

  val ( * ) : t -> t -> t
end) : Monoid with type t = M.t = Make (struct
  include M

  let empty = one

  let combine = ( * )
end)

module Product (A : Basic) (B : Basic) : Monoid with type t = A.t * B.t =
Make (struct
  type t = A.t * B.t

  let empty = (A.empty, B.empty)

  let combine (a1, b1) (a2, b2) = (A.combine a1 a2, B.combine b1 b2)
end)

module Dual (M : Basic) : Monoid with type t = M.t = Make (struct
  type t = M.t

  let empty = M.empty

  let combine x y = M.combine y x
end)

module Function (A : sig
  type t
end)
(M : Basic) : Monoid with type t = A.t -> M.t = Make (struct
  type t = A.t -> M.t

  let empty _ = M.empty

  let combine f g x = M.combine (f x) (g x)
end)

module Endofunction (A : sig
  type t
end) : Monoid with type t = A.t -> A.t = Make (struct
  type t = A.t -> A.t

  let empty x = x

  let combine f g x = f (g x)
end)
