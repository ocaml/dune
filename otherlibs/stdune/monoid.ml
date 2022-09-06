module type Basic = Monoid_intf.Basic

module type S = Monoid_intf.S

module Make (M : Basic) : Monoid_intf.S with type t = M.t = struct
  include M

  module O = struct
    let ( @ ) = combine
  end

  let reduce = List.fold_left ~init:empty ~f:combine

  let map_reduce ~f =
    List.fold_left ~init:empty ~f:(fun acc a -> combine acc (f a))
end
[@@inline always]

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
end) : Monoid_intf.S with type t = M.t list = Make (struct
  type t = M.t list

  let empty = []

  let combine = ( @ )
end)

module Appendable_list (M : sig
  type t
end) : Monoid_intf.S with type t = M.t Appendable_list.t = Make (struct
  type t = M.t Appendable_list.t

  let empty = Appendable_list.empty

  let combine = Appendable_list.( @ )
end)

module Unit : Monoid_intf.S with type t = Unit.t = Make (struct
  include Unit

  let empty = ()

  let combine () () = ()
end)

module Add (M : sig
  type t

  val zero : t

  val ( + ) : t -> t -> t
end) : Monoid_intf.S with type t = M.t = Make (struct
  include M

  let empty = zero

  let combine = ( + )
end)

module Mul (M : sig
  type t

  val one : t

  val ( * ) : t -> t -> t
end) : Monoid_intf.S with type t = M.t = Make (struct
  include M

  let empty = one

  let combine = ( * )
end)

module Union (M : sig
  type t

  val empty : t

  val union : t -> t -> t
end) : Monoid_intf.S with type t = M.t = Make (struct
  include M

  let combine = union
end)

module Product (A : Monoid_intf.Basic) (B : Monoid_intf.Basic) :
  Monoid_intf.S with type t = A.t * B.t = Make (struct
  type t = A.t * B.t

  let empty = (A.empty, B.empty)

  let combine (a1, b1) (a2, b2) = (A.combine a1 a2, B.combine b1 b2)
end)

module Product3
    (A : Monoid_intf.Basic)
    (B : Monoid_intf.Basic)
    (C : Monoid_intf.Basic) : Monoid_intf.S with type t = A.t * B.t * C.t =
Make (struct
  type t = A.t * B.t * C.t

  let empty = (A.empty, B.empty, C.empty)

  let combine (a1, b1, c1) (a2, b2, c2) =
    (A.combine a1 a2, B.combine b1 b2, C.combine c1 c2)
end)

module Function (A : sig
  type t
end)
(M : Monoid_intf.Basic) : Monoid_intf.S with type t = A.t -> M.t = Make (struct
  type t = A.t -> M.t

  let empty _ = M.empty

  let combine f g x = M.combine (f x) (g x)
end)

module Endofunction = struct
  module Left (A : sig
    type t
  end) : Monoid_intf.S with type t = A.t -> A.t = Make (struct
    type t = A.t -> A.t

    let empty x = x

    let combine f g x = g (f x)
  end)

  module Right (A : sig
    type t
  end) : Monoid_intf.S with type t = A.t -> A.t = Make (struct
    type t = A.t -> A.t

    let empty x = x

    let combine f g x = f (g x)
  end)
end
