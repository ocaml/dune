module type Basic = Monoid_intf.Basic
module type S = Monoid_intf.S

module Make (M : Basic) = struct
  include M

  module O = struct
    let ( @ ) = combine
  end

  let reduce = List.fold_left ~init:empty ~f:combine
  let map_reduce ~f = List.fold_left ~init:empty ~f:(fun acc a -> combine acc (f a))
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
  end) =
Make (struct
    type t = M.t list

    let empty = []
    let combine = ( @ )
  end)

module Appendable_list (M : sig
    type t
  end) =
Make (struct
    type t = M.t Appendable_list.t

    let empty = Appendable_list.empty
    let combine = Appendable_list.( @ )
  end)

module Unit = Make (struct
    include Unit

    let empty = ()
    let combine () () = ()
  end)

module type Add = sig
  type t

  val zero : t
  val ( + ) : t -> t -> t
end

module Add (M : Add) = Make (struct
    include M

    let empty = zero
    let combine = ( + )
  end)

module type Mul = sig
  type t

  val one : t
  val ( * ) : t -> t -> t
end

module Mul (M : Mul) = Make (struct
    include M

    let empty = one
    let combine = ( * )
  end)

module type Union = sig
  type t

  val empty : t
  val union : t -> t -> t
end

module Union (M : Union) = Make (struct
    include M

    let combine = union
  end)

module Product (A : Basic) (B : Basic) = Make (struct
    type t = A.t * B.t

    let empty = A.empty, B.empty
    let combine (a1, b1) (a2, b2) = A.combine a1 a2, B.combine b1 b2
  end)

module Product3 (A : Basic) (B : Basic) (C : Basic) = Make (struct
    type t = A.t * B.t * C.t

    let empty = A.empty, B.empty, C.empty

    let combine (a1, b1, c1) (a2, b2, c2) =
      A.combine a1 a2, B.combine b1 b2, C.combine c1 c2
    ;;
  end)

module Function
    (A : sig
       type t
     end)
    (M : Basic) =
Make (struct
    type t = A.t -> M.t

    let empty _ = M.empty
    let combine f g x = M.combine (f x) (g x)
  end)

module Endofunction = struct
  module Left (A : sig
      type t
    end) =
  Make (struct
      type t = A.t -> A.t

      let empty x = x
      let combine f g x = g (f x)
    end)

  module Right (A : sig
      type t
    end) =
  Make (struct
      type t = A.t -> A.t

      let empty x = x
      let combine f g x = f (g x)
    end)
end

module Commutative = struct
  (* Inject the "proof" of commutativity into a give monoid. *)
  module Make_commutative (M : S) = struct
    include M

    type combine_is_commutative = unit
  end

  module type Basic = Monoid_intf.Commutative.Basic
  module type S = Monoid_intf.Commutative.S

  module Make (M : Basic) = Make_commutative (Make (M))
  module Exists = Make_commutative (Exists)
  module Forall = Make_commutative (Forall)
  module Unit = Make_commutative (Unit)
  module Add (M : Add) = Make_commutative (Add (M))
  module Mul (M : Mul) = Make_commutative (Mul (M))
  module Union (M : Union) = Make_commutative (Union (M))
  module Product (A : Basic) (B : Basic) = Make_commutative (Product (A) (B))

  module Product3 (A : Basic) (B : Basic) (C : Basic) =
    Make_commutative (Product3 (A) (B) (C))

  module Function
      (A : sig
         type t
       end)
      (M : Basic) =
    Make_commutative (Function (A) (M))
end
