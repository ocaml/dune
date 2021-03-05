module type S = sig
  type t

  module Set : Set.S with type elt = t

  module Map : Map_intf.S with type key = t

  val gen : unit -> t

  val peek : unit -> t

  val to_int : t -> int

  val compare : t -> t -> Ordering.t

  val equal : t -> t -> bool

  val hash : t -> int

  val to_dyn : t -> Dyn.t
end

module Make () : S = struct
  module Set = Int.Set
  module Map = Int.Map

  type t = int

  let next = ref 0

  let gen () =
    let v = !next in
    next := v + 1;
    v

  let peek () = !next

  let to_int x = x

  let compare = Int.compare

  let equal = Int.equal

  let hash (t : t) = t

  let to_dyn t = Dyn.Int t
end
