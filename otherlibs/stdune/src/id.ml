module type S = sig
  type t [@@immediate]

  include Comparable_intf.S with type key := t
  module Table : Hashtbl.S with type key = t

  val gen : unit -> t
  val peek : unit -> t
  val to_int : t -> int
  val compare : t -> t -> Ordering.t
  val equal : t -> t -> bool
  val hash : t -> int
  val to_dyn : t -> Dyn.t
end

module Make () : S = struct
  include Int
  module Table = Hashtbl.Make (Int)

  let next = ref 0

  let gen () =
    let v = !next in
    next := v + 1;
    v
  ;;

  let peek () = !next
  let to_int x = x
end
