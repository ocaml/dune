open Import

module type S = sig
  type t

  val make : string -> t
  val compare : t -> t -> Ordering.t

  module Set : sig
    include Set.S with type elt = t

    val make : string list -> t
  end

  module Map : Map.S with type key = t
end

module Make() = struct
  include Int

  let table = Hashtbl.create 1024
  let next = ref 0

  let make s =
    Hashtbl.find_or_add table s ~f:(fun _ ->
      let n = !next in
      next := n + 1;
      n)

  module Set = struct
    include Int_set

    let make l =
      List.fold_left l ~init:empty ~f:(fun acc s -> add acc (make s))
  end

  module Map = Int_map
end
