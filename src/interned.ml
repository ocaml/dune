open Import

module type S = sig
  type t
  val compare : t -> t -> Ordering.t
  val to_string : t -> string
  val make : string -> t
  val get : string -> t option
  module Set : sig
    include Set.S with type elt = t
    val make : string list -> t
  end
  module Map : Map.S with type key = t
  module Table : sig
    type key = t
    type 'a t
    val create : default_value:'a -> 'a t
    val get : 'a t -> key -> 'a
    val set : 'a t -> key:key -> data:'a -> unit
  end with type key := t
end

module Make() = struct
  include Int

  let ids = Hashtbl.create 1024
  let next = ref 0

  module Table = struct
    type 'a t =
      { default_value : 'a
      ; mutable data  : 'a array
      }

    let create ~default_value =
      { default_value
      ; data = [||]
      }

    let resize t =
      let increment_size = 512 in
      let n = (!next land (lnot (increment_size - 1))) + (increment_size * 2) in
      let old_data = t.data                       in
      let new_data = Array.make n t.default_value in
      t.data <- new_data;
      Array.blit
        ~src:old_data ~src_pos:0
        ~dst:new_data ~dst_pos:0
        ~len:(Array.length old_data)

    let get t key =
      if key >= Array.length t.data then
        t.default_value
      else
        t.data.(key)

    let set t ~key ~data =
      if key >= Array.length t.data then resize t;
      t.data.(key) <- data
  end

  let names = Table.create ~default_value:""

  let make s =
    Hashtbl.find_or_add ids s ~f:(fun s ->
      let n = !next in
      next := n + 1;
      Table.set names ~key:n ~data:s;
      n)

  let get s = Hashtbl.find ids s

  let to_string t = Table.get names t

  module Set = struct
    include Int_set

    let make l =
      List.fold_left l ~init:empty ~f:(fun acc s -> add acc (make s))
  end

  module Map = Int_map
end
