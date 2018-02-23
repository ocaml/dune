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
    val create : unit -> 'a t
    val get : 'a t -> key -> 'a option
    val set : 'a t -> key:key -> data:'a -> unit
  end with type key := t
end

module Make() = struct
  include Int

  let ids = Hashtbl.create 1024
  let next = ref 0

  module Table = struct
    type 'a t = 'a option array ref

    let create () = ref [||]

    let resize t =
      let increment_size = 512 in
      let n = (!next land (lnot (increment_size - 1))) + (increment_size * 2) in
      let old_array = !t                in
      let new_array = Array.make n None in
      t := new_array;
      Array.blit
        ~src:old_array ~src_pos:0
        ~dst:new_array ~dst_pos:0
        ~len:(Array.length old_array)

    let get t key =
      if key >= Array.length !t then
        None
      else
        !t.(key)

    let set t ~key ~data =
      if key >= Array.length !t then resize t;
      !t.(key) <- Some data
  end

  let names = Table.create ()

  let make s =
    Hashtbl.find_or_add ids s ~f:(fun s ->
      let n = !next in
      next := n + 1;
      Table.set names ~key:n ~data:s;
      n)

  let get s = Hashtbl.find ids s

  let to_string t = Option.value_exn (Table.get names t)

  module Set = struct
    include Int_set

    let make l =
      List.fold_left l ~init:empty ~f:(fun acc s -> add acc (make s))
  end

  module Map = Int_map
end
