(* This module is an unsafe implementation of [Univ_map] that should be faster for
   small-sized maps. The principle is that copying a small array is, in fact, faster and
   requires less allocation than updating a persistent map.

   We represent the map as an [Obj.t array], and inserting and retrieval are performed by
   coercing to and from the expected types, thus allowing the collection to contain values
   of different types.

   The interface of this module ensures type safety by requiring each index to be created
   at a specific type. A value can only be retrieved using the same index that was used to
   store it, which means that the type of the value will be what the caller expects. *)

type t = Obj.t array

let empty = [||]

module Key = struct
  type 'a t = int

  let next_key = ref 0
  let initial_values : Obj.t array ref = ref [||]

  let create initial_value =
    let key = !next_key in
    incr next_key;
    initial_values
    := Array.init (key + 1) (fun i ->
         if i < key then !initial_values.(i) else Obj.repr initial_value);
    key
  ;;
end

let get (type a) (t : t) key : a =
  if key < Array.length t
  then (Obj.magic : Obj.t -> a) t.(key)
  else (Obj.magic : Obj.t -> a) !Key.initial_values.(key)
;;

let set (type a) (t : t) (key : a Key.t) (x : a) : t =
  let copy =
    if key < Array.length t then Array.copy t else Array.init (key + 1) (fun i -> get t i)
  in
  copy.(key) <- Obj.repr x;
  copy
;;

let update (type a) (t : t) (key : a Key.t) ~(f : a -> a) : t =
  let old = get t key in
  let new_ = f old in
  set t key new_
;;
