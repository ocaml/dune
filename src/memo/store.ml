open! Stdune

module type Store = sig
  type key

  type 'a t

  val create : unit -> 'a t

  val clear : 'a t -> unit

  val set : 'a t -> key -> 'a -> unit

  val find : 'a t -> key -> 'a option
end

module type Store_instance = sig
  type key

  type value

  type t

  val clear : t -> unit

  val set : t -> key -> value -> unit

  val find : t -> key -> value option

  val store : t
end

type ('k, 'v) t =
  (module Store_instance with type key = 'k and type value = 'v)

let make (type k v) (module S : Store with type key = k) : (k, v) t =
  ( module struct
    type key = k

    type value = v

    type t = v S.t

    let store = S.create ()

    let set = S.set

    let find = S.find

    let clear = S.clear
  end )

let clear (type k v) ((module S) : (k, v) t) = S.clear S.store

let set (type k v) ((module S) : (k, v) t) (k : k) (v : v) = S.set S.store k v

let find (type k v) ((module S) : (k, v) t) (k : k) : v option =
  S.find S.store k

let of_table (type k v) (table : (k, v) Table.t) : (k, v) t =
  ( module struct
    type key = k

    type value = v

    let store = table

    type t = (key, v) Table.t

    let clear = Table.clear

    let find = Table.find

    let set = Table.set
  end )
