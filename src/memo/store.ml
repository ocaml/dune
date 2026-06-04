open Stdune

type ('k, 'v) t = (module Store_intf.Instance with type key = 'k and type value = 'v)

let make (type k v) (module S : Store_intf.S with type key = k) : (k, v) t =
  (module struct
    type key = k
    type value = v
    type t = v S.t

    let store = S.create ()
    let set = S.set
    let find = S.find

    (* [S] only provides [find] and [set], so custom stores fall back to two lookups. The
       default store ([of_table]) below overrides this with a single-lookup
       implementation. *)
    let find_or_add t key ~f =
      match S.find t key with
      | Some v -> v
      | None ->
        let v = f key in
        S.set t key v;
        v
    ;;

    let clear = S.clear
    let iter = S.iter
  end)
;;

let clear (type k v) ((module S) : (k, v) t) = S.clear S.store
let set (type k v) ((module S) : (k, v) t) (k : k) (v : v) = S.set S.store k v
let find (type k v) ((module S) : (k, v) t) (k : k) : v option = S.find S.store k

let find_or_add (type k v) ((module S) : (k, v) t) (k : k) ~(f : k -> v) : v =
  S.find_or_add S.store k ~f
;;

let iter (type k v) ((module S) : (k, v) t) ~(f : v -> unit) : unit = S.iter S.store ~f

let of_table (type k v) (table : (k, v) Table.t) : (k, v) t =
  (module struct
    type key = k
    type value = v

    let store = table

    type t = (key, v) Table.t

    let clear = Table.clear
    let find = Table.find
    let find_or_add = Table.find_or_add
    let set = Table.set
    let iter = Table.iter
  end)
;;
