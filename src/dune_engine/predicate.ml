open Stdune

type 'a t =
  { id : Dyn.t Lazy.t
  ; f : 'a -> bool
  }

let compare x y = Dyn.compare (Lazy.force x.id) (Lazy.force y.id)

let equal x y = compare x y = Ordering.Eq

let hash t = Dyn.hash (Lazy.force t.id)

let to_dyn t =
  let open Dyn in
  Record [ ("id", Lazy.force t.id) ]

let encode _ = Dune_lang.Encoder.string "predicate <opaque>"

let create ~id ~f = { id; f }

let true_ = { id = lazy (String "true_"); f = (fun _ -> true) }

let false_ = { id = lazy (String "false_"); f = (fun _ -> false) }

let test t e = t.f e

let contramap t ~f ~map_id =
  { f = (fun s -> t.f (f s)); id = lazy (map_id (Lazy.force t.id)) }
