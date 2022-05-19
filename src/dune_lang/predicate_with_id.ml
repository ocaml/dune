open Stdune

type 'a t =
  { id : Dyn.t Lazy.t
  ; f : 'a Predicate.t
  }

let predicate t = t.f

let compare x y = Dyn.compare (Lazy.force x.id) (Lazy.force y.id)

let equal x y = compare x y = Ordering.Eq

let hash t = Dyn.hash (Lazy.force t.id)

let to_dyn t =
  let open Dyn in
  Record [ ("id", Lazy.force t.id) ]

let encode _ = Encoder.string "predicate <opaque>"

let create ~id ~f = { id; f = Predicate.create f }

let true_ = { id = lazy (String "true_"); f = Predicate.true_ }

let false_ = { id = lazy (String "false_"); f = Predicate.false_ }

let test t e = Predicate.test t.f e

let contramap t ~f ~map_id =
  { f = Predicate.contramap t.f ~f; id = lazy (map_id (Lazy.force t.id)) }
