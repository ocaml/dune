open Stdune

type 'a t =
  { id : Sexp.t Lazy.t
  ; f : 'a -> bool
  }

let compare x y = Sexp.compare (Lazy.force x.id) (Lazy.force y.id)
let equal x y = compare x y = Ordering.Eq
let hash t = Sexp.hash (Lazy.force t.id)

let to_sexp t = Sexp.Encoder.constr "Predicate" [Lazy.force t.id]

let to_dyn t =
  let open Dyn in
  Record
    [ "id", Sexp.to_dyn (Lazy.force t.id)
    ]

let encode _ = Dune_lang.Encoder.string "predicate <opaque>"

let create ~id ~f =
  { id
  ; f
  }

let test t e = t.f e

let contramap t ~f ~map_id =
  { f = (fun s -> t.f (f s))
  ; id = lazy (map_id (Lazy.force t.id))
  }

let pp fmt t = Dyn.pp fmt (to_dyn t)
