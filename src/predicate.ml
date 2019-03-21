open Stdune

type 'a t =
  { id : Sexp.t Lazy.t
  ; f : 'a -> bool
  }

let equal x y = Sexp.equal (Lazy.force x.id) (Lazy.force y.id)
let hash t = Sexp.hash (Lazy.force t.id)

let to_sexp t = Sexp.Encoder.constr "Predicate" [Lazy.force t.id]

let to_dyn t =
  let open Dyn in
  Record
    [ "id", Sexp.to_dyn (Lazy.force t.id)
    ]

let create ~id ~f =
  { id
  ; f
  }

let test t e = t.f e

let contramap t ~f ~map_id =
  { f = (fun s -> t.f (f s))
  ; id = lazy (map_id (Lazy.force t.id))
  }
