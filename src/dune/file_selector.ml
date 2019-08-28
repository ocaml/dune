open Stdune

module Predicate = struct
  type t =
    | Predicate of string Predicate.t
    | Glob of Glob.t

  let test = function
    | Predicate predicate -> Predicate.test predicate
    | Glob glob -> Glob.test glob

  let compare x y =
    match (x, y) with
    | Predicate x, Predicate y -> Predicate.compare x y
    | Predicate _, _ -> Lt
    | _, Predicate _ -> Gt
    | Glob x, Glob y -> Glob.compare x y

  let hash = function
    | Predicate predicate -> Hashtbl.hash (0, Predicate.hash predicate)
    | Glob glob -> Hashtbl.hash (1, Glob.hash glob)

  let to_dyn =
    let open Dyn in
    function
    | Predicate predicate ->
      Variant ("predicate", [ Predicate.to_dyn predicate ])
    | Glob glob -> Variant ("glob", [ Glob.to_dyn glob ])

  let encode =
    let open Dune_lang.Encoder in
    function
    | Predicate predicate ->
      record [ ("predicate", Predicate.encode predicate) ]
    | Glob glob -> record [ ("glob", Glob.encode glob) ]
end

type t =
  { dir : Path.t
  ; predicate : Predicate.t
  }

let dir t = t.dir

let compare x y =
  match Path.compare x.dir y.dir with
  | (Ordering.Lt | Gt) as a -> a
  | Eq -> Predicate.compare x.predicate y.predicate

let create ~dir predicate = { dir; predicate = Predicate predicate }

let from_glob ~dir glob = { dir; predicate = Glob glob }

let to_dyn { dir; predicate } =
  let open Dyn in
  Record [ ("dir", Path.to_dyn dir); ("predicate", Predicate.to_dyn predicate) ]

let encode { dir; predicate } =
  let open Dune_lang.Encoder in
  record
    [ ("dir", Dpath.encode dir); ("predicate", Predicate.encode predicate) ]

let equal x y = compare x y = Eq

let hash { dir; predicate } =
  Tuple.T2.hash Path.hash Predicate.hash (dir, predicate)

let test t path = Predicate.test t.predicate (Path.basename path)
