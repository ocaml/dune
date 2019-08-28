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

  let encode = function
    | Predicate predicate -> Predicate.encode predicate
    | Glob glob -> Glob.encode glob
end

type t =
  { dir : Path.t
  ; test : Predicate.t
  }

let dir t = t.dir

let compare x y =
  match Path.compare x.dir y.dir with
  | (Ordering.Lt | Gt) as a -> a
  | Eq -> Predicate.compare x.test y.test

let from_predicate ~dir predicate = { dir; test = Predicate predicate }

let from_glob ~dir glob = { dir; test = Glob glob }

let to_dyn { dir; test } =
  let open Dyn in
  Record [ ("dir", Path.to_dyn dir); ("test", Predicate.to_dyn test) ]

let encode { dir; test } =
  let open Dune_lang.Encoder in
  record [ ("dir", Dpath.encode dir); ("test", Predicate.encode test) ]

let equal x y = compare x y = Eq

let hash { dir; test } = Tuple.T2.hash Path.hash Predicate.hash (dir, test)

let test t path = Predicate.test t.test (Path.basename path)
