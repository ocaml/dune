open Stdune

type t =
  { dir : Path.t
  ; predicate : string Predicate.t
  ; only_generated_files : bool
  }

let dir t = t.dir

let predicate t = t.predicate

let only_generated_files t = t.only_generated_files

let compare x y =
  match Path.compare x.dir y.dir with
  | (Ordering.Lt | Gt) as a -> a
  | Eq -> Predicate.compare x.predicate y.predicate

let create ~dir ?(only_generated_files = false) predicate =
  { dir; predicate; only_generated_files }

let to_dyn { dir; predicate; only_generated_files } =
  let open Dyn in
  Record
    [ ("dir", Path.to_dyn dir)
    ; ("predicate", Predicate.to_dyn predicate)
    ; ("only_generated_files", Encoder.bool only_generated_files)
    ]

let encode { dir; predicate; only_generated_files } =
  let open Dune_lang.Encoder in
  record
    [ ("dir", Dpath.encode dir)
    ; ("predicate", Predicate.encode predicate)
    ; ("only_generated_files", bool only_generated_files)
    ]

let equal x y = compare x y = Eq

let hash { dir; predicate; only_generated_files } =
  Tuple.T3.hash Path.hash Predicate.hash Bool.hash
    (dir, predicate, only_generated_files)

let test t path = Predicate.test t.predicate (Path.basename path)
