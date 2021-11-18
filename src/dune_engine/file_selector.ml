open Stdune

type t =
  { dir : Path.t
  ; predicate : string Predicate.t
  ; only_generated_files : bool
  }

let dir t = t.dir

let predicate t = t.predicate

let only_generated_files t = t.only_generated_files

let compare { dir; predicate; only_generated_files } t =
  let open Ordering.O in
  let= () = Path.compare dir t.dir in
  let= () = Predicate.compare predicate t.predicate in
  Bool.compare only_generated_files t.only_generated_files

let create ~dir ?(only_generated_files = false) predicate =
  { dir; predicate; only_generated_files }

let to_dyn { dir; predicate; only_generated_files } =
  Dyn.Record
    [ ("dir", Path.to_dyn dir)
    ; ("predicate", Predicate.to_dyn predicate)
    ; ("only_generated_files", Bool only_generated_files)
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
