open Import

type t =
  { dir : Path.t
  ; predicate : Filename.t Predicate_with_id.t
  ; only_generated_files : bool
  }

let dir t = t.dir

let predicate t = t.predicate

let only_generated_files t = t.only_generated_files

let compare { dir; predicate; only_generated_files } t =
  let open Ordering.O in
  let= () = Path.compare dir t.dir in
  let= () = Predicate_with_id.compare predicate t.predicate in
  Bool.compare only_generated_files t.only_generated_files

let create ~dir ?(only_generated_files = false) predicate =
  { dir; predicate; only_generated_files }

let to_dyn { dir; predicate; only_generated_files } =
  Dyn.Record
    [ ("dir", Path.to_dyn dir)
    ; ("predicate", Predicate_with_id.to_dyn predicate)
    ; ("only_generated_files", Bool only_generated_files)
    ]

let encode { dir; predicate; only_generated_files } =
  let open Dune_lang.Encoder in
  record
    [ ("dir", Dpath.encode dir)
    ; ("predicate", Predicate_with_id.encode predicate)
    ; ("only_generated_files", bool only_generated_files)
    ]

let equal x y = compare x y = Eq

let hash { dir; predicate; only_generated_files } =
  Tuple.T3.hash Path.hash Predicate_with_id.hash Bool.hash
    (dir, predicate, only_generated_files)

let test t path = Predicate_with_id.test t.predicate (Path.basename path)
