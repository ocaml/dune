open Stdune

type t =
  { dir : Path.t
  ; predicate : string Predicate.t
  ; include_source_file_copies : bool
  }

let dir t = t.dir

let predicate t = t.predicate

let include_source_file_copies t = t.include_source_file_copies

let compare x y =
  match Path.compare x.dir y.dir with
  | (Ordering.Lt | Gt) as a -> a
  | Eq -> Predicate.compare x.predicate y.predicate

let create ~dir ?(include_source_file_copies = true) predicate =
  { dir; predicate; include_source_file_copies }

let to_dyn { dir; predicate; include_source_file_copies } =
  let open Dyn in
  Record
    [ ("dir", Path.to_dyn dir)
    ; ("predicate", Predicate.to_dyn predicate)
    ; ("include_source_file_copies", Encoder.bool include_source_file_copies)
    ]

let encode { dir; predicate; include_source_file_copies } =
  let open Dune_lang.Encoder in
  record
    [ ("dir", Dpath.encode dir)
    ; ("predicate", Predicate.encode predicate)
    ; ("include_source_file_copies", bool include_source_file_copies)
    ]

let equal x y = compare x y = Eq

let hash { dir; predicate; include_source_file_copies } =
  Tuple.T3.hash Path.hash Predicate.hash Bool.hash
    (dir, predicate, include_source_file_copies)

let test t path = Predicate.test t.predicate (Path.basename path)
