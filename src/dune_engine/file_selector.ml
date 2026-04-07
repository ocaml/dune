open Import

type t =
  { (* CR-someday rgrinberg: this [dir] is ignored when evaluating the
       selector with [test]. It's better to just drop it completely and
       provide the directory explicitly when building or evaluating *)
    dir : Path.t
  ; predicate : Predicate_lang.Glob.t
  ; only_generated_files : bool
  }

let dir t = t.dir
let only_generated_files t = t.only_generated_files
let predicate t = t.predicate

let repr =
  Repr.record
    "file-selector"
    [ Repr.field "dir" Path.repr ~get:dir
    ; Repr.field "predicate" Predicate_lang.Glob.repr ~get:predicate
    ; Repr.field "only_generated_files" Repr.bool ~get:only_generated_files
    ]
;;

let digest t = Digest.repr repr t

let compare { dir; predicate; only_generated_files } t =
  let open Ordering.O in
  let= () = Path.compare dir t.dir in
  let= () = Predicate_lang.Glob.compare predicate t.predicate in
  Bool.compare only_generated_files t.only_generated_files
;;

let of_predicate_lang ~dir ?(only_generated_files = false) predicate =
  { dir; predicate; only_generated_files }
;;

let of_glob ~dir glob = of_predicate_lang ~dir (Predicate_lang.Glob.of_glob glob)
let to_dyn = Repr.to_dyn repr
let equal x y = compare x y = Eq

let hash { dir; predicate; only_generated_files } =
  Tuple.T3.hash
    Path.hash
    Predicate_lang.Glob.hash
    Bool.hash
    (dir, predicate, only_generated_files)
;;

let test t path =
  Predicate_lang.Glob.test
    t.predicate
    ~standard:Predicate_lang.false_
    (Path.basename path)
;;

let test_basename t ~basename =
  Predicate_lang.Glob.test t.predicate ~standard:Predicate_lang.false_ basename
;;
