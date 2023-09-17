open! Stdune
open Dune_sexp
include Dune_glob.V1

let to_dyn t = Dyn.variant "Glob" [ Dyn.string (to_string t) ]
let compare x y = String.compare (to_string x) (to_string y)
let equal x y = String.equal (to_string x) (to_string y)
let hash t = String.hash (to_string t)

let encode t =
  let open Encoder in
  string (to_string t)
;;

let decode =
  let open Decoder in
  plain_string (fun ~loc str -> of_string_exn loc str)
;;

let filter t = List.filter ~f:(test t)
let to_predicate t = Predicate.create (test t)
