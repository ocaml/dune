open! Stdune
open Import
include Dune_glob.V1

let of_string = `shadowed

let _ = of_string

let compare x y = String.compare (to_string x) (to_string y)

let equal x y = String.equal (to_string x) (to_string y)

let hash t = String.hash (to_string t)

let to_dyn t = Dyn.Encoder.string (to_string t)

let of_string_exn loc repr =
  match of_string_result repr with
  | Error (_, msg) -> User_error.raise ~loc [ Pp.textf "invalid glob: :%s" msg ]
  | Ok t -> t

let encode t =
  let open Dune_lang.Encoder in
  string (to_string t)

let decode =
  let open Dune_lang.Decoder in
  plain_string (fun ~loc str -> of_string_exn loc str)

let filter t = List.filter ~f:(test t)

let to_pred t =
  let id =
    lazy
      (let open Dyn.Encoder in
      constr "Glob" [ string (to_string t) ])
  in
  Predicate.create ~id ~f:(test t)
