open! Stdune

type t = { packages : Package_dependency.t list }

let encode { packages } =
  let open Dune_sexp.Encoder in
  record_fields [ field_l "depends" Package_dependency.encode packages ]
;;

let decode =
  let open Dune_sexp.Decoder in
  fields
  @@ let+ packages = field ~default:[] "depends" (repeat Package_dependency.decode) in
     { packages }
;;

let to_dyn { packages } =
  let open Dyn in
  list Package_dependency.to_dyn packages
;;
