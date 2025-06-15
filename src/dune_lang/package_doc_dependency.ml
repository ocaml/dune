open! Stdune

type t =
  { libraries : string list
  ; packages : Package_dependency.t list
  }

let encode { libraries; packages } =
  let open Dune_sexp.Encoder in
  record_fields
    [ field_l "packages" Package_dependency.encode packages
    ; field_l "libraries" string libraries
    ]
;;

let decode =
  let open Dune_sexp.Decoder in
  fields
  @@ let+ libraries = field ~default:[] "libraries" (repeat string)
     and+ packages = field ~default:[] "packages" (repeat Package_dependency.decode) in
     { packages; libraries }
;;

let to_dyn { libraries; packages } =
  let open Dyn in
  let x a = list string a in
  let y b = list Package_dependency.to_dyn b in
  pair x y (libraries, packages)
;;
