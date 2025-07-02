open! Stdune

(* Documentation urls are part of [Package_info.t], however they are parsed here
   since they also use the [documentation] field name.

 Hence, the decode and encode function has an additional url as
 output/input. This url comes/goes to [Package_info.t] *)

type t = { packages : Package_dependency.t list }

let encode ~url { packages } =
  let open Dune_sexp.Encoder in
  match packages, url with
  | [], Some url -> [ string url ]
  | _ ->
    record_fields
      [ field_l "depends" Package_dependency.encode packages; field_o "url" string url ]
;;

let decode =
  let open Dune_sexp.Decoder in
  let+ res =
    either string
    @@ fields
         (let+ packages = field ~default:[] "depends" (repeat Package_dependency.decode)
          and+ url = field_o "url" string in
          url, { packages })
  in
  match res with
  | Left s -> Some s, { packages = [] }
  | Right res -> res
;;

let to_dyn { packages } =
  let open Dyn in
  record [ "packages", list Package_dependency.to_dyn packages ]
;;
