open Dune_sexp.Decoder

(* Can be extended later if needed *)
type t = Toggle.t

let to_string = function
  | `Disabled -> "disabled"
  | `Enabled -> "enabled"
;;

let encode t = Dune_sexp.Encoder.string (to_string t)
let decoder = enum [ "disabled", `Disabled; "enabled", `Enabled ]

let field =
  field_o "subst" (Dune_sexp.Syntax.since Stanza.syntax (3, 0) >>> located decoder)
;;

let of_config = function
  | None -> `Enabled
  | Some conf -> conf
;;
