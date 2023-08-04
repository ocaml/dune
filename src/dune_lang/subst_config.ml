open Dune_sexp.Decoder

(* Can be extended later if needed *)
type t =
  | Disabled
  | Enabled

let is_enabled = function
  | Enabled -> true
  | Disabled -> false
;;

let to_string = function
  | Disabled -> "disabled"
  | Enabled -> "enabled"
;;

let to_dyn conf = to_string conf |> Dyn.string
let encode t = Dune_sexp.Encoder.string (to_string t)
let decoder = enum [ "disabled", Disabled; "enabled", Enabled ]
let field ~since = field_o "subst" (Dune_sexp.Syntax.since Stanza.syntax since >>> decoder)

let of_config = function
  | None -> Enabled
  | Some conf -> conf
;;
