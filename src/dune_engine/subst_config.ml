open! Stdune
open Import
open Dune_lang.Decoder

(* Can be extended later if needed *)
type t =
  | Disabled
  | Enabled

let to_dyn conf =
  let open Dyn.Encoder in
  match conf with
  | Disabled -> string "disabled"
  | Enabled -> string "enabled"

let decoder = enum [ ("disabled", Disabled); ("enabled", Enabled) ]

let field ~since =
  field_o "subst" (Dune_lang.Syntax.since Stanza.syntax since >>> decoder)

let of_config = function
  | None -> Enabled
  | Some conf -> conf
