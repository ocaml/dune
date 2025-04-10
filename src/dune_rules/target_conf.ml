[@@@warning "-33"]

open Stdune
open Dune_lang
module Name = String

type t = String_with_vars.t Targets_spec.Named_target.t Bindings.t

let decode =
  let open Dune_sexp.Decoder in
  Bindings.decode (Targets_spec.decode_target ~allow_directory_targets:true)
;;

let empty = Bindings.empty
