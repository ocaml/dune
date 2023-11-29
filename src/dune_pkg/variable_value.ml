open! Import

(* Currently only string values can be represented. Opam silently converts
   between strings to booleans when appropriate so this doesn't prevent boolean
   values from being used in formulae. Since string values in dune syntax don't
   require quotes, representing boolean literals requires extra syntax which we
   don't yet have. When such a syntax exists we can add boolean values to this
   type.

   We also don't yet support setting variables to lists of strings which would
   be needed for full compatibility with the [OpamTypes.variable_contents]
   type.
*)
type t = String of string

let string string = String string

let equal a b =
  match a, b with
  | String a, String b -> String.equal a b
;;

let to_dyn = function
  | String string -> Dyn.variant "String" [ Dyn.string string ]
;;

let to_string = function
  | String string -> string
;;

let decode =
  let open Decoder in
  let+ string = string in
  String string
;;

let encode = function
  | String string -> Encoder.string string
;;

let to_opam_filter = function
  | String string -> OpamTypes.FString string
;;
