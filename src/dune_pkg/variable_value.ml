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
type t = string

let true_ = "true"
let false_ = "false"
let string = Fun.id
let equal = String.equal
let compare = String.compare
let to_dyn = Dyn.string
let to_string = Fun.id
let decode = Decoder.string
let encode = Encoder.string
let to_opam_filter t = OpamTypes.FString t
let to_opam_variable_contents t = OpamTypes.S t

let sentinel_value_of_variable_name variable_name =
  let uppercase_replacing_dash_with_underscore =
    Package_variable_name.to_string variable_name
    |> String.uppercase
    |> String.replace_char ~from:'-' ~to_:'_'
  in
  string (String.concat ~sep:"" [ "__"; uppercase_replacing_dash_with_underscore ])
;;
