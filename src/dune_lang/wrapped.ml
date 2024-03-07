open Stdune
open Dune_sexp.Decoder

type t =
  | Simple of bool
  | Yes_with_transition of string

let equal = Poly.equal

let decode =
  sum
    [ "true", return (Simple true)
    ; "false", return (Simple false)
    ; ( "transition"
      , Dune_sexp.Syntax.since Stanza.syntax (1, 2)
        >>> let+ x = string in
            Yes_with_transition x )
    ]
;;

let encode =
  let open Dune_sexp.Encoder in
  function
  | Simple b -> bool b
  | Yes_with_transition m -> pair string string ("transition", m)
;;

let to_bool = function
  | Simple b -> b
  | Yes_with_transition _ -> true
;;

let to_dyn =
  let open Dyn in
  function
  | Simple s -> variant "Simple" [ bool s ]
  | Yes_with_transition s -> variant "Yes_with_transition" [ string s ]
;;
