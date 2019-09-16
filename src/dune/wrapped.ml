open! Stdune
open Dune_lang.Decoder

type t =
  | Simple of bool
  | Yes_with_transition of string

let decode =
  sum
    [ ("true", return (Simple true))
    ; ("false", return (Simple false))
    ; ( "transition"
      , Dune_lang.Syntax.since Stanza.syntax (1, 2)
        >>> let+ x = string in
            Yes_with_transition x )
    ]

let encode =
  let open Dune_lang.Encoder in
  function
  | Simple b -> bool b
  | Yes_with_transition m -> pair string string ("transition", m)

let to_bool = function
  | Simple b -> b
  | Yes_with_transition _ -> true

let to_dyn =
  let open Dyn.Encoder in
  function
  | Simple s -> constr "Simple" [ bool s ]
  | Yes_with_transition s -> constr "Yes_with_transition" [ string s ]
