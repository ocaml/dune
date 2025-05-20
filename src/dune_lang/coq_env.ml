open Import
open Decoder

type t =
  { flags : Ordered_set_lang.Unexpanded.t
  ; coqdep_flags : Ordered_set_lang.Unexpanded.t
  ; coqdoc_flags : Ordered_set_lang.Unexpanded.t
  }

let default =
  { flags = Ordered_set_lang.Unexpanded.standard
  ; coqdep_flags = Ordered_set_lang.Unexpanded.standard
  ; coqdoc_flags = Ordered_set_lang.Unexpanded.standard
  }
;;

let flags t = t.flags
let coqdep_flags t = t.coqdep_flags
let coqdoc_flags t = t.coqdoc_flags

let decode =
  field
    "coq"
    ~default
    (fields
       (let+ flags =
          Ordered_set_lang.Unexpanded.field
            "flags"
            ~check:(Syntax.since Stanza.syntax (2, 7))
        and+ coqdep_flags =
          Ordered_set_lang.Unexpanded.field
            "coqdep_flags"
            ~check:(Syntax.since Stanza.syntax (3, 17))
        and+ coqdoc_flags =
          Ordered_set_lang.Unexpanded.field
            "coqdoc_flags"
            ~check:(Syntax.since Stanza.syntax (3, 13))
        in
        { flags; coqdep_flags; coqdoc_flags }))
;;

let equal { flags; coqdep_flags; coqdoc_flags } t =
  Ordered_set_lang.Unexpanded.equal flags t.flags
  && Ordered_set_lang.Unexpanded.equal coqdep_flags t.coqdep_flags
  && Ordered_set_lang.Unexpanded.equal coqdoc_flags t.coqdoc_flags
;;
