open Import
open Dune_lang.Decoder

type t =
  { flags : Ordered_set_lang.Unexpanded.t
  ; coqdep_flags : Ordered_set_lang.Unexpanded.t
  ; coqdoc_flags : Ordered_set_lang.Unexpanded.t
  ; coqdoc_header : String_with_vars.t option
  ; coqdoc_footer : String_with_vars.t option
  }

let default =
  { flags = Ordered_set_lang.Unexpanded.standard
  ; coqdep_flags = Ordered_set_lang.Unexpanded.standard
  ; coqdoc_flags = Ordered_set_lang.Unexpanded.standard
  ; coqdoc_header = None
  ; coqdoc_footer = None
  }
;;

let flags t = t.flags
let coqdep_flags t = t.coqdep_flags
let coqdoc_flags t = t.coqdoc_flags
let coqdoc_header t = t.coqdoc_header
let coqdoc_footer t = t.coqdoc_footer

let decode =
  field
    "coq"
    ~default
    (fields
       (let+ flags =
          Ordered_set_lang.Unexpanded.field
            "flags"
            ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 7))
        and+ coqdep_flags =
          Ordered_set_lang.Unexpanded.field
            "coqdep_flags"
            ~check:(Dune_lang.Syntax.since Stanza.syntax (3, 17))
        and+ coqdoc_flags =
          Ordered_set_lang.Unexpanded.field
            "coqdoc_flags"
            ~check:(Dune_lang.Syntax.since Stanza.syntax (3, 13))
        and+ coqdoc_header =
          field_o
            "coqdoc_header"
            (Dune_lang.Syntax.since Stanza.syntax (3, 17) >>> String_with_vars.decode)
        and+ coqdoc_footer =
          field_o
            "coqdoc_footer"
            (Dune_lang.Syntax.since Stanza.syntax (3, 17) >>> String_with_vars.decode)
        in
        { flags; coqdep_flags; coqdoc_flags; coqdoc_header; coqdoc_footer }))
;;

let equal { flags; coqdep_flags; coqdoc_flags; coqdoc_header; coqdoc_footer } t =
  Ordered_set_lang.Unexpanded.equal flags t.flags
  && Ordered_set_lang.Unexpanded.equal coqdep_flags t.coqdep_flags
  && Ordered_set_lang.Unexpanded.equal coqdoc_flags t.coqdoc_flags
  && Option.equal String_with_vars.equal coqdoc_header t.coqdoc_header
  && Option.equal String_with_vars.equal coqdoc_footer t.coqdoc_footer
;;
