(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)

open Import
open Decoder

type t =
  { flags : Ordered_set_lang.Unexpanded.t
  ; rocqdep_flags : Ordered_set_lang.Unexpanded.t
  ; rocqdoc_flags : Ordered_set_lang.Unexpanded.t
  ; rocqdoc_header : String_with_vars.t option
  ; rocqdoc_footer : String_with_vars.t option
  }

let default =
  { flags = Ordered_set_lang.Unexpanded.standard
  ; rocqdep_flags = Ordered_set_lang.Unexpanded.standard
  ; rocqdoc_flags = Ordered_set_lang.Unexpanded.standard
  ; rocqdoc_header = None
  ; rocqdoc_footer = None
  }
;;

let flags t = t.flags
let rocqdep_flags t = t.rocqdep_flags
let rocqdoc_flags t = t.rocqdoc_flags
let rocqdoc_header t = t.rocqdoc_header
let rocqdoc_footer t = t.rocqdoc_footer

let decode =
  field
    "rocq"
    ~default
    (fields
       (let+ flags =
          Ordered_set_lang.Unexpanded.field
            "flags"
            ~check:(Syntax.since Stanza.syntax (2, 7))
        and+ rocqdep_flags =
          Ordered_set_lang.Unexpanded.field
            "rocqdep_flags"
            ~check:(Syntax.since Stanza.syntax (3, 17))
        and+ rocqdoc_flags =
          Ordered_set_lang.Unexpanded.field
            "rocqdoc_flags"
            ~check:(Syntax.since Stanza.syntax (3, 13))
        and+ rocqdoc_header =
          field_o
            "rocqdoc_header"
            (Syntax.since Stanza.syntax (3, 21) >>> String_with_vars.decode)
        and+ rocqdoc_footer =
          field_o
            "rocqdoc_footer"
            (Syntax.since Stanza.syntax (3, 21) >>> String_with_vars.decode)
        in
        { flags; rocqdep_flags; rocqdoc_flags; rocqdoc_header; rocqdoc_footer }))
;;

let equal { flags; rocqdep_flags; rocqdoc_flags; rocqdoc_header; rocqdoc_footer } t =
  Ordered_set_lang.Unexpanded.equal flags t.flags
  && Ordered_set_lang.Unexpanded.equal rocqdep_flags t.rocqdep_flags
  && Ordered_set_lang.Unexpanded.equal rocqdoc_flags t.rocqdoc_flags
  && Option.equal String_with_vars.equal rocqdoc_header t.rocqdoc_header
  && Option.equal String_with_vars.equal rocqdoc_footer t.rocqdoc_footer
;;
