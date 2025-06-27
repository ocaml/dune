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
  }

let default =
  { flags = Ordered_set_lang.Unexpanded.standard
  ; rocqdep_flags = Ordered_set_lang.Unexpanded.standard
  ; rocqdoc_flags = Ordered_set_lang.Unexpanded.standard
  }
;;

let flags t = t.flags
let rocqdep_flags t = t.rocqdep_flags
let rocqdoc_flags t = t.rocqdoc_flags

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
        in
        { flags; rocqdep_flags; rocqdoc_flags }))
;;

let equal { flags; rocqdep_flags; rocqdoc_flags } t =
  Ordered_set_lang.Unexpanded.equal flags t.flags
  && Ordered_set_lang.Unexpanded.equal rocqdep_flags t.rocqdep_flags
  && Ordered_set_lang.Unexpanded.equal rocqdoc_flags t.rocqdoc_flags
;;
