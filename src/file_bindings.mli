type file =
  { src : String_with_vars.t
  ; dst : String_with_vars.t option
  }

type t = file list

val empty : t

val decode : t Stanza.Decoder.t
