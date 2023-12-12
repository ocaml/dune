open Import

type t =
  { name : string
  ; libraries : (Loc.t * Lib_name.t) list
  ; loc : Loc.t
  ; pps : Preprocess.Without_instrumentation.t Preprocess.t
  }

include Stanza.S with type t := t

val decode : t Dune_sexp.Decoder.t
