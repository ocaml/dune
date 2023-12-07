open Import

type t =
  { package : Package.t
  ; name : Package.Name.t
  ; libraries : (Loc.t * Lib_name.t) list
  ; site : Loc.t * (Package.Name.t * Site.t)
  ; optional : bool
  }

val decode : t Dune_lang.Decoder.t

include Stanza.S with type t := t
