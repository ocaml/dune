open Import

type t =
  { name : Alias.Name.t
  ; deps : Dep_conf.t Bindings.t
  ; action : (Loc.t * Dune_lang.Action.t) option
  ; locks : Locks.t
  ; package : Package.t option
  ; enabled_if : Blang.t
  ; loc : Loc.t
  }

include Stanza.S with type t := t

val decode : t Dune_lang.Decoder.t
