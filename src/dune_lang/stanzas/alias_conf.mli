open Import

type t =
  { name : Alias_name.t
  ; deps : Dep_conf.t Bindings.t
  ; action : (Loc.t * Action.t) option
  ; locks : Locks.t
  ; package : Package.t option
  ; enabled_if : Blang.t
  ; loc : Loc.t
  }

include Stanza.S with type t := t

val decode : t Decoder.t
