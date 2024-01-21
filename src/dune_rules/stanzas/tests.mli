open Import

type t =
  { exes : Executables.t
  ; locks : Locks.t
  ; package : Package.t option
  ; deps : Dep_conf.t Bindings.t
  ; enabled_if : Blang.t
  ; build_if : Blang.t
  ; action : Dune_lang.Action.t option
  }

include Stanza.S with type t := t

val single : t Dune_lang.Decoder.t
val multi : t Dune_lang.Decoder.t
