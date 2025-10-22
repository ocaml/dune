open Import

type t =
  { targets : String_with_vars.t Targets_spec.t
  ; deps : Dep_conf.t Bindings.t
  ; action : Loc.t * Dune_lang.Action.t
  ; mode : Rule.Mode.t
  ; locks : Locks.t
  ; loc : Loc.t
  ; enabled_if : Blang.t
  ; aliases : Alias.Name.t list
  ; package : Package.t option
  }

include Stanza.S with type t := t

val decode : t Dune_sexp.Decoder.t

type lex_or_yacc

val ocamlyacc : t list Dune_lang.Decoder.t
val ocamllex : t list Dune_lang.Decoder.t
