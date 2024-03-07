open Import

type t =
  { targets : String_with_vars.t Targets_spec.t
  ; deps : Dep_conf.t Bindings.t
  ; action : Loc.t * Dune_lang.Action.t
  ; mode : Rule.Mode.t
  ; patch_back_source_tree : bool
  ; locks : Locks.t
  ; loc : Loc.t
  ; enabled_if : Blang.t
  ; aliases : Alias.Name.t list
  ; package : Package.t option
  }

include Stanza.S with type t := t

val decode : t Dune_sexp.Decoder.t

type lex_or_yacc =
  { modules : string list
  ; mode : Rule.Mode.t
  ; enabled_if : Blang.t
  }

val ocamlyacc : lex_or_yacc Dune_lang.Decoder.t
val ocamllex : lex_or_yacc Dune_lang.Decoder.t
val ocamllex_to_rule : Loc.t -> lex_or_yacc -> t list
val ocamlyacc_to_rule : Loc.t -> lex_or_yacc -> t list
