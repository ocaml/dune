open Import

type applies_to =
  | Whole_subtree
  | Files_matching_in_this_dir of Predicate_lang.Glob.t

type t =
  { loc : Loc.t (* ; dir : Path.t *)
  ; applies_to : applies_to
  ; alias : Alias.Name.t option
  ; deps : Dep_conf.t Bindings.t option
  ; enabled_if : Blang.t
  ; locks : Locks.t
  ; package : Package.t option
  ; runtest_alias : (Loc.t * bool) option
  }

include Stanza.S with type t := t

val stanza : (string * Stanza.t list Dune_lang.Decoder.t) list
