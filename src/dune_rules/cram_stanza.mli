open! Dune_engine
open Import

type applies_to =
  | Whole_subtree
  | Files_matching_in_this_dir of Predicate_lang.Glob.t

type t =
  { loc : Loc.t
  ; applies_to : applies_to
  ; alias : Alias.Name.t option
  ; deps : Dep_conf.t Bindings.t option
  ; enabled_if : Blang.t
  }

val decode : t Dune_lang.Decoder.t
