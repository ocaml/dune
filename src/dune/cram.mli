open Import

type test =
  | File of Path.Source.t
  | Dir of
      { file : Path.Source.t
      ; dir : Path.Source.t
      }

val dyn_of_test : test -> Dyn.t

val name : test -> string

val script : test -> Path.Source.t

val is_cram_suffix : string -> bool

module Stanza : sig
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
end
