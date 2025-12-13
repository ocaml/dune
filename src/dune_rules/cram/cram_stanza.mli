open Import

type applies_to =
  | Whole_subtree
  | Files_matching_in_this_dir of Predicate_lang.Glob.t

module Conflict_markers : sig
  type t =
    | Error
    | Ignore

  val to_string : t -> string
end

type t =
  { loc : Loc.t (* ; dir : Path.t *)
  ; applies_to : applies_to
  ; alias : Alias.Name.t option
  ; deps : Dep_conf.t Bindings.t option
  ; enabled_if : Blang.t
  ; locks : Locks.t
  ; conflict_markers : Conflict_markers.t option
  ; package : Package.t option
  ; runtest_alias : (Loc.t * bool) option
  ; timeout : (Loc.t * Time.Span.t) option
  ; setup_scripts : (Loc.t * string) list
  }

val decode : t Dune_lang.Decoder.t

include Stanza.S with type t := t
