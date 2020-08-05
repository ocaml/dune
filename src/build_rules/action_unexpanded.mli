(** Actions as they are written in dune files. *)
open Stdune

include module type of struct
    (** The type definition exists in [Action_dune_lang] and not here to break
        cycles.*)
    include Action_dune_lang
  end
  (* We don't want to leak ugly aliases *)
  with type program := String_with_vars.t
   and type string := String_with_vars.t
   and type path := String_with_vars.t
   and type target := String_with_vars.t

val remove_locs : t -> t

(** Expand an action and return its target and dependencies.

    Expanding an action substitutes all [%{..}] forms, discovers dependencies
    and targets, and verifies invariants such as:

    - All the targets are in [targets_dir]
    - The [targets] mode is respected

    [foreign_flags] has to be passed because it depends on [Super_context].
    Fetching it directly would introduce a dependency cycle. *)
val expand :
     t
  -> loc:Loc.t
  -> dep_kind:Lib_deps_info.Kind.t
  -> targets_dir:Path.Build.t
  -> targets:Targets.Or_forbidden.t
  -> expander:Expander.t
  -> Path.t Bindings.t Build.t
  -> Action.t Build.With_targets.t

(** This module is exposed only for testing *)
module Infer : sig
  module Outcome : sig
    type t = private
      { deps : Path.Set.t
      ; targets : Path.Build.Set.t
      }
  end

  val infer : Action.t -> Outcome.t
end
