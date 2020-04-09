open Stdune

include module type of struct
  include Action_dune_lang
end

val remove_locs : t -> t

val expand :
     t
  -> map_exe:(Path.t -> Path.t)
  -> dep_kind:Lib_deps_info.Kind.t
  -> deps_written_by_user:Path.t Bindings.t Build.t
  -> targets:Targets.Or_forbidden.t
  -> expander:Expander.t
  -> foreign_flags:
       (   dir:Path.Build.t
        -> Stdune.String.t list Build.t Foreign.Language.Dict.t)
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
