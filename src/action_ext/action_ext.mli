open Stdune

module Make (S : Dune_engine.Action.Ext.Spec) : sig
  val action : (Path.t, Path.Build.t) S.t -> Dune_engine.Action.t
end
