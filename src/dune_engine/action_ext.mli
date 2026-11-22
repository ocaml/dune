open Import
open Action.Ext

module Make (S : Spec_base) : sig
  val action : (Path.t, Path.Build.t) S.t -> Action.t
end
