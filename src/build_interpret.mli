open! Import

module Rule : sig
  type t =
    { build   : (unit, Action.t) Build.t
    ; targets : Path.Set.t
    ; sandbox : bool
    }

  val make : ?sandbox:bool -> (unit, Action.t) Build.t -> t
end

val deps
  :  (_, _) Build.t
  -> all_targets_by_dir:Path.Set.t Path.Map.t Lazy.t
  -> Path.Set.t

val lib_deps
  :  (_, _) Build.t
  -> Build.lib_deps Path.Map.t

val targets
  :  (_, _) Build.t
  -> Path.Set.t
