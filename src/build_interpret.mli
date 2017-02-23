open Import

module Target : sig
  type t =
    | Normal of Path.t
    | Vfile : _ Build.Vspec.t -> t

  val path : t -> Path.t
  val paths : t list -> Path.Set.t
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
  -> Target.t list
