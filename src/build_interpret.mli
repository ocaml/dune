open! Import

module Target : sig
  type t =
    | Normal of Path.t
    | Vfile : _ Build.Vspec.t -> t

  val path : t -> Path.t
  val paths : t list -> Path.Set.t
end

module Rule : sig
  type t =
    { context  : Context.t option
    ; build    : (unit, Action.t) Build.t
    ; targets  : Target.t list
    ; sandbox  : bool
    ; fallback : Jbuild.Rule.Fallback.t
    ; locks    : Path.t list
    ; loc      : Loc.t option
    }

  val make
    :  ?sandbox:bool
    -> ?fallback:Jbuild.Rule.Fallback.t
    -> ?context:Context.t
    -> ?locks:Path.t list
    -> ?loc:Loc.t
    -> (unit, Action.t) Build.t
    -> t
end

module Static_deps : sig
  type t =
    { rule_deps   : Path.Set.t
    ; action_deps : Path.Set.t
    }
end

(* must be called first *)
val static_deps
  :  (_, _) Build.t
  -> all_targets_by_dir:Path.Set.t Path.Map.t Lazy.t
  -> Static_deps.t

val lib_deps
  :  (_, _) Build.t
  -> Build.lib_deps Path.Map.t

val targets
  :  (_, _) Build.t
  -> Target.t list
