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
    ; mode     : Jbuild.Rule.Mode.t
    ; locks    : Path.t list
    ; loc      : Loc.t option
    ; (** Directory where all the targets are produced *)
      dir      : Path.t
    ; package  : Package.Name.t option
    }

  val make
    :  ?sandbox:bool
    -> ?mode:Jbuild.Rule.Mode.t
    -> context:Context.t option
    -> ?locks:Path.t list
    -> ?loc:Loc.t
    -> ?package:Package.Name.t
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
  -> all_targets:(dir:Path.t -> Path.Set.t)
  -> file_tree:File_tree.t
  -> Static_deps.t

val lib_deps
  :  (_, _) Build.t
  -> Build.lib_deps

val targets
  :  (_, _) Build.t
  -> Target.t list
