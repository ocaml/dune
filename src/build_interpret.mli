open! Stdune
open! Import

module Rule : sig
  type t =
    { context  : Context.t option
    ; env      : Env.t option
    ; build    : (unit, Action.t) Build.t
    ; targets  : Path.Set.t
    ; sandbox  : bool
    ; mode     : Dune_file.Rule.Mode.t
    ; locks    : Path.t list
    ; loc      : Loc.t option
    ; (** Directory where all the targets are produced *)
      dir      : Path.t
    }

  val make
    :  ?sandbox:bool
    -> ?mode:Dune_file.Rule.Mode.t
    -> context:Context.t option
    -> env:Env.t option
    -> ?locks:Path.t list
    -> ?loc:Loc.t
    -> (unit, Action.t) Build.t
    -> t
end

(** Must be called first before [lib_deps] and [targets] as it updates
    some of the internal references in the build arrow. *)
val static_deps
  :  (_, _) Build.t
  -> all_targets:(dir:Path.t -> Path.Set.t)
  -> Static_deps.t

val lib_deps
  :  (_, _) Build.t
  -> Lib_deps_info.t

val targets
  :  (_, _) Build.t
  -> Path.Set.t
