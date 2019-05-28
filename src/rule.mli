(** Representation of rules *)

open! Stdune
open! Import

module Info : sig
  type t =
    | From_dune_file of Loc.t
    | Internal
    | Source_file_copy

  val of_loc_opt : Loc.t option -> t

  val loc : t -> Loc.t option
end

type t =
  { context  : Context.t option
  ; env      : Env.t option
  ; build    : (unit, Action.t) Build.t
  ; targets  : Path.Build.Set.t
  ; sandbox  : Sandbox_config.t
  ; mode     : Dune_file.Rule.Mode.t
  ; locks    : Path.t list
  ; info     : Info.t
  ; (** Directory where all the targets are produced *)
    dir      : Path.Build.t
  }

val make
  :  ?sandbox:Sandbox_config.t
  -> ?mode:Dune_file.Rule.Mode.t
  -> context:Context.t option
  -> env:Env.t option
  -> ?locks:Path.t list
  -> ?info:Info.t
  -> (unit, Action.t) Build.t
  -> t
