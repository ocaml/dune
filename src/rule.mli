(** Representation of rules *)

open! Stdune
open! Import

module Info : sig
  type t =
    | From_dune_file of Loc.t
    | Internal
    | Source_file_copy

  val of_loc_opt : Loc.t option -> t
end

type t =
  { context  : Context.t option
  ; env      : Env.t option
  ; build    : (unit, Action.t) Build.t
  ; targets  : Path.Set.t
  ; sandbox  : bool
  ; mode     : Dune_file.Rule.Mode.t
  ; locks    : Path.t list
  ; info     : Info.t
  ; (** Directory where all the targets are produced *)
    dir      : Path.t
  }

val make
  :  ?sandbox:bool
  -> ?mode:Dune_file.Rule.Mode.t
  -> context:Context.t option
  -> env:Env.t option
  -> ?locks:Path.t list
  -> ?info:Info.t
  -> (unit, Action.t) Build.t
  -> t
