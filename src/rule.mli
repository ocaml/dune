(** Representation of rules *)

open! Stdune
open! Import

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
