(** Workspaces definitions *)

open! Import

module Context : sig
  module Target : sig
    type t =
      | Native
      | Named of string
  end
  module Base : sig
    type t =
      { loc     : Loc.t
      ; profile : string
      ; targets : Target.t list
      }
  end
  module Opam : sig
    type t =
      { base    : Base.t
      ; name    : string
      ; switch  : string
      ; root    : string option
      ; merlin  : bool
      }
  end

  module Default : sig
    type t = Base.t
  end

  type t = Default of Default.t | Opam of Opam.t

  val name : t -> string
end

type t =
  { merlin_context : string option
  ; contexts       : Context.t list
  }

val load : ?x:string -> ?profile:string -> Path.t -> t

(** Default name of workspace files *)
val filename : string

(** Default configuration *)
val default : ?x:string -> ?profile:string -> unit -> t
