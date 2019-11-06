(** Workspaces definitions *)

open! Stdune
open! Import

module Context : sig
  module Target : sig
    type t =
      | Native
      | Named of Context_name.t
  end

  module Common : sig
    type t =
      { loc : Loc.t
      ; profile : Profile.t
      ; targets : Target.t list
      ; env : Dune_env.Stanza.t
      ; toolchain : Context_name.t option
      ; name : Context_name.t
      ; host_context : Context_name.t option
      ; paths : (string * Ordered_set_lang.t) list
      ; fdo_target_exe : Path.t option
            (* By default Dune builds and installs [dll*.so] archives of
               foreign stubs. This is disabled when [build_foreign_dll_files]
               is set to [false], in which case Dune will link in all stub
               archives statically into the runtime system. *)
      ; build_foreign_dll_files : bool
      }
  end

  module Opam : sig
    type t =
      { base : Common.t
      ; switch : Context_name.t
      ; root : string option
      ; merlin : bool
      }
  end

  module Default : sig
    type t = Common.t
  end

  type t =
    | Default of Default.t
    | Opam of Opam.t

  val loc : t -> Loc.t

  val name : t -> Context_name.t

  val env : t -> Dune_env.Stanza.t

  val host_context : t -> Context_name.t option
end

(** Representation of a workspace. The list of context is topologically sorted,
    i.e. a context always comes before the contexts where it is used as host
    context. *)
type t = private
  { merlin_context : Context_name.t option
  ; contexts : Context.t list
  ; env : Dune_env.Stanza.t
  }

val load : ?x:Context_name.t -> ?profile:Profile.t -> Path.t -> t

(** Default name of workspace files *)
val filename : string

(** Default configuration *)
val default : ?x:Context_name.t -> ?profile:Profile.t -> unit -> t
