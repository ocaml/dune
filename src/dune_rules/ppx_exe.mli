open Import

(** Get the path to a ppx driver executable for a list of ppx rewriter libraries.
    This module provides the core ppx executable resolution logic without depending
    on the Expander module. *)

module Key : sig
  module Decoded : sig
    type t = private
      { pps : Lib_name.t list
      ; project_root : Path.Source.t option
      }
  end

  val decode : Digest.t -> Decoded.t
end

val ppx_exe_path : Build_context.t -> key:string -> Path.Build.t
val ppx_driver_exe : Context.t -> Lib.t list -> Path.Build.t Memo.t

(** Get the path to the ppx driver executable for a list of ppx libraries.
    The libraries must be provided with their locations for error reporting. *)
val get_ppx_exe
  :  Context.t
  -> scope:Scope.t
  -> (Loc.t * Lib_name.t) list
  -> Path.Build.t Resolve.Memo.t
