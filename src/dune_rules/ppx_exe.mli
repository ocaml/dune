open Import

(** Get the path to a ppx driver executable for a list of ppx rewriter libraries.
    This module provides the core ppx executable resolution logic without depending
    on the Expander module. *)

(** Get the path to the ppx driver executable for a list of ppx libraries.
    The libraries must be provided with their locations for error reporting. *)
val get_ppx_exe
  :  Context.t
  -> scope:Scope.t
  -> (Loc.t * Lib_name.t) list
  -> Path.Build.t Resolve.Memo.t
