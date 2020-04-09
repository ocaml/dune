open Stdune

(* This module is to be used in Dune_file. It should not introduce any
   dependencies unless they're already dependencies of Dune_file *)
include
  Action_intf.Ast
    with type program := String_with_vars.t
     and type string := String_with_vars.t
     and type path := String_with_vars.t
     and type target := String_with_vars.t

include Dune_lang.Conv.S with type t := t

(** Raises User_error on invalid action. *)
val validate : loc:Loc.t -> t -> unit

include
  Action_intf.Helpers
    with type t := t
     and type program = String_with_vars.t
     and type string = String_with_vars.t
     and type path = String_with_vars.t
     and type target = String_with_vars.t

val compare_no_locs : t -> t -> Ordering.t

val to_dyn : t -> Dyn.t

val remove_locs : t -> t
