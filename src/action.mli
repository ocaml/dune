open! Stdune
open! Import

module Outputs : module type of struct include Action_intf.Outputs end
module Diff_mode : module type of struct include Action_intf.Diff_mode end

(** result of the lookup of a program, the path to it or information about the
    failure and possibly a hint how to fix it *)
module Prog : sig
  module Not_found : sig
    type t =
      { context : string
      ; program : string
      ; hint    : string option
      ; loc     : Loc.t option
      }

    val raise : t -> _
  end

  type t = (Path.t, Not_found.t) result
end

include Action_intf.Ast
  with type program := Prog.t
  with type path    := Path.t
  with type string  := string

include Action_intf.Helpers
  with type program := Prog.t
  with type path    := Path.t
  with type string  := string
  with type t       := t

val dparse : t Dsexp.Of_sexp.t

module For_shell : sig
  include Action_intf.Ast
    with type program := string
    with type path    := string
    with type string  := string

  val dgen : t Dsexp.To_sexp.t
end

(** Convert the action to a format suitable for printing *)
val for_shell : t -> For_shell.t

(** Return the list of directories the action chdirs to *)
val chdirs : t -> Path.Set.t

(** Ast where programs are not yet looked up in the PATH *)
module Unresolved : sig
  type action = t

  module Program : sig
    type t =
      | This   of Path.t
      | Search of Loc.t option * string
  end

  include Action_intf.Ast
    with type program := Program.t
    with type path    := Path.t
    with type string  := string

  val resolve : t -> f:(Loc.t option -> string -> Path.t) -> action
end with type action := t

module Unexpanded : sig
  include Action_intf.Ast
    with type program := String_with_vars.t
    with type path    := String_with_vars.t
    with type string  := String_with_vars.t

  include Dsexp.Sexpable with type t := t

  module Partial : sig
    include Action_intf.Ast
      with type program = (Unresolved.Program.t, String_with_vars.t) either
      with type path    = (Path.t              , String_with_vars.t) either
      with type string  = (string              , String_with_vars.t) either

    val expand
      :  t
      -> dir:Path.t
      -> map_exe:(Path.t -> Path.t)
      -> f:(Value.t list option String_with_vars.expander)
      -> Unresolved.t
  end

  val partial_expand
    :  t
    -> dir:Path.t
    -> map_exe:(Path.t -> Path.t)
    -> f:(Value.t list option String_with_vars.expander)
    -> Partial.t

  val remove_locs : t -> t
end

(** Infer dependencies and targets.

    This currently doesn't support well (rename ...) and (remove-tree ...). However these
    are not exposed in the DSL.
*)
module Infer : sig
  module Outcome : sig
    type t =
      { deps    : Path.Set.t
      ; targets : Path.Set.t
      }
  end

  val infer : t -> Outcome.t

  (** If [all_targets] is [true] and a target cannot be determined statically, fail *)
  val partial : all_targets:bool -> Unexpanded.Partial.t -> Outcome.t

  (** Return the list of targets of an unexpanded action. *)
  val unexpanded_targets : Unexpanded.t -> String_with_vars.t list
end

(** Return a sandboxed version of an action *)
val sandbox
  :  t
  -> sandboxed:(Path.t -> Path.t)
  -> deps:Deps.t
  -> targets:Path.t list
  -> t
