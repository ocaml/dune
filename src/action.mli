open! Import

module Var_expansion : sig
  module Concat_or_split : sig
    type t =
      | Concat (* default *)
      | Split  (* ${!...} *)
  end

  type t =
    | Paths   of Path.t list * Concat_or_split.t
    | Strings of string list * Concat_or_split.t
end

module Outputs : module type of struct include Action_intf.Outputs end

module Program : sig
  type t =
    | This      of Path.t
    | Not_found of string
end

include Action_intf.Ast
  with type program := Program.t
  with type path    := Path.t
  with type string  := string

val t : t Sexp.Of_sexp.t
val sexp_of_t : t Sexp.To_sexp.t

(** Return the list of files under an [Update_file] *)
val updated_files : t -> Path.Set.t

(** Return the list of directories the action chdirs to *)
val chdirs : t -> Path.Set.t

module Unexpanded : sig
  type action = t

  include Action_intf.Ast
    with type program := String_with_vars.t
    with type path    := String_with_vars.t
    with type string  := String_with_vars.t

  val t : t Sexp.Of_sexp.t
  val sexp_of_t : t Sexp.To_sexp.t

  module Partial : sig
    include Action_intf.Ast
      with type program = (Program.t, String_with_vars.t) either
      with type path    = (Path.t   , String_with_vars.t) either
      with type string  = (string   , String_with_vars.t) either

    val expand
      :  Context.t
      -> Path.t
      -> t
      -> f:(Loc.t -> String.t -> Var_expansion.t option)
      -> action
  end

  val partial_expand
    :  Context.t
    -> Path.t
    -> t
    -> f:(Loc.t -> string -> Var_expansion.t option)
    -> Partial.t
end with type action := t

val exec : targets:Path.Set.t -> ?context:Context.t -> t -> unit Future.t

(* Return a sandboxed version of an action *)
val sandbox
  :  t
  -> sandboxed:(Path.t -> Path.t)
  -> deps:Path.t list
  -> targets:Path.t list
  -> t

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
end
