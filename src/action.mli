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

(** Infer dependencies and targets *)
module Infer : sig
  module Outcome : sig
    type t =
      { deps    : Path.Set.t
      ; targets : Path.Set.t
      }
  end

  val infer : t -> Outcome.t
end

module Unexpanded : sig
  type action = t

  include Action_intf.Ast
    with type program := String_with_vars.t
    with type path    := String_with_vars.t
    with type string  := String_with_vars.t

  val t : t Sexp.Of_sexp.t
  val sexp_of_t : t Sexp.To_sexp.t
  val fold_vars : t -> init:'a -> f:('a -> Loc.t -> string -> 'a) -> 'a
  val expand : Context.t -> Path.t -> t -> f:(string -> Var_expansion.t option) -> action
end with type action := t

val exec : targets:Path.Set.t -> ?context:Context.t -> t -> unit Future.t

(* Return a sandboxed version of an action *)
val sandbox
  :  t
  -> sandboxed:(Path.t -> Path.t)
  -> deps:Path.t list
  -> targets:Path.t list
  -> t
