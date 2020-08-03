open! Stdune
open! Import

module Outputs : sig
  include module type of Action_intf.Outputs

  val to_string : t -> string
end

module Inputs : module type of struct
  include Action_intf.Inputs
end

(** result of the lookup of a program, the path to it or information about the
    failure and possibly a hint how to fix it *)
module Prog : sig
  module Not_found : sig
    type t = private
      { context : Context_name.t
      ; program : string
      ; hint : string option
      ; loc : Loc.t option
      }

    val create :
         ?hint:string
      -> context:Context_name.t
      -> program:string
      -> loc:Loc.t option
      -> unit
      -> t

    val raise : t -> _
  end

  type t = (Path.t, Not_found.t) result

  val to_dyn : t -> Dyn.t

  val ok_exn : t -> Path.t
end

include
  Action_intf.Ast
    with type program := Prog.t
    with type path := Path.t
    with type target := Path.Build.t
    with type string := string

module Ast :
  Action_intf.Ast
    with type program = Prog.t
    with type path = Path.t
    with type target = Path.Build.t
    with type string = string
    with type t = t

include
  Action_intf.Helpers
    with type program := Prog.t
    with type path := Path.t
    with type target := Path.Build.t
    with type string := string
    with type t := t

val decode : t Dune_lang.Decoder.t

module For_shell : sig
  include
    Action_intf.Ast
      with type program := string
      with type path := string
      with type target := string
      with type string := string

  val encode : t Dune_lang.Encoder.t
end

(** Convert the action to a format suitable for printing *)
val for_shell : t -> For_shell.t

(** Return the list of directories the action chdirs to *)
val chdirs : t -> Path.Set.t

(** The empty action that does nothing. *)
val empty : t

(** Checks, if action contains a [Dynamic_run]. *)
val is_dynamic : t -> bool

(** Ast where programs are not yet looked up in the PATH *)
module Unresolved : sig
  type action = t

  module Program : sig
    type t =
      | This of Path.t
      | Search of Loc.t option * string

    val of_string : dir:Path.t -> loc:Loc.t option -> string -> t
  end

  include
    Action_intf.Ast
      with type program := Program.t
      with type path := Path.t
      with type target := Path.Build.t
      with type string := string

  val resolve : t -> f:(Loc.t option -> string -> Path.t) -> action
end
with type action := t

(** Return a sandboxed version of an action. It takes care of preparing deps in
    the sandbox, but it does not copy the targets back out. It's the
    responsibility of the caller to do that. *)
val sandbox :
     t
  -> sandboxed:(Path.Build.t -> Path.Build.t)
  -> mode:Sandbox_mode.some
  -> deps:Dep.Set.t
  -> eval_pred:Dep.eval_pred
  -> t

type is_useful =
  | Clearly_not
  | Maybe

(** Whether it makes sense to run the action inside a sandbox because it could
    have harmful side effects, to ensure it only consumes declared dependencies
    and it does not produce undeclared targets.

    Eg. it is maybe useful to sandbox an arbitrary shell command, but not a
    directory creation. *)
val is_useful_to_sandbox : t -> is_useful

(** Whether it makes sense to lookup the target in the distributed cache.

    Eg. there is no point in trying to fetch the result of a local file copy
    from the distributed cache, as we already have the file locally. *)
val is_useful_to_distribute : t -> is_useful

(** Whether it is useful to promote the rule to the cache.

    Eg. a file copy should be cached so we benefit from hardlink deduplication,
    but an action creating a symlink should not since the cache will reject it
    anyway. *)
val is_useful_to_memoize : t -> is_useful
