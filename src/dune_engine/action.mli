(** Actions as defined an executed by the build system.

    These executions correspond to primitives that the build system knows how to
    execute. These usually, but not necessarily correspond to actions written by
    the user in [Dune_lang.Action.t] *)

open Import
module Ext : module type of Action_intf.Ext
include module type of Action_intf.Exec

(** result of the lookup of a program, the path to it or information about the
    failure and possibly a hint how to fix it *)
module Prog : sig
  module Not_found : sig
    type t = private
      { context : Context_name.t
      ; program : Filename.t
      ; hint : string option
      ; loc : Loc.t option
      }

    val create
      :  ?hint:string
      -> context:Context_name.t
      -> program:Filename.t
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
  with type ext :=
    (module Ext.Instance with type target = Path.Build.t and type path = Path.t)

include
  Action_intf.Helpers
  with type program := Prog.t
  with type path := Path.t
  with type target := Path.Build.t
  with type string := string
  with type t := t

include Monoid with type t := t

module For_shell : sig
  module Program : sig
    type t =
      | Resolved of string
      | Unresolved of
          { context : Context_name.t
          ; program : Filename.t
          ; hint : string option
          }
  end

  include
    Action_intf.Ast
    with type program := Program.t
    with type path := string
    with type target := string
    with type string := string
    with type ext := Sexp.t
end

(** Convert the action to a format suitable for printing *)
val for_shell : t -> For_shell.t

(** Convert an action to the editable replay form. Relative paths are
    interpreted from [dir], and resolved local programs retain an explicit path
    instead of becoming PATH lookups. *)
val for_shell_replay : t -> dir:Path.t -> For_shell.t

val digest : Dune_digest.Manual.t -> t -> unit

(** Return the list of directories the action chdirs to *)
val chdirs : t -> Path.Build.Set.t

(** The empty action that does nothing. *)
val empty : t

(** Checks, if action contains a [Dynamic_run]. *)
val is_dynamic : t -> bool

(** Checks if the action contains a concurrent group. *)
val contains_concurrent : t -> bool

(** Return the name of the first action extension, if any. *)
val find_extension_name : t -> string option

(** Checks if executing the action may spawn a process. *)
val runs_process : t -> bool

(** Re-root all the paths in the action to their sandbox version *)
val sandbox : t -> Sandbox.t -> t

type is_useful =
  | Clearly_not
  | Maybe

(** Whether it makes sense to lookup the target in the distributed cache.

    Eg. there is no point in trying to fetch the result of a local file copy
    from the distributed cache, as we already have the file locally. *)
val is_useful_to_distribute : t -> is_useful

(** Whether it is useful to promote the rule to the cache.

    Eg. a file copy should be cached so we benefit from hardlink deduplication,
    but an action creating a symlink should not since the cache will reject it
    anyway. *)
val is_useful_to_memoize : t -> is_useful

module Full : sig
  type action := t

  (** A full action with its environment and list of locks *)
  type t = private
    { action : action
    ; env : Env.t
    ; locks : Path.t list
    ; can_go_in_shared_cache : bool
    ; sandbox : Sandbox_config.t
    ; corrections : Corrections.t option
    }

  val make
    :  ?env:Env.t (** default [Env.empty] *)
    -> ?locks:Path.t list (** default [[]] *)
    -> ?can_go_in_shared_cache:bool
         (** default [!Clflags.can_fo_in_shared_cache_default] *)
    -> ?sandbox:Sandbox_config.t (** default [Sandbox_config.default] *)
    -> ?corrections:Corrections.t (** default [Corrections.Ignore] *)
    -> action
    -> t

  val map : t -> f:(action -> action) -> t

  (** The various [add_xxx] functions merge the given value with existing field
      of the action. Put another way, [add_xxx x t] is the same as:

      {[
        combine t (make ~xxx:x (Progn []))
      ]} *)

  val add_env : Env.t -> t -> t
  val add_locks : Path.t list -> t -> t
  val add_sandbox : Sandbox_config.t -> t -> t
  val add_can_go_in_shared_cache : bool -> t -> t
  val add_corrections : Corrections.t -> t -> t

  include Monoid with type t := t
end
