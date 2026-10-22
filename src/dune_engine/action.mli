(** Actions as defined an executed by the build system.

    These executions correspond to primitives that the build system knows how to
    execute. These usually, but not necessarily correspond to actions written by
    the user in [Dune_lang.Action.t] *)

open Import
open Action_types
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
    val program : t -> Filename.t
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

val run : Prog.t -> string list -> t
val chdir : Path.t -> t -> t
val setenv : string -> string -> t -> t
val with_stdout_to : ?perm:File_perm.t -> Path.Build.t -> t -> t
val with_stderr_to : ?perm:File_perm.t -> Path.Build.t -> t -> t
val with_outputs_to : ?perm:File_perm.t -> Path.Build.t -> t -> t
val with_stdin_from : Path.t -> t -> t
val ignore_stdout : t -> t
val ignore_stderr : t -> t
val ignore_outputs : t -> t
val progn : t list -> t
val concurrent : t list -> t
val echo : string list -> t
val cat : Path.t list -> t
val copy : Path.t -> Path.Build.t -> t
val symlink : Path.t -> Path.Build.t -> t
val system : string -> t
val bash : string -> t
val write_file : ?perm:File_perm.t -> Path.Build.t -> string -> t
val rename : Path.Build.t -> Path.Build.t -> t
val remove_tree : Path.Build.t -> t
val mkdir : Path.Build.t -> t

val diff
  :  ?optional:bool
  -> ?mode:Diff.Mode.t
  -> ?directory_diffs:bool
  -> Path.t
  -> Path.Build.t
  -> t

include Monoid with type t := t

module For_shell : sig
  include
    Action_intf.Ast
    with type program := string
    with type path := string
    with type target := string
    with type string := string
    with type ext := Sexp.t
end

(** Convert the action to a format suitable for printing *)
val for_shell : t -> For_shell.t

val digest : Dune_digest.Manual.t -> t -> unit

(** Return the list of directories the action chdirs to *)
val chdirs : t -> Path.Build.Set.t

(** The empty action that does nothing. *)
val empty : t

(** Checks, if action contains a [Dynamic_run]. *)
val is_dynamic : t -> bool

(** Checks if executing the action may spawn a process. *)
val runs_process : t -> bool

(** Re-root all the paths in the action to their sandbox version *)
val sandbox : t -> Sandbox.t -> t

type is_useful =
  | Clearly_not
  | Maybe

(** Whether it is useful to promote the rule to the cache.

    Eg. a file copy should be cached so we benefit from hardlink deduplication,
    but an action creating a symlink should not since the cache will reject it
    anyway. *)
val is_useful_to_memoize : t -> is_useful

module Full : sig
  type action := t

  (** Properties that control how an action is executed. *)
  module Props : sig
    type t = private
      { env : Env.t
      ; locks : Path.Set.t
      ; can_go_in_shared_cache : bool
      ; can_use_sandbox_policy : bool
        (** Whether spawned processes can be subject to an additional sandbox policy. *)
      ; sandbox : Sandbox_config.t
      ; corrections : Corrections.t option
      ; job_slots : action option
        (** An action that prints the number of job slots that the action
            takes. This lets an action that runs its own parallel work, such as
            another build system, reserve the capacity that it uses. *)
      }
  end

  (** An action together with its execution properties. *)
  type t = private
    { action : action
    ; props : Props.t
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

  (** The various [add_xxx] functions merge the given value with the existing
      field of the action. *)

  val add_env : Env.t -> t -> t
  val add_locks : Path.t list -> t -> t
  val add_sandbox : Sandbox_config.t -> t -> t
  val add_can_go_in_shared_cache : bool -> t -> t
  val disable_sandbox_policy : t -> t
  val add_corrections : Corrections.t -> t -> t
  val add_job_slots : action -> t -> t

  include Monoid with type t := t
end
