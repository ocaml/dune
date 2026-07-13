open Import

(** Preparation of a rule action for an interactive [dune shell] session. *)

type t =
  { dir : Path.t
  ; shell_env : Env.t
    (** The entry environment after common leading action wrappers and the
        final Dune temporary-directory injection. *)
  ; replay_env : Env.t
    (** The base action environment. Scoped wrappers remain in [action]; the
        Dune temporary-directory injection is already present. *)
  ; sandbox_dir : Path.Build.t option
  ; sandbox_mode : Sandbox_mode.some option
  ; action : Action.t
  ; targets : Targets.Validated.t
  ; rule_digest : Digest.t
  }

type direct_process =
  { program : Path.t
  ; args : string list
  ; dir : Path.t
  ; env : Env.t
  }

(** If the prepared action is a single process invocation modulo its leading
    wrappers, return that direct process: its program, arguments, and the
    working directory and environment the action interpreter would run it
    with. *)
val direct_process : t -> direct_process option

(** Build and evaluate all prerequisites of [rule], prepare the rule's action
    in its normally selected execution location, and run [f] instead of the
    action. Existing declared targets are removed as they are before ordinary
    action execution. The selected action is not executed and its outputs are
    not extracted, cached, or promoted. The execution location and the rule's
    action locks remain owned by the build system until [f] returns. *)
val with_ : Rule.t -> f:(t -> 'a Fiber.t) -> 'a Memo.t
