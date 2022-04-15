(** Parameters that influence rule execution *)

(** Such as:

    - should targets be set read-only?

    - should aliases be expanded when sandboxing rules?

    These often depend on the version of the Dune language used, which is
    written in the [dune-project] file. Depending on the execution parameters
    rather than the whole [dune-project] file means that when some part of the
    [dune-project] file changes and this part does not have an effect on rule
    execution, we can skip a bunch of work by not trying to re-execute all the
    rules. *)

type t

val equal : t -> t -> bool

val hash : t -> int

val to_dyn : t -> Dyn.t

module Action_output_on_success : sig
  (** How to deal with the output (stdout/stderr) of actions when they succeed. *)
  type t =
    | Print  (** Print it to the terminal. *)
    | Swallow
        (** Completely ignore it. There is no way for the user to access it but
            the output of Dune is clean. *)
    | Must_be_empty
        (** Require it to be empty. Treat the action as failed if it is not. *)

  val all : (string * t) list

  val equal : t -> t -> bool

  val hash : t -> int

  val to_dyn : t -> Dyn.t
end

(** {1 Constructors} *)

val builtin_default : t

val set_dune_version : Dune_lang.Syntax.Version.t -> t -> t

val set_action_stdout_on_success : Action_output_on_success.t -> t -> t

val set_action_stderr_on_success : Action_output_on_success.t -> t -> t

val set_expand_aliases_in_sandbox : bool -> t -> t

val add_workspace_root_to_build_path_prefix_map : t -> bool

(** As configured by [init] *)
val default : t Memo.t

(** {1 Accessors} *)

val dune_version : t -> Dune_lang.Syntax.Version.t

val should_remove_write_permissions_on_generated_files : t -> bool

val expand_aliases_in_sandbox : t -> bool

val action_stdout_on_success : t -> Action_output_on_success.t

val action_stderr_on_success : t -> Action_output_on_success.t

(** {1 Initialisation} *)

val init : t Memo.t -> unit
