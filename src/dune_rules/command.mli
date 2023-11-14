(** Command line arguments specification *)

open Import

(** This module implements a small DSL to specify the command line argument of a
    program as well as the dependencies and targets of the program at the same
    time.

    For instance to represent the argument of
    [ocamlc -o src/foo.exe src/foo.ml], one might write:

    {[
      [ A "-o"; Target (Path.relatie dir "foo.exe"); Dep (Path.relative dir "foo.ml") ]
    ]}

    This DSL was inspired from the ocamlbuild API. *)

(** [A] stands for "atom", it is for command line arguments that are neither
    dependencies nor targets.

    [Path] is similar to [A] in the sense that it defines a command line
    argument that is neither a dependency or target. However, the difference
    between the two is that [A s] produces exactly the argument [s], while
    [Path p] produces a string that depends on where the command is executed.
    For instance [Path (Path.of_string "src/foo.ml")] will translate to
    "../src/foo.ml" if the command is started from the "test" directory. *)

module Args : sig
  type without_targets = [ `Others ]

  type any =
    [ `Others
    | `Targets
    ]

  (** The type [expand] captures the meaning of a [Command.Args.t] that has no
      target declarations: it is a way to construct functions that given a
      current working directory [dir] compute the list of command line arguments
      of type [string list] in the action builder monad. You can use the
      constructor [Expand] to specify the meaning directly, which is sometimes
      useful, e.g. for memoization. *)
  type expand = dir:Path.t -> string list Action_builder.t

  type _ t =
    | A : string -> _ t
    | As : string list -> _ t
    | S : 'a t list -> 'a t
    | Concat : string * 'a t list -> 'a t
    | Dep : Path.t -> _ t
    | Deps : Path.t list -> _ t
    | Target : Path.Build.t -> [> `Targets ] t
    | Path : Path.t -> _ t
    | Paths : Path.t list -> _ t
    | Hidden_deps : Dep.Set.t -> _ t
    | Hidden_targets : Path.Build.t list -> [> `Targets ] t
    | Dyn : without_targets t Action_builder.t -> _ t
    | Expand : expand -> _ t

  (** Create dynamic command line arguments. *)
  val dyn : string list Action_builder.t -> _ t

  (** Create an empty command line. *)
  val empty : _ t

  (** Memoize the computation of command line arguments specified by a given
      expression. Use this function when the same subexpression appears in
      multiple [Command.Args.t] expressions to share both the time and memory
      required for the computation. *)
  val memo : without_targets t -> _ t

  val as_any : without_targets t -> any t
end

(** Same as [run] but we delay determining the program *)
val run_dyn_prog
  :  dir:Path.t
  -> ?sandbox:Sandbox_config.t
  -> ?stdout_to:Path.Build.t
  -> Action.Prog.t Action_builder.t
  -> Args.any Args.t list
  -> Action.Full.t Action_builder.With_targets.t

val run
  :  dir:Path.t
  -> ?sandbox:Sandbox_config.t
  -> ?stdout_to:Path.Build.t
  -> Action.Prog.t
  -> Args.any Args.t list
  -> Action.Full.t Action_builder.With_targets.t

(** Same as [run], but for actions that don't produce targets *)
val run'
  :  dir:Path.t
  -> Action.Prog.t
  -> Args.without_targets Args.t list
  -> Action.Full.t Action_builder.t

(** [quote_args quote args] is [As [quote; arg1; quote; arg2; ...]] *)
val quote_args : string -> string list -> _ Args.t

module Ml_kind : sig
  val flag : Ml_kind.t -> _ Args.t
  val ppx_driver_flag : Ml_kind.t -> _ Args.t
end

(** [expand ~dir args] interprets the command line arguments [args] to produce
    corresponding strings, assuming they will be used as arguments to run a
    command in directory [dir]. *)
val expand : dir:Path.t -> 'a Args.t -> string list Action_builder.With_targets.t

(** [expand_no_targets ~dir args] interprets the command line arguments [args]
    to produce corresponding strings, assuming they will be used as arguments to
    run a command in directory [dir]. *)
val expand_no_targets
  :  dir:Path.t
  -> Args.without_targets Args.t
  -> string list Action_builder.t
