(** Command line arguments specification *)
open! Dune_engine

open! Stdune

(** This module implements a small DSL to specify the command line argument of a
    program as well as the dependencies and targets of the program at the same
    time.

    For instance to represent the argument of
    [ocamlc -o src/foo.exe src/foo.ml], one might write:

    {[
      [ A "-o"
      ; Target (Path.relatie dir "foo.exe")
      ; Dep (Path.relative dir "foo.ml")
      ]
    ]}

    This DSL was inspired from the ocamlbuild API. *)

open! Import

(** [A] stands for "atom", it is for command line arguments that are neither
    dependencies nor targets.

    [Path] is similar to [A] in the sense that it defines a command line
    argument that is neither a dependency or target. However, the difference
    between the two is that [A s] produces exactly the argument [s], while
    [Path p] produces a string that depends on where the command is executed.
    For instance [Path (Path.of_string "src/foo.ml")] will translate to
    "../src/foo.ml" if the command is started from the "test" directory. *)

module Args : sig
  type without_targets = [ `Without_targets ]

  type any =
    [ `Without_targets
    | `With_targets
    ]

  (** The type [expand] captures the meaning of a dynamic [Command.Args.t] that
      has no target declarations: it is a way to construct functions that given
      a current working directory [dir] compute the list of command line
      arguments of type [string list] and a set of dependencies of type
      [Dep.Set.t], or fail. You can use the constructor [Expand] to specify the
      meaning directly, which is sometimes useful, e.g. for memoization. *)
  type expand =
    dir:Path.t -> (string list * Dep.Set.t, fail) result Memo.Build.t

  type _ t =
    | A : string -> [> `Without_targets ] t
    | As : string list -> [> `Without_targets ] t
    | S : 'a t list -> [> `Without_targets ] t
    | Concat : string * 'a t list -> [> `Without_targets ] t
    | Dep : Path.t -> [> `Without_targets ] t
    | Deps : Path.t list -> [> `Without_targets ] t
    | Target : Path.Build.t -> [> `With_targets ] t
    | Path : Path.t -> [> `Without_targets ] t
    | Paths : Path.t list -> [> `Without_targets ] t
    | Hidden_deps : Dep.Set.t -> [> `Without_targets ] t
    | Hidden_targets : Path.Build.t list -> [> `With_targets ] t
    | Dyn : without_targets t Action_builder.t -> [> `Without_targets ] t
    | Fail : fail -> [> `Without_targets ] t
    | Expand : expand -> [> `Without_targets ] t

  (** Create with_targets command line arguments. *)
  val dyn : string list Action_builder.t -> [> `Without_targets ] t

  (** Create an empty command line. *)
  val empty : [> `Without_targets ] t

  (** Memoize the computation of command line arguments specified by a given
      expression. Use this function when the same subexpression appears in
      multiple [Command.Args.t] expressions to share both the time and memory
      required for the computation. *)
  val memo : without_targets t -> [> `Without_targets ] t

  val as_any : without_targets t -> any t
end

(* TODO: Using list in [with_targets t list] complicates the API unnecessarily:
   we can use the constructor [S] to concatenate lists instead. *)
val run :
     dir:Path.t
  -> ?stdout_to:Path.Build.t
  -> Action.Prog.t
  -> Args.any Args.t list
  -> Action.t Action_builder.With_targets.t

(** Same as [run], but for actions that don't produce targets *)
val run' :
     dir:Path.t
  -> Action.Prog.t
  -> Args.without_targets Args.t list
  -> Action.t Action_builder.t

(** [quote_args quote args] is [As \[quote; arg1; quote; arg2; ...\]] *)
val quote_args : string -> string list -> [> `Without_targets ] Args.t

val of_result : ([> `Without_targets ] as 'a) Args.t Or_exn.t -> 'a Args.t

val of_result_map :
  'a Or_exn.t -> f:('a -> ([> `Without_targets ] as 'b) Args.t) -> 'b Args.t

val fail : exn -> [> `Without_targets ] Args.t

module Ml_kind : sig
  val flag : Ml_kind.t -> [> `Without_targets ] Args.t

  val ppx_driver_flag : Ml_kind.t -> [> `Without_targets ] Args.t
end
