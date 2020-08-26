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
  type static = Static

  type dynamic = Dynamic

  (** The type [expand] captures the meaning of a static [Command.Args.t] that
      has no target declarations: it is a way to construct functions that given
      a current working directory [dir] compute the list of command line
      arguments of type [string list] and a set of dependencies of type
      [Dep.Set.t], or fail. You can use the constructor [Expand] to specify the
      meaning directly, which is sometimes useful, e.g. for memoization. *)
  type expand = dir:Path.t -> (string list * Dep.Set.t, fail) result

  type _ t =
    | A : string -> _ t
    | As : string list -> _ t
    | S : 'a t list -> 'a t
    | Concat : string * 'a t list -> 'a t
    | Dep : Path.t -> _ t
    | Deps : Path.t list -> _ t
    | Target : Path.Build.t -> dynamic t
    | Path : Path.t -> _ t
    | Paths : Path.t list -> _ t
    | Hidden_deps : Dep.Set.t -> _ t
    | Hidden_targets : Path.Build.t list -> dynamic t
    | Dyn : static t Build.t -> dynamic t
    | Fail : fail -> _ t
    | Expand : expand -> _ t

  (** Create dynamic command line arguments. *)
  val dyn : string list Build.t -> dynamic t

  (** Create an empty command line. *)
  val empty : _ t

  (** Memoize the computation of command line arguments specified by a given
      expression. Use this function when the same subexpression appears in
      multiple [Command.Args.t] expressions to share both the time and memory
      required for the computation. *)
  val memo : static t -> _ t
end

(* TODO: Using list in [dynamic t list] complicates the API unnecessarily: we
   can use the constructor [S] to concatenate lists instead. *)
val run :
     dir:Path.t
  -> ?stdout_to:Path.Build.t
  -> Action.Prog.t
  -> Args.dynamic Args.t list
  -> Action.t Build.With_targets.t

(** [quote_args quote args] is [As \[quote; arg1; quote; arg2; ...\]] *)
val quote_args : string -> string list -> _ Args.t

val of_result : 'a Args.t Or_exn.t -> 'a Args.t

val of_result_map : 'a Or_exn.t -> f:('a -> 'b Args.t) -> 'b Args.t

val fail : exn -> 'a Args.t

module Ml_kind : sig
  val flag : Ml_kind.t -> _ Args.t

  val ppx_driver_flag : Ml_kind.t -> _ Args.t
end
