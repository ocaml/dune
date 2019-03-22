open! Stdune
(** Command line arguments specification *)

(** This module implements a small DSL to specify the command line
    argument of a program as well as the dependencies and targets of
    the program at the same time.

    For instance to represent the argument of [ocamlc -o src/foo.exe
    src/foo.ml], one might write:

    {[
      [ A "-o"
      ; Target (Path.relatie  dir "foo.exe")
      ; Dep    (Path.relative dir "foo.ml")
      ]
    ]}

    This DSL was inspired from the ocamlbuild API.  *)

open! Import

(** [A] stands for "atom", it is for command line arguments that are
    neither dependencies nor targets.

    [Path] is similar to [A] in the sense that it defines a command
    line argument that is neither a dependency or target. However, the
    difference between the two is that [A s] produces exactly the
    argument [s], while [Path p] produces a string that depends on
    where the command is executed. For instance [Path (Path.of_string
    "src/foo.ml")] will translate to "../src/foo.ml" if the command is
    started from the "test" directory.  *)
type static = Static
type dynamic = Dynamic

type ('a, _) t =
  | A        : string -> ('a, _) t
  | As       : string list -> ('a, _) t
  | S        : ('a, 'b) t list -> ('a, 'b) t
  | Concat   : string * ('a, 'b) t list  -> ('a, 'b) t
  | Dep      : Path.t -> ('a, _) t
  | Deps     : Path.t list -> ('a, _) t
  | Target   : Path.t -> ('a, dynamic) t
  | Path     : Path.t -> ('a, _) t
  | Paths    : Path.t list -> ('a, _) t
  | Hidden_deps    : Path.t list -> ('a, _) t
  | Hidden_targets : Path.t list -> ('a, dynamic) t
  | Dyn      : ('a -> (Nothing.t, static) t) -> ('a, dynamic) t
  | Fail     : fail -> ('a, _) t

val static_deps    : _ t list -> Path.Set.t
val add_targets : (_, dynamic) t list -> Path.t list -> Path.t list
val expand      : dir:Path.t -> ('a, dynamic) t list -> 'a -> string list * Path.Set.t

(** [quote_args quote args] is [As \[quote; arg1; quote; arg2; ...\]] *)
val quote_args : string -> string list -> _ t

val of_result : ('a, 'b) t Or_exn.t -> ('a, 'b) t
val of_result_map : 'a Or_exn.t -> f:('a -> ('b, 'c) t) -> ('b, 'c) t
