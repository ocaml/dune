(** Ctypes integration *)
open! Dune_engine

(** Ctypes is a library for generating C-stubs from pure OCaml.

    These dune rules are to help reduce the boilerplate involved in
    setting up the build system tooling to generate the stubs. *)

type t

type Stanza.t += T of t

val decode : t Dune_lang.Decoder.t
