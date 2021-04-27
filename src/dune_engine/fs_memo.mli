open! Stdune
open Import

(** All functions in this module raise a code error when given a path in the
    build directory. *)

(* CR-someday amokhov: Note that currently the scheduler generates [invalidate]
   calls only for source files, because we don't watch external files. We should
   try to implement at least a partial support for watching external files. *)

(** Check if a source or external file exists and declare a dependency on it. *)
val file_exists : Path.t -> bool Memo.Build.t

(** Digest the contents of a source or external file and declare a dependency on
    it. *)
val file_digest : Path.t -> Digest.t Memo.Build.t

module Invalidate_result : sig
  type t =
    | Invalidated
    | Skipped  (** The given path is not tracked by the build system. *)
end

(** Invalidate a source or external file, causing everything that depends on it
    to be rebuilt. *)
val invalidate : Path.t -> Invalidate_result.t
