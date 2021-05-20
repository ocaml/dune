(** Integration with feedback-directed optimizations using ocamlfdo. *)
open! Dune_engine

open Stdune

type phase =
  | All
  | Compile
  | Emit

val linear_ext : string

val linear_fdo_ext : string

val phase_flags : phase option -> string list

val c_flags : Context.t -> string list

val cxx_flags : Context.t -> string list

val opt_rule : Compilation_context.t -> Module.t -> unit Memo.Build.t

module Linker_script : sig
  type t

  val create : Compilation_context.t -> Path.t -> t

  val flags : t -> _ Command.Args.t Memo.Build.t
end
