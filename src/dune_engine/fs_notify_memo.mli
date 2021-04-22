open! Stdune
open Import

val depend : Path.t -> unit Memo.Build.t

module Invalidate_result : sig
  type t =
    | Invalidated
    | Skipped  (** The given path is not tracked by the build system. *)
end

val invalidate : Path.t -> Invalidate_result.t
