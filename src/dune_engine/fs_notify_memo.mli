open! Stdune
open Import

val depend : Path.t -> unit Memo.Build.t

val invalidate : Path.t -> unit
