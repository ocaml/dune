(** Dependency handling for the .merlin-exists file *)
open Stdune

val dep : dir:Path.t -> ('a, 'a) Build.t
val create : dir:Path.t -> ('a, Action.t) Build.t
