(** [Rules] represents a collection of rules across a known set of directories. *)

open! Stdune

(** [rule] is a function that produces some build system rules
    such as ([Build_system.add_rule]) in a known directory. *)
type rule = unit -> unit

type t = private rule Path.Build.Map.t

val to_map : t -> rule Path.Build.Map.t

(* [Path] must be in build directory *)
val file_rule : rule:(Path.t * rule) -> unit

(* [Path] must be in build directory *)
val dir_rule : (Path.t * rule) -> unit

val union : t -> t -> t

val collect : (unit -> 'a) -> ('a * t)
