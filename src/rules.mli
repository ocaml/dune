(** [Rules] represents a collection of rules across a known finite set of
    directories. *)

open! Stdune

module Dir_rules : sig
  (** [rule] is a function that produces some build system rules
      such as ([Build_system.add_rule]) in a known directory. *)
  type t = unit -> unit

  val empty : t
  val union : t -> t -> t
end

type t = private Dir_rules.t Path.Build.Map.t

val to_map : t -> Dir_rules.t Path.Build.Map.t

(* [Path] must be in build directory *)
val file_rule : rule:(Path.t * Dir_rules.t) -> unit

(* [Path] must be in build directory *)
val dir_rule : (Path.t * Dir_rules.t) -> unit

val union : t -> t -> t

val collect : (unit -> 'a) -> ('a * t)

val collect_unit : (unit -> unit) -> t
