(** dummy context for storing auxiliary stuff without polluting the rest *)

open Import

val t : Build_context.t

module Component : sig
  type dev_tool = Ocamlformat

  type t =
    | Lock_dir
    | Dev_tool of dev_tool

  val to_string : t -> string
  val equal : t -> t -> bool
end
