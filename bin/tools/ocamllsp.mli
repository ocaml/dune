open! Import

(** Command to run ocamllsp, installing it if necessary *)

module Exec : sig
  val command : unit Cmd.t
end

(** Command to print ocamllsp's path if exists *)

module Which : sig
  val command : unit Cmd.t
end
