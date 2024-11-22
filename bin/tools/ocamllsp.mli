open! Import

module Exec : sig
  (** Command to run ocamllsp, installing it if necessary *)
  val command : unit Cmd.t
end

module Install : sig
  (** Command to install ocamllsp *)
  val command : unit Cmd.t
end
