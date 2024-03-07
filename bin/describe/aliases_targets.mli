open Import

(** ls like commands for showing aliases and targets *)

module Aliases_cmd : sig
  (** The aliases command lists all the aliases available in the given
      directory, defaulting to the current working directory. *)
  val command : unit Cmd.t
end

module Targets_cmd : sig
  (** The targets command lists all the targets available in the given
      directory, defaulting to the current working directory. *)
  val command : unit Cmd.t
end
