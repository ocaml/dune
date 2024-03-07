open Import

(** Command group for dune describe *)
val group : unit Cmd.t

module Show : sig
  (** Command group for dune show (alias of describe) *)
  val group : unit Cmd.t
end
