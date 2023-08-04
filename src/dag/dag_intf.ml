(** Detection of cycles in dynamic dags *)

open! Stdune

module type Value = sig
  type t
end

(** A DAG (Directed Acyclic Graph). *)
module type S = sig
  (** Type of values attached to nodes. *)
  type value

  type node

  val value : node -> value

  module Id : Id.S

  val node_id : node -> Id.t

  (** A cycle has been found while adding an arc. *)
  exception Cycle of node list

  (** [create_node v] creates new node info that belongs to [dag]. *)
  val create_node : value -> node

  (** [add_assuming_missing dag v w] creates an arc going from [v] to [w]
      assuming it doesn't already exists. If the arc does exist, the behaviour
      is undefined.

      @raise Cycle if creating the arc would create a cycle. *)
  val add_assuming_missing : node -> node -> unit

  (** Pretty print a node. *)
  val pp_node : (Format.formatter -> value -> unit) -> Format.formatter -> node -> unit
end
