(** Detection of cycles in dynamic dags *)

open! Stdune

module type Value = sig
  type t
end

module type S = sig
  (** A DAG (Directed Acyclic Graph). *)
  type t

  (** Info about a node in the DAG. *)
  type node_info

  (** Type of values attached to nodes. *)
  type value

  type node =
    { data : value
    ; info : node_info
    }

  (** A cycle has been found while adding an arc. *)
  exception Cycle of node list

  (** [create ()] creates a directed acyclic graph. *)
  val create : unit -> t

  (** [create_node_info dag v] creates new node info that belongs to [dag]. *)
  val create_node_info : t -> node_info

  (** [add_assuming_missing dag v w] creates an arc going from [v] to [w]
      assuming it doesn't already exists. The the arc does exist, the behaviuor
      is undefined.

      @raise Cycle if creating the arc would create a cycle. *)
  val add_assuming_missing : t -> node -> node -> unit

  (** Pretty print a node. *)
  val pp_node :
    (Format.formatter -> value -> unit) -> Format.formatter -> node -> unit
end
