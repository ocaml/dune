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

  (** [add dag v w] creates an arc going from [v] to [w]. @raise Cycle if
      creating the arc would create a cycle.
      This assumes that the arc does not already exist. *)
  val add : t -> node -> node -> unit

  (** [add_idempotent dag v w] creates an arc going from [v] to [w] unless
      it already exists. @raise Cycle if creating the arc would create a cycle. *)
  val add_idempotent : t -> node -> node -> unit

  (** [children v] returns all nodes [w] for which an arc going from [v] to [w]
      exists. *)
  val children : node -> node list

  (** Pretty print a node. *)
  val pp_node : value Fmt.t -> node Fmt.t

  (** [is_child v w] returns a boolean indicating if an arc going from [v] to
      [w] exists. *)
  val is_child : node -> node -> bool
end
