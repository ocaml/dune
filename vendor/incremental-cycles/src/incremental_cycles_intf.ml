module type Raw_graph = sig
  type mark
  type graph
  type vertex

  val new_mark : graph -> mark

  val vertex_eq : vertex -> vertex -> bool

  val is_marked : graph -> vertex -> mark -> bool
  val set_mark : graph -> vertex -> mark -> unit

  val get_level : graph -> vertex -> int
  val set_level : graph -> vertex -> int -> unit

  val get_incoming : graph -> vertex -> vertex list
  val clear_incoming : graph -> vertex -> unit
  val add_incoming : graph -> vertex -> vertex -> unit

  val get_parent : graph -> vertex -> vertex
  val set_parent : graph -> vertex -> vertex -> unit

  val get_outgoing : graph -> vertex -> vertex list

  val raw_add_edge : graph -> vertex -> vertex -> unit
  val raw_add_vertex : graph -> vertex -> unit
end

module type S = sig
  type graph
  type vertex

  type add_edge_result =
    | EdgeAdded
    | EdgeCreatesCycle of (unit -> vertex list)

  val add_edge_or_detect_cycle : graph -> vertex -> vertex -> add_edge_result
  val add_vertex : graph -> vertex -> unit
end
