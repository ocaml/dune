module type Raw_graph = sig
  (** {1 Types} *)

  (** The graph data structure that is modified by the operations below. *)
  type graph

  (** The type of vertices of the graph. *)
  type vertex

  (** The type of marks (each vertex has an associated mark). *)
  type mark

  (** {1 Standard graph operations} *)

  (** NB: One must {e not} call [raw_add_edge] and [raw_add_vertex] manually, as
     it would break the internal invariants of the cycle detection algorithm.
     One must use instead the safe wrappers [add_vertex] and
     [add_edge_or_detect_cycle] that are provided as output of the
     [Incremental_cycles.Make] functor. *)

  (** [vertex_eq v1 v2] tests whether vertices [v1] and [v2] are equal. *)
  val vertex_eq : vertex -> vertex -> bool

  (** [get_outgoing g v] returns the list of successors of [v] in the graph. *)
  val get_outgoing : graph -> vertex -> vertex list

  (** [raw_add_edge g v w] inserts a new (directed) arc between vertices [v] and
     [w].

      [v] and [w] must have been previously added to the graph using
     [raw_add_vertex], and the arc [v]->[w] must not already be in the graph. *)
  val raw_add_edge : graph -> vertex -> vertex -> unit

  (** [raw_add_vertex g v] inserts a new vertex [v] into the graph.

      - The mark of a new vertex must be some "default mark" which is different
        from all marks that can be returned by [new_mark].
      - The level (returned by [get_level]) of a new vertex must be [1].
      - The incoming vertices (returned by [get_incoming]) of a new vertex
        must be [\[\]] (the empty list).
  *)
  val raw_add_vertex : graph -> vertex -> unit


  (** {1 Operations on graph meta-data} *)

  (** [new_mark g] generates a fresh mark.

     More specifically, this mark must be different from all the marks
     previously returned by [new_mark g] (on the same graph [g]). It must also
     be different from all the marks currently associated to vertices of the
     graph [g]. *)
  val new_mark : graph -> mark

  (** [is_marked g v m] tests whether the vertex [v] has mark [m].

     NB: [is_marked g v m] can only hold if [set_mark g v m] has been called
     previously. *)
  val is_marked : graph -> vertex -> mark -> bool

  (** [set_mark g v m] sets the mark of vertex [v] to be [m]. *)
  val set_mark : graph -> vertex -> mark -> unit

  (** [get_level g v] returns the level of node [v].

      It is either the value previously set by [set_level], or the default value
     for a newly created vertex (i.e. [1], see [raw_add_vertex]). *)
  val get_level : graph -> vertex -> int

  (** [set_level g v l] sets the level of node [v] to be [l]. *)
  val set_level : graph -> vertex -> int -> unit

  (** [get_incoming g v] returns the list of "incoming" vertices of node [v].

      It corresponds to either the default value for a newly created vertex
     (i.e. the empty list, see [raw_add_vertex]), or the result of previous
     calls to [clear_incoming] and [add_incoming]. *)
  val get_incoming : graph -> vertex -> vertex list

  (** [clear_incoming g v] sets the list of "incoming" vertices of [v] to be the
     empty list. *)
  val clear_incoming : graph -> vertex -> unit

  (** [add_incoming g v w] adds [w] to the list of "incoming" vertices of [v]. *)
  val add_incoming : graph -> vertex -> vertex -> unit

  (** [get_parent g v] returns the "parent" of node [v], as set by [set_parent].

      {e Note: there is no default value for [get_parent]}. It is fine for
     [get_parent g v] to fail if it is called while [set_parent g v w] has not
     be called beforehand. *)
  val get_parent : graph -> vertex -> vertex

  (** [set_parent g v w] sets the "parent" of node [v] to be [w]. *)
  val set_parent : graph -> vertex -> vertex -> unit


  (** {1 Asymptotic complexity} *)

  (** All the operations provided by [Raw_graph] must run in constant time. *)
end

module type S = sig
  (** The [graph] type from [Raw_graph].

     NB: one must always start from an empty [graph], and add vertices and edges
     using the functions [add_edge_or_detect_cycle] and [add_vertex] provided
     below. It is {e not} safe to use these functions on a graph manually
     constructed using the internal operations of [Raw_graph]. *)
  type graph

  (** The [vertex] type from [Raw_graph]. *)
  type vertex

  (** {1 Operations} *)

  (** The result of [add_edge_or_detect_cycle]. *)
  type add_edge_result =
    | EdgeAdded
    | EdgeCreatesCycle of (unit -> vertex list)

  (** [add_edge_or_detect_cycle g v w] adds the edge [v]->[w] to the graph [g],
     provided doing so does not make the graph cyclic.

      This assumes that [v] and [w] have previously been added to the graph
     using [add_vertex], and that the edge [v]->[w] is not already in the graph.

      - If adding the edge does not make the graph cyclic, the function returns
     [EdgeAdded], and updates the graph [g].

      - If adding the edge would make the graph cyclic (i.e. there is currently
     a path from [w] to [v]), the function returns [EdgeCreatesCycle
     compute_cycle]. Then, [compute_cycle ()] can be called to get the list of
     vertices that form a path from [w] to [v] (in linear time).

      In the [EdgeCreatesCycle] case, the edge [v]->[w] is not inserted in the
     graph, but the internal invariants of the graph do not hold anymore. It is
     {e not} safe to call again [add_edge_or_detect_cycle] or [add_vertex] on
     the graph.
  *)
  val add_edge_or_detect_cycle :
    graph -> vertex -> vertex ->
    add_edge_result

  (** [add_vertex g v] adds the vertex [v] to the graph [g]. *)
  val add_vertex : graph -> vertex -> unit

  (** {1 Asymptotic complexity} *)

  (** Inserting [n] vertices and [m] edges (using [add_edge_or_detect_cycle] and
     [add_vertex]) has complexity [O(m * min(m^1/2, n^2/3) + n)].

      Roughly speaking, in a sparse enough graph (for which this algorithm is
     optimized), this means that each edge insertion has amortized complexity
     [O(sqrt(m))] (as opposed to [O(m)] for a naive algorithm).
  *)
end
