(** An incremental cycle detection algorithm for directed graphs. *)

(** {1 Functorial interface} *)

(** Signature for the graph data structure on which the algorithm operates.
    It is the input signature of the [Make] functor.

   This corresponds to a standard imperative directed graph structure.

   Additionally, extra meta-data is associated to each node, to hold internal
   data of the cycle detection algorithm. The meta-data is written and accessed
   by the algorithm through the corresponding [set_*] and [get_*] functions; it
   must not be modified otherwise.

   The standard graph operations provided by [Raw_graph] are:
   - adding a new vertex;
   - adding a new edge between two existing vertices;
   - returning the list of successors of a vertex;
   - testing for equality of vertices.


   The extra meta-data that [Raw_graph] must provide is:
   - Vertices can be marked, and it must be possible to generate fresh marks.
     Intuitively, a mark can be implemented as an integer, and generating a
     fresh mark as incrementing some mark counter.
   - Each vertex has an associated integer  "level", which can be read and set.
   - Each vertex has an associated list of "incoming" vertices.
   - Each vertex has an associated "parent", which can be read and set to an
     other vertex of the graph.

   No particular assumption should be made by the implementor of [Raw_graph]
   about the contents of these fields.
*)
module type Raw_graph = Incremental_cycles_intf.Raw_graph

(** Output signature of the functor [Incremental_cycles.Make]. *)
module type S = Incremental_cycles_intf.S

(** The algorithm is provided as a functor parameterized over the directed graph
   implementation [Raw_graph].

   NB: The algorithm does not allocate or maintain (long-lived) data itself: it
   only mutates the graph by calling the operations provided by [Raw_graph].
*)
module Make (Raw_graph : Raw_graph) : S
  with type graph := Raw_graph.graph
   and type vertex := Raw_graph.vertex
