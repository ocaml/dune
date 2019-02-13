module type Raw_graph = Incremental_cycles_intf.Raw_graph
module type S = Incremental_cycles_intf.S

module Make (Raw_graph : Raw_graph) : S
  with type graph := Raw_graph.graph
   and type vertex := Raw_graph.vertex
