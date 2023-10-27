(** Various performance counters. *)

val enabled : bool ref
val nodes_restored : int ref
val nodes_computed : int ref
val edges_traversed : int ref
val nodes_in_cycle_detection_graph : int ref
val edges_in_cycle_detection_graph : int ref
val paths_in_cycle_detection_graph : int ref

(** Reset all counters to zero. *)
val reset : unit -> unit
