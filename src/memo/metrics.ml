open! Stdune

module Counter = struct
  type t = int ref

  let create () = ref 0
  let read t = !t
  let incr t = incr t
  let add t count = t := !t + count
end

module Restore = struct
  let nodes = Counter.create ()
  let edges = Counter.create ()
  let blocked = Counter.create ()

  let reset () =
    nodes := 0;
    edges := 0;
    blocked := 0
  ;;
end

module Compute = struct
  let nodes = Counter.create ()
  let edges = Counter.create ()
  let blocked = Counter.create ()

  let reset () =
    nodes := 0;
    edges := 0;
    blocked := 0
  ;;
end

module Cycle_detection = struct
  let nodes = Counter.create ()
  let edges = Counter.create ()

  let reset () =
    nodes := 0;
    edges := 0
  ;;
end

let reset () =
  Restore.reset ();
  Compute.reset ();
  Cycle_detection.reset ()
;;

let report ~reset_after_reporting =
  let memo =
    sprintf
      "Memo graph: %d/%d/%d nodes/edges/blocked (restore), %d/%d/%d nodes/edges/blocked \
       (compute)"
      (Counter.read Restore.nodes)
      (Counter.read Restore.edges)
      (Counter.read Restore.blocked)
      (Counter.read Compute.nodes)
      (Counter.read Compute.edges)
      (Counter.read Compute.blocked)
  in
  let cycle_detection =
    sprintf
      "Memo cycle detection graph: %d/%d/%d nodes/edges/paths"
      (Counter.read Cycle_detection.nodes)
      (Counter.read Cycle_detection.edges)
      (Counter.read Restore.blocked + Counter.read Compute.blocked)
  in
  if reset_after_reporting then reset ();
  String.concat ~sep:"\n" [ memo; cycle_detection ]
;;

let assert_invariants () =
  let nodes_in_cycle_detection_graph = Counter.read Cycle_detection.nodes in
  let nodes_restore = Counter.read Restore.nodes in
  let nodes_compute = Counter.read Compute.nodes in
  assert (nodes_in_cycle_detection_graph <= nodes_compute + nodes_restore);
  let edges_in_cycle_detection_graph = Counter.read Cycle_detection.edges in
  let edges_restore = Counter.read Restore.edges in
  let edges_compute = Counter.read Compute.edges in
  assert (edges_in_cycle_detection_graph <= edges_restore + edges_compute)
;;
