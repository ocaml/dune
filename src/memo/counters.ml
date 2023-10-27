(* CR-soon amokhov: Move conditionals checking [enabled] from [memo.ml] to this module. *)
let enabled = ref false
let nodes_restored = ref 0
let nodes_computed = ref 0
let edges_traversed = ref 0
let nodes_in_cycle_detection_graph = ref 0
let edges_in_cycle_detection_graph = ref 0
let paths_in_cycle_detection_graph = ref 0

let reset () =
  nodes_restored := 0;
  nodes_computed := 0;
  edges_traversed := 0;
  nodes_in_cycle_detection_graph := 0;
  edges_in_cycle_detection_graph := 0;
  paths_in_cycle_detection_graph := 0
;;
