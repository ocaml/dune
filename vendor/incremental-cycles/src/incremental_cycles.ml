
include Incremental_cycles_intf

module Make (Raw_graph : Raw_graph) : S
  with type graph := Raw_graph.graph
   and type vertex := Raw_graph.vertex =
struct

(* This implements the algorithm of incremental cycle detection described in
   Section 2 of the following paper:

   A New Approach to Incremental Cycle Detection and Related Problems

   Bender, M. A., Fineman, J. T., Gilbert, S., & Tarjan, R. E. (2015).

   https://dl.acm.org/citation.cfm?id=2756553
*)

(******************************************************************************)
(* The implementation of the algorithm only depends on an abstract graph
   structure, here implemented by [Raw_graph].

   Here, [raw_graph.ml] provides a concrete implementation, but the proof
   only relies on the abstract interface it implements. In the final exported
   code (see the export/ directory), [Raw_graph] becomes a functor parameter.
*)

open Raw_graph

(******************************************************************************)
(* Interruptible fold_left.

   At each step, the client function decides whether it wants to continue (by
   using [Continue new_accumulator]) or stop (by using [Break return_value]).

   Ultimately, [interruptible_fold] returns either the last accumulator or the
   value returned by [Break], along with a boolean indicating whether it was
   interrupted prematurely.
*)

type ('a, 'b) interruptible_fold_step =
  | Continue of 'a
  | Break of 'b

let rec interruptible_fold f l acc =
  match l with
  | [] -> Continue acc
  | x :: xs ->
    let res = f x acc in
    match res with
    | Continue acc -> interruptible_fold f xs acc
    | Break _ -> res

(******************************************************************************)
(* The cycle detection algorithm, implemented as an [add_edge] function on
   [graph], which either successfully inserts the edge, or reports a cycle. *)

type visit_backward_result =
  | VisitBackwardCompleted
  | VisitBackwardInterrupted
  | VisitBackwardCyclic

(* Traverse the graph backwards from entries in [stack], looking for [target],
   and marking explored vertices with [mark].

   If a path to [target] is found, return [VisitBackwardCyclic].

   If [fuel] runs out, return [VisitBackwardInterrupted].

   Otherwise, report that the search was complete with [VisitBackwardCompleted].
*)
let rec visit_backward
    (g: graph) (target: vertex) (mark: mark)
    (fuel: int) (stack: vertex list):
  visit_backward_result
  =
  (* fuel >= 0 *)
  match stack with
  | [] -> VisitBackwardCompleted
  | vertex :: stack ->
    let res = interruptible_fold (fun y (stack, fuel) ->
      if fuel = 0 then
        (* There is no fuel left *)
        Break true
      else if is_marked g y mark then
        (* This vertex has already been visited, skip it *)
        Continue (stack, fuel - 1)
      else if vertex_eq y target then
        (* A path to [target] has been found *)
        Break false
      else begin
        set_mark g y mark;
        set_parent g y vertex;
        Continue (y :: stack, fuel - 1)
      end
    ) (get_incoming g vertex) (stack, fuel)
    in
    match res with
    | Break timeout ->
      if timeout then VisitBackwardInterrupted
      else (set_parent g target vertex; VisitBackwardCyclic)
    | Continue (stack, fuel) ->
      visit_backward g target mark fuel stack

type backward_search_result =
  | BackwardForward of int * mark
  | BackwardCyclic
  | BackwardAcyclic

(* The whole backwards search phase (Step 2 of the algorithm). Explores the
   graph backwards starting from [v], and looking for [w].
   This function mainly calls [visit_backward] and does some post-processing.

   If [w] is found, return [BackwardCyclic].

   If [w] is not found and the algorithm should continue with the forward
   search phase, return [BackwardForward (new_w_level, visited)], where
   [new_w_level] is the level at which [w] needs to be put, and [visited]
   is the mark of vertices that have been visited during the search.

   If [w] is not found and the algorithm should directly skip to the last step,
   return [BackwardAcyclic].
*)
let backward_search
    (fuel: int)
    (g: graph) (v: vertex) (w: vertex):
  backward_search_result
  =
  let mark = new_mark g in
  let v_level = get_level g v in
  set_mark g v mark;
  match visit_backward g w mark fuel [v] with
  | VisitBackwardCyclic -> BackwardCyclic
  | VisitBackwardInterrupted ->
    (* w_level < v_level + 1 *)
    BackwardForward (v_level + 1, mark)
  | VisitBackwardCompleted ->
    let w_level = get_level g w in
    if w_level = v_level then
      BackwardAcyclic
    else
      (* w_level < v_level *)
      BackwardForward (v_level, mark)

type forward_search_result =
  | ForwardCyclic of vertex * vertex
  | ForwardCompleted

(* Traverse the graph forwards. [stack] contains the current working set of
   vertices; these are at level [new_level] but their neighbors have not been
   yet all visited.

   Only follow edges that point to vertices with a smaller level, but update the
   incoming edges sets for all vertices encountered.

   If a vertex that has been visited during the backward search phase is
   encountered, return [ForwardCyclic]. Otherwise, return [ForwardCompleted]. *)
let rec visit_forward
    (g: graph) (new_level: int) (visited: mark)
    (stack: vertex list):
  forward_search_result
  =
  match stack with
  | [] -> ForwardCompleted
  | x :: stack ->
    let res = interruptible_fold (fun y stack ->
      if is_marked g y visited then
        (* We found a path to a marked vertex *)
        Break y
      else begin
        let y_level = get_level g y in
        set_parent g y x;
        if y_level < new_level then begin
          set_level g y new_level;
          clear_incoming g y;
          add_incoming g y x;
          Continue (y :: stack)
        end else if y_level = new_level then begin
          add_incoming g y x;
          Continue stack
        end else (* y_level > new_level *)
          Continue stack
      end
    ) (get_outgoing g x) stack
    in
    match res with
    | Break y -> ForwardCyclic (x, y)
    | Continue stack -> visit_forward g new_level visited stack

(* The whole forward search phase (Step 3 of the algorithm). Explores the
   graph forwards starting from [w], updating the levels and incoming edges
   sets.

   This function is a simple wrapper over [visit_forward].
*)
let forward_search
    (g: graph) (w: vertex) (new_w_level: int) (visited: mark):
  forward_search_result
  =
  clear_incoming g w;
  set_level g w new_w_level;
  visit_forward g new_w_level visited [w]


type add_edge_result =
  | EdgeAdded
  | EdgeCreatesCycle of (unit -> vertex list)

let rec list_of_parents
  (g: graph) (x: vertex) (y: vertex) (acc: vertex list):
  vertex list
  =
  if vertex_eq x y then acc
  else
    let p = get_parent g x in
    let acc' = p :: acc in
    if vertex_eq p y then acc'
    else list_of_parents g p y acc'

(* (z, t) is an edge of the graph such that:
   - z has been visited by the forward traversal
   - t has been visited by the backward traversal

   So the path from w to v is of the form:
   w -> ... -> z -> t -> ... -> v

   [compute_cycle] returns the list of nodes in that path (including w and v).
*)
let compute_cycle (g: graph) (v: vertex) (w: vertex) (z: vertex) (t: vertex) =
  list_of_parents g z w (z :: t :: List.rev (list_of_parents g t v []))

(* The core of the algorithm, wrapping up the previous phases.

   This efficiently checks if there is a path from [w] to [v].
   If there is none, then it adds the edge [(v, w)] to the graph. *)
let add_edge_or_detect_cycle (g: graph) (v: vertex) (w: vertex) =
  let succeed () =
    raw_add_edge g v w;
    if get_level g v = get_level g w then
      add_incoming g w v;
    EdgeAdded
  in
  if vertex_eq v w then
    EdgeCreatesCycle (fun () -> [v])
  else if get_level g w > get_level g v then
    (* There cannot be a path from [w] to [v], as levels form a
       pseudo-lexicographic ordering: edges always go to equal or increasing
       levels. *)
    succeed ()
  else match backward_search (get_level g v) g v w with
    | BackwardCyclic ->
      EdgeCreatesCycle (fun () -> w :: List.rev (list_of_parents g w v []))
    | BackwardAcyclic -> succeed ()
    | BackwardForward (new_level, visited) ->
      match forward_search g w new_level visited with
      | ForwardCyclic (z, t) ->
        EdgeCreatesCycle (fun () -> compute_cycle g v w z t)
      | ForwardCompleted -> succeed ()

let add_vertex (g: graph) (v: vertex) =
  raw_add_vertex g v

end

