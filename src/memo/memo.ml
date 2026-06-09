module Metrics0 = Metrics
open Stdune
module Metrics = Metrics0
open Fiber.O
module Graph = Dune_graph.Graph
module Console = Console
module Debug = Memo_debug
module Event = Spec.Event
include Fiber

let when_ x y =
  match x with
  | true -> y ()
  | false -> return ()
;;

let of_reproducible_fiber = Fun.id

module type Input = sig
  type t

  include Table.Key with type t := t
end

(* We can get rid of this once we use the memoization system more pervasively
   and all the dependencies are properly specified *)
module Caches = struct
  let cleaners = ref []
  let register ~clear = cleaners := clear :: !cleaners
  let clear () = List.iter !cleaners ~f:(fun f -> f ())
end

exception Non_reproducible = Exec.Non_reproducible

open Node
module Error = Node.Error
module Cycle_error = Node.Cycle_error
module Table = Node.Table

let () =
  Printexc.register_printer (fun exn ->
    let dyn =
      let open Dyn in
      match exn with
      | Error.E err -> Some (variant "Memo.Error.E" [ Error.to_dyn err ])
      | Cycle_error.E frames ->
        Some (variant "Cycle_error.E" [ Cycle_error.to_dyn frames ])
      | Non_reproducible exn -> Some (variant "Memo.Non_reproducible" [ Exn.to_dyn exn ])
      | _ -> None
    in
    Option.map dyn ~f:Dyn.to_string)
;;

module To_open = struct
  module Stack_frame = Stack_frame_with_state
end

open To_open
include Parallel

let pp_stack () =
  let open Pp.O in
  let+ stack = Call_stack.get_call_stack () in
  Pp.vbox
    (Pp.box (Pp.text "Memoized function stack:")
     ++ Pp.cut
     ++ Pp.chain stack ~f:(fun frame -> Dyn.pp (Stack_frame.to_dyn frame)))
;;

let dump_stack () =
  let+ pp = pp_stack () in
  Console.print [ pp ]
;;

module Stack_frame = struct
  include Dep_node.Packed

  let name (Dep_node.T t) = t.spec.name
  let input (Dep_node.T t) = Dep_node.input_to_dyn t
  let to_dyn = Dep_node.Packed.to_dyn_without_state

  let as_instance_of stack_frame ~(of_ : _ Table.t) =
    Dep_node.Packed.as_instance_of stack_frame of_.spec.witness
  ;;
end

(* There are two approaches to invalidating memoization nodes. Currently, when a
   node is invalidated by calling [invalidate_dep_node], only the node itself is
   marked as "changed" (by setting its [state] to [Out_of_date]). Then the whole
   graph is marked as "possibly changed" by calling [Run.restart ()] that makes
   all remaining [last_validated_at : Run.t] values out of date in O(1) time. In
   the next run, the whole graph is traversed from top to bottom to discover
   "actual changes" and recompute all the nodes affected by these changes. One
   disadvantage of this approach is that the whole graph needs to be traversed
   even if only a small part of it depends on the set of invalidated nodes.

   An alternative approach is as follows. When [invalidate_dep_node] is called,
   recursively mark all of its reverse dependencies as "possibly changed". Then,
   in the next run, traverse only the marked part of graph (instead of the whole
   graph as we do currently). One disadvantage of this approach is that every
   node needs to store a list of its reverse dependencies, which introduces
   cyclic memory references and complicates garbage collection. Another issue is
   that recursively marking all reverse dependencies as "possibly changed" can
   still result in marking too much, because there is no way to take the cutoff
   predicate into account when propagating "possible changes".

   Is it worth switching from the current approach to the alternative? It's best
   to answer this question by benchmarking. This is not urgent but is worth
   documenting in the code. *)
let invalidate_dep_node (dep_node : _ Dep_node.t) =
  match dep_node.state with
  | Cached -> dep_node.state <- Out_of_date
  | Not_cached | Out_of_date -> ()
  | Restoring _ ->
    Code_error.raise
      "invalidate_dep_node called on a node in Restoring state"
      [ "dep_node", Dep_node.to_dyn_without_state dep_node ]
  | Computing _ ->
    Code_error.raise
      "invalidate_dep_node called on a node in Computing state"
      [ "dep_node", Dep_node.to_dyn_without_state dep_node ]
;;

let invalidate_store = Store.iter ~f:invalidate_dep_node

let create_with_cache
      (type i o)
      name
      ~cache
      ~input
      ~cutoff
      ~human_readable_description
      ~on_event
      (f : i -> o Fiber.t)
  : (i, o) Table.t
  =
  let spec =
    Spec.create ~name:(Some name) ~input ~cutoff ~human_readable_description ?on_event f
  in
  Caches.register ~clear:(fun () ->
    Store.clear cache;
    invalidate_store cache);
  { cache; spec }
;;

let create_with_store
      (type i)
      name
      ~store:(module S : Store_intf.S with type key = i)
      ~input
      ?cutoff
      ?human_readable_description
      ?on_event
      f
  =
  let cache = Store.make (module S) in
  create_with_cache name ~cache ~input ~cutoff ~human_readable_description ~on_event f
;;

let create
      (type i)
      name
      ~input:(module Input : Input with type t = i)
      ?(initial_store_size = 2)
      ?cutoff
      ?human_readable_description
      ?on_event
      f
  =
  (* This mutable table is safe: the implementation tracks all dependencies. *)
  let cache = Store.of_table (Stdune.Table.create (module Input) initial_store_size) in
  let input = (module Input : Store_intf.Input with type t = i) in
  create_with_cache name ~cache ~input ~cutoff ~human_readable_description ~on_event f
;;

let make_dep_node ~spec ~input : _ Dep_node.t =
  { id = Id.gen ()
  ; input
  ; spec
  ; state = Not_cached
  ; value = Uninitialized
  ; runs = Run.Pair.create ~last_changed_at:Run.invalid ~last_validated_at:Run.invalid
  ; deps = Deps.empty
  }
;;

let dep_node (t : (_, _) Table.t) input =
  Store.find_or_add t.cache input ~f:(fun input -> make_dep_node ~spec:t.spec ~input)
;;

let check_point = Exec.check_point
let exec (type i o) (t : (i, o) Table.t) i = Exec.exec_dep_node (dep_node t i)

let create_rec
      name
      ~input
      ?initial_store_size
      ?cutoff
      ?human_readable_description
      ?on_event
      f
  =
  let rec table =
    lazy
      (create
         name
         ~input
         ?initial_store_size
         ?cutoff
         ?human_readable_description
         ?on_event
         (fun input ->
            let table = Stdlib.Lazy.force table in
            f (exec table) input))
  in
  Stdlib.Lazy.force table
;;

let dump_cached_graph ?(on_not_cached = `Raise) ?(time_nodes = false) cell =
  let rec collect_graph (Dep_node.T dep_node) graph : Graph.t Fiber.t =
    let src_id = Id.to_int dep_node.id in
    match get_cached_deps_in_current_run dep_node with
    | Some deps ->
      let* attributes =
        if time_nodes
        then (
          let start = Time.now () in
          (* CR-someday cmoseley: We could record errors here and include them
             as part of the graph. *)
          let+ (_ : (_, Collect_errors_monoid.t) result) =
            Exec.report_and_collect_errors (fun () -> dep_node.spec.f dep_node.input)
          in
          let runtime = Time.Span.to_secs (Time.diff (Time.now ()) start) in
          String.Map.of_list_exn [ "runtime", Graph.Attribute.Float runtime ])
        else Fiber.return String.Map.empty
      in
      let graph = Graph.add_node graph ~id:src_id ?label:dep_node.spec.name ~attributes in
      List.fold_left
        (Deps.For_debugging.to_list deps)
        ~init:(Fiber.return graph)
        ~f:(fun graph (Dep_node.T dst_node as packed) ->
          let* graph = graph in
          let dst_id = Id.to_int dst_node.id in
          let graph = Graph.add_edge graph ~src_id ~dst_id in
          if Graph.has_node graph ~id:dst_id
          then Fiber.return graph
          else collect_graph packed graph)
    | None ->
      (match on_not_cached with
       | `Raise -> failwith "Memo graph contains uncached nodes"
       | `Ignore -> Fiber.return graph)
  in
  Error_handler.with_error_handler
    (fun (_ : Exn_with_backtrace.t) -> Fiber.return ())
    (fun () -> collect_graph (Dep_node.T cell) Graph.empty)
;;

let get_call_stack = Call_stack.get_call_stack_without_state

module Invalidation = struct
  (* This is currently used only for informing the user about the reason for
     restarting a build. *)
  module Reason = struct
    type t =
      | Unknown
      | Path_changed of Path.t
      | Event_queue_overflow
      | Upgrade
      | Test
      | Variable_changed of string

    let to_string_hum = function
      | Unknown -> None
      | Path_changed path -> Some (Path.to_string path ^ " changed")
      | Event_queue_overflow -> Some "Event queue overflow; full rebuild required"
      | Upgrade -> Some "Dune upgrader initiated a full rebuild"
      | Test -> Some "Rebuild initiated by an internal testsuite"
      | Variable_changed v -> Some (sprintf "Variable %s changed" v)
    ;;

    let equal a b =
      match a, b with
      | Unknown, Unknown
      | Event_queue_overflow, Event_queue_overflow
      | Upgrade, Upgrade
      | Test, Test -> true
      | Path_changed a, Path_changed b -> Path.equal a b
      | Variable_changed a, Variable_changed b -> String.equal a b
      | ( ( Unknown
          | Path_changed _
          | Event_queue_overflow
          | Upgrade
          | Test
          | Variable_changed _ )
        , _ ) -> false
    ;;
  end

  module Leaf = struct
    type kind =
      | Invalidate_node : _ Dep_node.t -> kind
      | Clear_cache : ('input, ('input, 'output) Dep_node.t) Store.t -> kind
      | Clear_caches
      | Custom of (unit -> unit)

    type t =
      { kind : kind
      ; reason : Reason.t
      }

    let to_string_hum { reason; _ } = Reason.to_string_hum reason
  end

  module T = struct
    (* Represented as a tree mainly to get a tail-recursive execution. *)
    type t =
      | Empty
      | Leaf of Leaf.t
      | Combine of t * t

    let empty : t = Empty

    let combine a b =
      match a, b with
      | Empty, x | x, Empty -> x
      | x, y -> Combine (x, y)
    ;;
  end

  include T
  include (Monoid.Make (T) : Monoid.S with type t := t)

  let execute_leaf { Leaf.kind; _ } =
    match kind with
    | Invalidate_node dep_node -> invalidate_dep_node dep_node
    | Clear_cache store -> invalidate_store store
    | Clear_caches -> Caches.clear ()
    | Custom f -> f ()
  ;;

  let rec execute x xs =
    match x with
    | Empty -> execute_list xs
    | Leaf f ->
      execute_leaf f;
      execute_list xs
    | Combine (x, y) -> execute x (y :: xs)

  and execute_list = function
    | [] -> ()
    | x :: xs -> execute x xs
  ;;

  let rec to_list_x_xs x xs acc =
    match x with
    | Empty -> to_list_xs xs acc
    | Leaf f -> to_list_xs xs (f :: acc)
    | Combine (x, y) -> to_list_x_xs x (y :: xs) acc

  and to_list_xs xs acc =
    match xs with
    | [] -> acc
    | x :: xs -> to_list_x_xs x xs acc
  ;;

  let to_list t = to_list_x_xs t [] []

  let details_hum ?(max_elements = 1) t =
    assert (max_elements > 0);
    let details =
      List.filter_map ~f:Leaf.to_string_hum (to_list t)
      |> String.Set.of_list
      |> String.Set.to_list
    in
    (* CR-someday amokhov: Right now we just take first [max_elements] elements
       from the sorted list, but we could prioritise some reasons over others,
       e.g. if there is a global reset because of [Event_queue_overflow], it may
       be better to ensure that this reason is included. *)
    match List.truncate ~max_length:max_elements details with
    | `Not_truncated [] ->
      [ "Restarting for an unknown reason, please report it as a bug" ]
    | `Not_truncated details -> details
    | `Truncated truncated_details ->
      let extra_message =
        let remaining_details = List.length details - max_elements in
        let plural =
          match remaining_details > 1 with
          | true -> "s"
          | false -> ""
        in
        sprintf ", and %d more change%s" remaining_details plural
      in
      (match List.destruct_last truncated_details with
       | None -> assert false
       | Some (all_but_last, last) -> all_but_last @ [ last ^ extra_message ])
  ;;

  let changed_paths t =
    List.filter_map (to_list t) ~f:(fun ({ Leaf.reason; _ } : Leaf.t) ->
      match reason with
      | Path_changed path -> Some path
      | Unknown | Event_queue_overflow | Upgrade | Test | Variable_changed _ -> None)
    |> Path.Set.of_list
    |> Path.Set.to_list
  ;;

  let execute x = execute x []

  let is_empty = function
    | Empty -> true
    | _ -> false
  ;;

  let clear_caches ~reason = Leaf { kind = Clear_caches; reason }

  let invalidate_cache ~reason ({ cache; _ } : _ Table.t) =
    Leaf { kind = Clear_cache cache; reason }
  ;;

  let invalidate_node ~reason (dep_node : _ Dep_node.t) =
    Leaf { kind = Invalidate_node dep_node; reason }
  ;;

  (* For out-of-band caching mechanisms that need to run an arbitrary action when the build
     is invalidated. *)
  let custom ~reason ~f = Leaf { kind = Custom f; reason }

  let to_reason_list t =
    List.fold_left (to_list t) ~init:[] ~f:(fun acc ({ Leaf.reason; _ } : Leaf.t) ->
      if List.mem acc reason ~equal:Reason.equal then acc else reason :: acc)
    |> List.rev
  ;;
end

module Current_run = struct
  let f () = Run.current () |> return
  let memo = create "current-run" ~input:(module Unit) f
  let exec () = exec memo ()
  let invalidate ~reason = Invalidation.invalidate_node ~reason (dep_node memo ())
end

let current_run () = Current_run.exec ()

let of_non_reproducible_fiber fiber =
  let* (_ : Run.t) = current_run () in
  fiber
;;

let is_top_level =
  let+ is_set = Error_handler.is_set in
  not is_set
;;

let run_with_error_handler t ~handle_error_no_raise =
  Error_handler.with_error_handler handle_error_no_raise (fun () ->
    let* res = Exec.report_and_collect_errors t in
    match res with
    | Ok ok -> Fiber.return ok
    | Error ({ exns; reproducible = _ } : Collect_errors_monoid.t) ->
      Fiber.reraise_all (Exn_set.to_list exns))
;;

let run t =
  let* is_top_level = is_top_level in
  (* CR-someday aalekseyev: I think this automagical detection of toplevel calls
     is weird. My hunch is that having separate functions for toplevel and
     non-toplevel [run] would be better. *)
  match is_top_level with
  | true ->
    (* Start each top-level run with a clean cycle-detection state so that a cycle hit in
       one run does not leak into independent later runs. In production Memo performs a
       single run per build with a [reset] in between, so this matches resetting in
       [reset]. (Concurrent top-level runs do not occur in Dune; if that ever changes,
       this reset would need to be scoped to the individual run rather than shared.) *)
    Call_stack.reset_cycle_error_in_the_current_run ();
    run_with_error_handler
      (fun () -> t)
      ~handle_error_no_raise:(fun _exn -> Fiber.return ())
  | false -> t
;;

module With_implicit_output = struct
  type ('i, 'o) t = 'i -> 'o Fiber.t

  let create name ~input ~implicit_output impl =
    let memo =
      create name ~input (fun i ->
        Implicit_output.collect implicit_output (fun () -> impl i))
    in
    fun input ->
      let* res, output = exec memo input in
      let+ () = Implicit_output.produce_opt implicit_output output in
      res
  ;;

  let exec t = t
end

module Cell = struct
  type ('i, 'o) t = ('i, 'o) Dep_node.t

  let input (t : (_, _) t) = t.input
  let read = Exec.exec_dep_node
  let invalidate = Invalidation.invalidate_node
end

let cell = dep_node

module Implicit_output = Implicit_output

let lazy_cell ?cutoff ?name ?human_readable_description ?on_event f =
  let on_event = Option.map on_event ~f:(fun on_event () event -> on_event event) in
  let spec =
    Spec.create ~name ~input:(module Unit) ~cutoff ~human_readable_description ?on_event f
  in
  make_dep_node ~spec ~input:()
;;

let push_stack_frame ~human_readable_description f =
  Cell.read (lazy_cell ~human_readable_description f)
;;

module Lazy = struct
  type 'a t = unit -> 'a Fiber.t

  let of_val a () = Fiber.return a

  module Expert = struct
    let create ?cutoff ?name ?human_readable_description ?on_event f =
      let cell = lazy_cell ?cutoff ?name ?human_readable_description ?on_event f in
      cell, fun () -> Cell.read cell
    ;;
  end

  let create ?cutoff ?name ?human_readable_description ?on_event f =
    let cell = lazy_cell ?cutoff ?name ?human_readable_description ?on_event f in
    fun () -> Cell.read cell
  ;;

  let force f = f ()
  let map t ~f = create (fun () -> Fiber.map ~f (t ()))
end

let lazy_ = Lazy.create

module Poly (Function : sig
    type 'a input
    type 'a output

    val name : string
    val id : 'a input -> 'a Type_eq.Id.t
    val to_dyn : _ input -> Dyn.t
    val eval : 'a input -> 'a output Fiber.t
  end) =
struct
  open Function

  module Key = struct
    type t = T : _ input -> t

    let to_dyn (T t) = to_dyn t
    let hash (T t) = Type_eq.Id.hash (id t)
    let equal (T x) (T y) = Type_eq.Id.equal (id x) (id y)
  end

  module Value = struct
    type t = T : ('a Type_eq.Id.t * 'a output) -> t

    let get (type a) ~(input_with_matching_id : a input) value : a output =
      match value with
      | T (id_v, res) ->
        (match Type_eq.Id.same id_v (id input_with_matching_id) with
         | None ->
           Code_error.raise
             "Type_eq.Id.t mismatch in Memo.Poly: the likely reason is that the provided \
              Function.id returns different ids for the same input."
             [ "Function.name", Dyn.String name ]
         | Some Type_eq.T -> res)
    ;;
  end

  let memo =
    create
      name
      ~input:(module Key)
      (function Key.T input -> eval input >>| fun v -> Value.T (id input, v))
  ;;

  let eval x = exec memo (Key.T x) >>| Value.get ~input_with_matching_id:x
end

let reset invalidation =
  (* We rely on [invalidation] to list the actual reasons for the reset, which
     justifies the [~reason:Unknown] below. *)
  let invalidate_current_run = Current_run.invalidate ~reason:Unknown in
  Invalidation.execute (Invalidation.combine invalidation invalidate_current_run);
  Call_stack.reset_cycle_error_in_the_current_run ();
  Run.restart ()
;;

(* Like [reset], but a no-op when nothing can have changed: the [invalidation] is empty and
   no non-reproducible error was computed in the current run. This lets a caller (e.g. a
   file-watcher waking up with no relevant changes) avoid advancing the run and discarding
   the whole cache. *)
let reset_if_necessary invalidation =
  if
    (not (Invalidation.is_empty invalidation))
    || Run.is_current !Exec.last_saw_non_reproducible_exn_at
  then reset invalidation
;;

module For_tests = struct
  let get_deps (type i o) (t : (i, o) Table.t) inp =
    match Store.find t.cache inp with
    | None -> None
    | Some dep_node ->
      (match get_cached_deps_in_current_run dep_node with
       | None -> None
       | Some deps ->
         Some
           (Deps.For_debugging.to_list deps
            |> List.map ~f:(fun (Dep_node.T dep) ->
              dep.spec.name, Dep_node.input_to_dyn dep)))
  ;;

  let clear_memoization_caches () = Caches.clear ()
end

module Store = Store_intf

module Run = struct
  type t = Run.t

  module For_tests = struct
    let compare = Run.compare
    let current = Run.current
  end
end

module type S = sig
    type 'a memo = 'a t

    include Monad.S
    module List : Monad.List with type 'a t := 'a t

    val of_memo : 'a memo -> 'a t
  end
  with type 'a memo := 'a t

let of_memo = Fun.id

module List = struct
  include Monad.List (Fiber)

  let map = parallel_map
  let concat_map l ~f = map l ~f >>| List.concat
end

module Option = Monad.Option (Fiber)
module Result = Monad.Result (Fiber)

module Var = struct
  (* CR-soon amokhov: Simplify this to [type 'a t = (unit, 'a) Cell.t].

     [Cell.t]s already store all the information we need, and the only change that needs
     to happen is making [Cell.invalidate] smart enough to return [Invalidation.empty]
     when the [cutoff] fires.

     Once we have that, we should also be able to implement [Fs_memo] on top of [Var.t]s
     instead of hand-written "variable tables".
  *)
  type 'a t =
    { cell : (unit, 'a) Cell.t
    ; value : 'a ref
      (* We manually cutoff instead of depending on [Cell.t] cutoff mechanism, so that
           we don't pay for invalidation when the value doesn't change. *)
    ; cutoff : ('a -> 'a -> bool) option
    }

  let create (type a) ?cutoff value ~name : a t =
    let value = ref value in
    let spec =
      Spec.create
        ~name:(Some name)
        ~input:(module Stdune.Unit)
        ~human_readable_description:None
        ~cutoff:None
        (fun () -> return !value)
    in
    let cell = make_dep_node ~spec ~input:() in
    { cell; value; cutoff }
  ;;

  let set t v =
    match t.cutoff with
    | Some cutoff when cutoff !(t.value) v ->
      (* Note: We do *not* set [t.value := v] when the change is insignificant according
         to the [cutoff]. This is consistent with how cutoffs work in the rest of Memo,
         e.g., see the [compute] and [update_value] functions.

         This prevents the "cutoff creep" where, e.g., the [cutoff] returns [true] for
         changes within 1% of the current [float] value and a sequence of smaller changes
         goes unnoticed, even when the aggregate change far exceeds the 1% threshold. *)
      Invalidation.empty
    | Some _ | None ->
      t.value := v;
      Cell.invalidate
        t.cell
        ~reason:(Variable_changed (Stdune.Option.value_exn t.cell.spec.name))
  ;;

  let read t = Cell.read t.cell

  module Unit = struct
    type t = (unit, unit) Cell.t

    (* All [unit]-valued variables share a single spec; they only differ by node identity,
       which is what invalidation acts on. *)
    let spec =
      Spec.create
        ~name:(Some "Memo.Var.Unit")
        ~input:(module Stdune.Unit)
        ~human_readable_description:None
        ~cutoff:None
        (fun () -> return ())
    ;;

    let create () : t = make_dep_node ~spec ~input:()
    let invalidate t ~reason = Cell.invalidate t ~reason
    let read t = Cell.read t
  end
end
