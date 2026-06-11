module Metrics0 = Metrics
module Table0 = Table
open Stdune
module Metrics = Metrics0
module Table = Table0
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

  include Stdune.Table.Key with type t := t
end

(* We can get rid of this once we use the memoization system more pervasively
   and all the dependencies are properly specified *)

exception Non_reproducible = Exec.Non_reproducible

open Node
module Error = Node.Error
module Cycle_error = Node.Cycle_error

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
module Map = Make_parallel_map

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
    match of_.spec.witness with
    | None -> None
    | Some witness -> Dep_node.Packed.as_instance_of stack_frame witness
  ;;
end

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
    Spec.create
      ~name:(Some name)
      ~witness:true
      ~input
      ~cutoff
      ~human_readable_description
      ?on_event
      f
  in
  Caches.register ~clear:(fun () ->
    Store.clear cache;
    Invalidation.invalidate_store cache);
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
  ; runs = Run.Pair.invalid
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

let dump_cached_graph ?(on_not_cached = `Raise) ?(time_nodes = false) node =
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
    (fun () -> collect_graph (Dep_node.T node) Graph.empty)
;;

let get_call_stack = Call_stack.get_call_stack_without_state

module Invalidation = Invalidation

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

module Node = struct
  type ('i, 'o) t = ('i, 'o) Dep_node.t

  let input (t : (_, _) t) = t.input
  let read = Exec.exec_dep_node
  let invalidate = Invalidation.invalidate_node
end

let node = dep_node

module Implicit_output = Implicit_output

let lazy_node ?cutoff ?name ?human_readable_description ?on_event f =
  let on_event = Option.map on_event ~f:(fun on_event () event -> on_event event) in
  let spec =
    Spec.create ~name ~input:(module Unit) ~cutoff ~human_readable_description ?on_event f
  in
  make_dep_node ~spec ~input:()
;;

let push_stack_frame ~human_readable_description f =
  Node.read (lazy_node ~human_readable_description f)
;;

module Lazy = struct
  type 'a t = unit -> 'a Fiber.t

  let of_val a () = Fiber.return a

  module Expert = struct
    let create ?cutoff ?name ?human_readable_description ?on_event f =
      let node = lazy_node ?cutoff ?name ?human_readable_description ?on_event f in
      node, fun () -> Node.read node
    ;;
  end

  let create ?cutoff ?name ?human_readable_description ?on_event f =
    let node = lazy_node ?cutoff ?name ?human_readable_description ?on_event f in
    fun () -> Node.read node
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

  let get_deps_structured (node : (_, _) Node.t) =
    match get_cached_deps_in_current_run node with
    | None -> None
    | Some deps ->
      Some
        (Deps.For_debugging.to_dyn
           (fun (Dep_node.T dep) ->
              Dyn.Tuple [ Dyn.option Dyn.string dep.spec.name; Dep_node.input_to_dyn dep ])
           deps)
  ;;

  let clear_memoization_caches () = Caches.clear ()
end

module Store = Store_intf

module Run = struct
  type t = Run.t

  module For_tests = struct
    let compare = Run.compare
    let current = Run.current
    let of_int = Run.For_testing.of_int
    let to_int = Run.For_testing.to_int

    module Pair = Run.Pair
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
  (* CR-soon amokhov: Simplify this to [type 'a t = (unit, 'a) Node.t].

     [Node.t]s already store all the information we need, and the only change that needs
     to happen is making [Node.invalidate] smart enough to return [Invalidation.empty]
     when the [cutoff] fires.

     Once we have that, we should also be able to implement [Fs_memo] on top of [Var.t]s
     instead of hand-written "variable tables".
  *)
  type 'a t =
    { node : (unit, 'a) Node.t
    ; value : 'a ref
      (* We manually cutoff instead of depending on [Node.t] cutoff mechanism, so that
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
    let node = make_dep_node ~spec ~input:() in
    { node; value; cutoff }
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
      Node.invalidate
        t.node
        ~reason:(Variable_changed (Stdune.Option.value_exn t.node.spec.name))
  ;;

  let read t = Node.read t.node

  module Unit = struct
    type t = (unit, unit) Node.t

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
    let invalidate t ~reason = Node.invalidate t ~reason
    let read t = Node.read t
  end
end
