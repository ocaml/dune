open! Stdune
open Fiber.O

let track_locations_of_lazy_values = ref false

type 'a build = 'a Fiber.t

module type Build = sig
  include Monad

  module List : sig
    val map : 'a list -> f:('a -> 'b t) -> 'b list t
  end

  val memo_build : 'a build -> 'a t
end

module Build = struct
  include Fiber

  let if_ x y =
    match x with
    | true -> y ()
    | false -> return ()

  let run = Fun.id

  let of_reproducible_fiber = Fun.id

  module Option = struct
    let iter option ~f =
      match option with
      | None -> return ()
      | Some a -> f a
  end

  module Result = struct
    let iter result ~f =
      match result with
      | Error _ -> return ()
      | Ok a -> f a
  end

  module List = struct
    let map = parallel_map
  end

  let memo_build = Fun.id
end

let unwrap_exn = ref Fun.id

module Allow_cutoff = struct
  type 'o t =
    | No
    | Yes of ('o -> 'o -> bool)
end

module type Output_simple = sig
  type t

  val to_dyn : t -> Dyn.t
end

module type Output_allow_cutoff = sig
  type t

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool
end

module type Input = sig
  type t

  include Table.Key with type t := t
end

module Output = struct
  type 'o t =
    | Simple of (module Output_simple with type t = 'o)
    | Allow_cutoff of (module Output_allow_cutoff with type t = 'o)

  let simple (type a) ?to_dyn () =
    let to_dyn = Option.value to_dyn ~default:(fun _ -> Dyn.Opaque) in
    Simple
      (module struct
        type t = a

        let to_dyn = to_dyn
      end)

  let allow_cutoff (type a) ?to_dyn ~(equal : a -> a -> bool) =
    let to_dyn = Option.value to_dyn ~default:(fun _ -> Dyn.Opaque) in
    Allow_cutoff
      (module struct
        type t = a

        let to_dyn = to_dyn

        let equal = equal
      end)

  let create ?cutoff ?to_dyn () =
    match cutoff with
    | None -> simple ?to_dyn ()
    | Some equal -> allow_cutoff ?to_dyn ~equal
end

module Visibility = struct
  type 'i t =
    | Hidden
    | Public of 'i Dune_lang.Decoder.t
end

module Exn_comparable = Comparable.Make (struct
  type t = Exn_with_backtrace.t

  let compare { Exn_with_backtrace.exn; backtrace = _ } (t : t) =
    Poly.compare (!unwrap_exn exn) (!unwrap_exn t.exn)

  let to_dyn = Exn_with_backtrace.to_dyn
end)

module Exn_set = Exn_comparable.Set

module Info = struct
  type t =
    { name : string
    ; doc : string option
    }
end

module Spec = struct
  type ('i, 'o) t =
    { info : Info.t option
    ; input : (module Store_intf.Input with type t = 'i)
    ; output : (module Output_simple with type t = 'o)
    ; allow_cutoff : 'o Allow_cutoff.t
    ; decode : 'i Dune_lang.Decoder.t
    ; witness : 'i Type_eq.Id.t
    ; f : 'i -> 'o Fiber.t
    }

  type packed = T : (_, _) t -> packed [@@unboxed]

  (* This mutable table is safe under the assumption that [register] is called
     only at the top level, which is currently true. This means that all
     memoization tables created not at the top level are hidden. *)
  let by_name : packed String.Table.t = String.Table.create 256

  let find name = String.Table.find by_name name

  let register t =
    match t.info with
    | None -> Code_error.raise "[Spec.register] got a function with no info" []
    | Some info -> (
      match find info.name with
      | Some _ ->
        Code_error.raise
          "[Spec.register] called twice on a function with the same name"
          [ ("name", Dyn.String info.name) ]
      | None -> String.Table.set by_name info.name (T t))

  let create (type o) ~info ~input ~visibility ~(output : o Output.t) ~f =
    let info =
      match info with
      | None when !track_locations_of_lazy_values ->
        Option.map
          (Caller_id.get ~skip:[ __FILE__ ])
          ~f:(fun loc ->
            let name =
              sprintf "lazy value created at %s" (Loc.to_file_colon_line loc)
            in
            { Info.name; doc = None })
      | _ -> info
    in
    let (output : (module Output_simple with type t = o)), allow_cutoff =
      match output with
      | Simple (module Output) -> ((module Output), Allow_cutoff.No)
      | Allow_cutoff (module Output) -> ((module Output), Yes Output.equal)
    in
    let decode =
      match visibility with
      | Visibility.Public decode -> decode
      | Hidden ->
        let open Dune_lang.Decoder in
        let+ loc = loc in
        User_error.raise ~loc [ Pp.text "<not-implemented>" ]
    in
    { info
    ; input
    ; output
    ; allow_cutoff
    ; decode
    ; witness = Type_eq.Id.create ()
    ; f
    }
end

module Id = Id.Make ()

(* We can get rid of this once we use the memoization system more pervasively
   and all the dependencies are properly specified *)
module Caches = struct
  let cleaners = ref []

  let register ~clear = cleaners := clear :: !cleaners

  let clear () = List.iter !cleaners ~f:(fun f -> f ())
end

module Dep_node_without_state = struct
  type ('i, 'o) t =
    { spec : ('i, 'o) Spec.t
    ; input : 'i
    ; id : Id.t
    }

  type packed = T : (_, _) t -> packed [@@unboxed]
end

let ser_input (type i) (node : (i, _) Dep_node_without_state.t) =
  let (module Input : Store_intf.Input with type t = i) = node.spec.input in
  Input.to_dyn node.input

module Stack_frame_without_state = struct
  open Dep_node_without_state

  type t = Dep_node_without_state.packed

  let name (T t) = Option.map t.spec.info ~f:(fun x -> x.name)

  let input (T t) = ser_input t

  let to_dyn t =
    Dyn.Tuple
      [ String
          (match name t with
          | Some name -> name
          | None -> "<unnamed>")
      ; input t
      ]
end

module Cycle_error = struct
  type t =
    { cycle : Stack_frame_without_state.t list
    ; stack : Stack_frame_without_state.t list
    }

  exception E of t

  let get t = t.cycle

  let stack t = t.stack
end

(* A value calculated during a "sample attempt". A sample attempt can fail for
   two reasons:

   - [Error]: the user-supplied function that was called to compute the value
   raised one or more exceptions, recorded in the [Exn_set.t].

   - [Cancelled]: the attempt was cancelled due to a dependency cycle.

   Note that we plan to make [Cancelled] more general and store the reason for
   cancellation: a dependency cycle or a request to cancel the current run. *)
module Value = struct
  type 'a t =
    | Ok of 'a
    | Error of Exn_set.t
    | Cancelled of { dependency_cycle : Cycle_error.t }

  let get_exn = function
    | Ok a -> Fiber.return a
    | Error exns -> Fiber.reraise_all (Exn_set.to_list exns)
    | Cancelled { dependency_cycle } -> raise (Cycle_error.E dependency_cycle)
end

module Dag : Dag.S with type value := Dep_node_without_state.packed =
Dag.Make (struct
  type t = Dep_node_without_state.packed
end)

(* [Value_id] is an identifier allocated every time a node value is computed and
   found to be different from before.

   The clients then use [Value_id] to see if the value have changed since the
   previous value they observed. This means we don't need to run the cutoff
   comparison at every client.

   There is a downside, though: if the value changes from x to y, and then back
   to x, then the [Value_id] changes without the value actually changing, which
   is a shame. So we should test and see if [Value_id] is worth keeping or it's
   better to just evaluate the cutoff multiple times. *)
module Value_id : sig
  type t

  val create : unit -> t

  val equal : t -> t -> bool

  val to_dyn : t -> Dyn.t
end = struct
  type t = int

  let to_dyn = Int.to_dyn

  let next = ref 0

  let create () =
    let res = !next in
    next := res + 1;
    res

  let equal = Int.equal
end

let _ = Value_id.to_dyn

(* Dependencies of a value accumulated so far. All of them are added to the DAG
   of sample attempts to detect cycles.

   We start accumulating dependencies during the [restore_from_cache] step. Some
   of them may turn out to be "phantom", i.e. we used to depend on them but do
   not depend on them anymore. This happens when the [restore_from_cache] step
   fails and we fall back to recomputing the value, which can lead to a new set
   of dependencies.

   Once we move to the [compute] step, we start accumulating actual dependencies
   of the value in [compute_deps]. The list of dependencies is stored in the
   reversed order for efficient updates.

   After the [compute] step has finished, [compute_deps] are stored in the
   resulting [Cached_value.t]. *)
module Deps_so_far = struct
  type 'node deps =
    | Compute_not_started
    | Compute_started of { deps_reversed : 'node list }

  (* Some of the [added_to_dag] nodes also need to be added to [compute_deps]. *)
  type status = { added_to_compute_deps : bool } [@@unboxed]

  type 'node t =
    { mutable added_to_dag : status Id.Map.t
    ; mutable compute_deps : 'node deps
    }

  let create () =
    { added_to_dag = Id.Map.empty; compute_deps = Compute_not_started }

  let start_compute t = t.compute_deps <- Compute_started { deps_reversed = [] }

  (* Add a new dependency [node] to [added_to_dag] and also to [compute_deps] if
     [Compute_started] and the dependency hasn't been added before. *)
  let add_dep t node_id node =
    t.added_to_dag <-
      Id.Map.update t.added_to_dag node_id ~f:(fun status ->
          match (t.compute_deps, status) with
          | Compute_not_started, _ -> Some { added_to_compute_deps = false }
          | _, Some { added_to_compute_deps } when added_to_compute_deps ->
            status
          | Compute_started { deps_reversed }, _ ->
            t.compute_deps <-
              Compute_started { deps_reversed = node :: deps_reversed };
            Some { added_to_compute_deps = true })

  let get_compute_deps_rev t =
    match t.compute_deps with
    | Compute_not_started ->
      Code_error.raise
        "get_compute_deps_rev called in the Compute_not_started state" []
    | Compute_started { deps_reversed } -> deps_reversed
end

module Cache_lookup = struct
  (* Looking up a value cached in a previous run can fail in three possible
     ways:

     - [Not_found]: either the value has never been computed before, or the last
     computation attempt failed.

     - [Out_of_date]: we found a value computed in a previous run but it is out
     of date because one of its dependencies changed; we return the old value so
     that it can be compared with a new one to support the early cutoff.

     - [Cancelled _]: the cache lookup has been cancelled because of a
     dependency cycle. This outcome indicates that a dependency cycle has been
     introduced in the current run. If a cycle existed in a previous run, the
     outcome would have been [Not_found] instead. *)
  module Failure = struct
    type 'a t =
      | Not_found
      | Out_of_date of 'a
      | Cancelled of { dependency_cycle : Cycle_error.t }
  end

  module Result = struct
    type 'a t = ('a, 'a Failure.t) result
  end
end

(* A fiber that can be shared but is still computed at most once. An equivalent
   of ['a Lazy.t] but for asynchronous computations. *)
module Once = struct
  type 'a state =
    | Not_forced of { must_not_raise : unit -> 'a Fiber.t }
    | Forced of 'a Fiber.Ivar.t

  type 'a t = { mutable state : 'a state }

  (* If a given thunk does in fact raise an exception, forcing it will propagate
     the exception to the first caller, and leave all subsequent callers stuck,
     forever waiting for the unfilled [Ivar.t]. *)
  let create ~must_not_raise = { state = Not_forced { must_not_raise } }

  (* Note that side effects of the shared fiber will happen only when the fiber
     returned by [force t] is executed. *)
  let force t =
    Fiber.of_thunk (fun () ->
        match t.state with
        | Forced ivar ->
          let+ res = Fiber.Ivar.read ivar in
          res
        | Not_forced { must_not_raise } ->
          let ivar = Fiber.Ivar.create () in
          t.state <- Forced ivar;
          let* result = must_not_raise () in
          let+ () = Fiber.Ivar.fill ivar result in
          result)

  (* Like a monadic bind but [f_must_not_raise] returns a [Fiber.t], not a [t]. *)
  let and_then t ~f_must_not_raise =
    create ~must_not_raise:(fun () -> Fiber.bind (force t) ~f:f_must_not_raise)
end

(* An attempt to sample the current value of a node. It's an "attempt" because
   it can fail due to a dependency cycle.

   A sample attempt begins its life in the [Running] state. Multiple readers can
   concurrently subscribe to the (possibly future) [result] of the attempt using
   the [restore] and [compute] functions. If the attempt succeeds, it goes to
   the [Finished] state.

   To detect dependency cycles, we maintain a DAG of [Running] sample attempts.
   [Finished] attempts do not need to be in the DAG but currently they remain
   there until the end of the current run. When the run completes, the DAG is
   garbage collected because we no longer hold any references to its nodes. *)
module Sample_attempt = struct
  module Result = struct
    type 'a t =
      { restore_from_cache : 'a Cache_lookup.Result.t Once.t
      ; compute : 'a Once.t
      }
  end

  type 'a t =
    | Finished of 'a
    | Running of
        { dag_node : Dag.node
        ; result : 'a Result.t
        }

  let restore = function
    | Finished cached_value -> Fiber.return (Ok cached_value)
    | Running { result; _ } -> Once.force result.restore_from_cache

  let compute = function
    | Finished cached_value -> Fiber.return cached_value
    | Running { result; _ } -> Once.force result.compute
end

module M = struct
  module rec Cached_value : sig
    type 'a t =
      { value : 'a Value.t
      ; (* The value id, used to check that the value is the same. *)
        id : Value_id.t
      ; (* When was last computed or confirmed unchanged *)
        mutable last_validated_at : Run.t
      ; (* The values stored in [deps] must have been calculated at
           [last_validated_at] too.

           In fact the set of deps can change over the lifetime of
           [Cached_value] even if the [value] and [id] do not change. This can
           happen if the value gets re-computed, but a cutoff prevents us from
           updating the value id.

           Note that [deps] should be listed in the order in which they were
           depended on to avoid recomputations of the dependencies that are no
           longer relevant (see an example below). [Async] functions induce a
           partial (rather than a total) order on dependencies, and so [deps]
           should be a linearisation of this partial order. It is also worth
           noting that the problem only occurs with dynamic dependencies,
           because static dependencies can never become irrelevant.

           As an example, consider the function [let f x = let y = g x in h y].
           The correct order of dependencies of [f 0] is [g 0] and then [h y1],
           where [y1] is the result of computing [g 0] in the first build run.
           Now consider the situation where (i) [h y1] is incorrectly listed
           first in [deps], and (ii) both [g] and [h] have changed in the second
           build run (e.g. because they read modified files). To determine that
           [f] needs to be recomputed, we start by recomputing [h y1], which is
           likely to be a waste because now we are really interested in [h y2],
           where [y2] is the result of computing [g 0] in the second run. Had we
           listed [g 0] first, we would recompute it and the work wouldn't be
           wasted since [f 0] does depend on it.

           aalekseyev: now we have a more stringent requirement: [deps] must be
           a linearization of dependency causality order, otherwise the
           validation algorithm may create spurious dependency cycles. *)
        mutable deps : Last_dep.t list
      }
  end =
    Cached_value

  and Running_state : sig
    type t =
      { deps_so_far : Dep_node.packed Deps_so_far.t
      ; dag_node : Dag.node
      }
  end =
    Running_state

  (* Why do we store a [run] in the [Considering] state?

     It is possible for a computation to remain in the [Considering] state after
     the current run is complete. This happens if the [restore_from_cache] step
     fails, and the parent node recreates all dependencies from scratch in the
     subsequent [compute] step. We call computations that become stuck in the
     [Considering] state "stale computations".

     To distinguish between "current" and "stale" computations, we store the
     [run] in which the computation had started. In this way, before subscribing
     to a [sample_attempt_result], we can check if it corresponds to the current
     run, and if not, restart the sample attempt from scratch. This is what the
     function [currently_considering] does.

     Once all stale computations have been restarted, we should hold no more
     references to the corresponding [sample_attempt_result]s, allowing them to
     be garbage collected. Note: some stale computations may never be restarted,
     e.g. if they end up getting forever stuck behind an inactive conditional. *)
  and State : sig
    type 'a t =
      (* [Considering] marks computations currently being considered, i.e. whose
         result we currently attempt to restore from the cache or recompute. *)
      | Not_considering
      | Considering of
          { run : Run.t
          ; running : Running_state.t
          ; sample_attempt_result : 'a Cached_value.t Sample_attempt.Result.t
          }
  end =
    State

  and Dep_node : sig
    type ('i, 'o) t =
      { without_state : ('i, 'o) Dep_node_without_state.t
      ; mutable state : 'o State.t
      ; mutable last_cached_value : 'o Cached_value.t option
      }

    type packed = T : (_, _) t -> packed [@@unboxed]
  end =
    Dep_node

  (* We store the [Value_id.t] of the last [Cached_value.t] value we depended on
     to support early cutoff.

     Consider a dependency [T (dep, value_id) : Last_dep.t] of a node [caller].

     If [dep.last_cached_value.id <> value_id] then the early cutoff fails, i.e.
     the value that the caller had previously used has changed and received a
     new identifier, which means the caller needs to be recomputed.

     Note that we can achieve the same early cutoff behaviour by switching to
     storing two runs in each [Cached_value.t] instead of just one:

     - [last_validated_at : Run.t], which we store already, and

     - [last_changed_at : Run.t], which records the run when the value changed
     last time, with the invariant [last_changed_at <= last_validated_at].

     If [dep.last_changed_at > caller.last_validated_at], then the value has
     changed since it had been previously used by the caller, and therefore the
     caller needs to be recomputed. This new condition is equivalent to the
     above condition [dep.last_cached_value.id <> value_id] but doesn't require
     storing value identifiers in [Last_dep.t].

     See Section 5.2.2 of "Build Systems a la Carte: Theory and Practice" for
     more details on this optimisation (it is worth checking out the scenario
     described in Fig. 7).

     Historical remark: previously [Last_dep.t] stored [Value.t] instead of just
     the corresponding [Value_id.t], which means we had to compare the current
     value and the value recorded in [Last_dep] in every run. By switching to
     storing the [Value_id.t], the (potentially expensive) value comparisons
     were replaced with cheap comparisons of their integer identifiers. *)
  and Last_dep : sig
    type t = T : ('a, 'b) Dep_node.t * Value_id.t -> t
  end =
    Last_dep
end

module State = M.State
module Running_state = M.Running_state
module Dep_node = M.Dep_node
module Last_dep = M.Last_dep

let currently_considering (v : _ State.t) : _ State.t =
  match v with
  | Not_considering -> Not_considering
  | Considering { run; _ } as running ->
    if Run.is_current run then
      running
    else
      Not_considering

let get_cached_value_in_current_cycle (dep_node : _ Dep_node.t) =
  match dep_node.last_cached_value with
  | None -> None
  | Some cv ->
    if Run.is_current cv.last_validated_at then
      Some cv
    else
      None

module Cached_value = struct
  include M.Cached_value

  let dump_stack_fdecl = Fdecl.create (fun _ -> Dyn.Opaque)

  let capture_dep_values ~deps_rev =
    List.rev_map deps_rev ~f:(function Dep_node.T dep_node ->
        (match get_cached_value_in_current_cycle dep_node with
        | None ->
          let reason =
            match dep_node.last_cached_value with
            | None -> "(no value)"
            | Some _ -> "(old run)"
          in
          Fdecl.get dump_stack_fdecl ();
          Code_error.raise
            ("Attempted to create a cached value based on some stale inputs "
           ^ reason)
            []
        | Some cv -> Last_dep.T (dep_node, cv.id)))

  let create x ~deps_rev =
    { deps = capture_dep_values ~deps_rev
    ; value = x
    ; last_validated_at = Run.current ()
    ; id = Value_id.create ()
    }

  (* Dependencies of cancelled computations are not accurate, so we store the
     empty list of [deps] in this case. In future, it would be better to
     refactor the code to avoid storing the list altogether in this case. *)
  let create_cancelled ~dependency_cycle =
    { deps = []
    ; value = Cancelled { dependency_cycle }
    ; last_validated_at = Run.current ()
    ; id = Value_id.create ()
    }

  let confirm_old_value t ~deps_rev =
    t.last_validated_at <- Run.current ();
    t.deps <- capture_dep_values ~deps_rev;
    t

  let value_changed (type o) (node : (_, o) Dep_node.t) prev_output curr_output
      =
    match (prev_output, curr_output) with
    | (Value.Error _ | Cancelled _), _ -> true
    | _, (Value.Error _ | Cancelled _) -> true
    | Ok prev_output, Ok curr_output -> (
      match node.without_state.spec.allow_cutoff with
      | Yes equal -> not (equal prev_output curr_output)
      | No -> true)
end

module Stack_frame_with_state = struct
  type ('i, 'o) unpacked =
    { without_state : ('i, 'o) Dep_node_without_state.t
    ; running_state : Running_state.t
    }

  type t = T : ('i, 'o) unpacked -> t

  let to_dyn (T t) = Stack_frame_without_state.to_dyn (T t.without_state)
end

module To_open = struct
  module Stack_frame = Stack_frame_with_state
end

open To_open

let global_dep_dag = Dag.create ()

module Call_stack = struct
  (* The variable holding the call stack for the current context. *)
  let call_stack_var = Fiber.Var.create ()

  let get_call_stack () =
    Fiber.Var.get call_stack_var |> Option.value ~default:[]

  let get_call_stack_without_state () =
    get_call_stack ()
    |> List.map ~f:(fun (Stack_frame_with_state.T t) ->
           Dep_node_without_state.T t.without_state)

  let get_call_stack_tip () = List.hd_opt (get_call_stack ())

  let push_async_frame (frame : Stack_frame_with_state.t) f =
    let stack = get_call_stack () in
    Fiber.Var.set call_stack_var (frame :: stack) (fun () ->
        Implicit_output.forbid_async f)
end

let pp_stack () =
  let open Pp.O in
  let stack = Call_stack.get_call_stack () in
  Pp.vbox
    (Pp.box (Pp.text "Memoized function stack:")
    ++ Pp.cut
    ++ Pp.chain stack ~f:(fun frame -> Dyn.pp (Stack_frame.to_dyn frame)))

let dump_stack () = Format.eprintf "%a" Pp.to_fmt (pp_stack ())

let () = Fdecl.set Cached_value.dump_stack_fdecl dump_stack

(* Add a dependency on the [dep_node] from the caller, if there is one. Returns
   an [Error] if the new dependency would introduce a dependency cycle. *)
let add_dep_from_caller (type i o) (dep_node : (i, o) Dep_node.t)
    (sample_attempt : _ Sample_attempt.t) =
  match Call_stack.get_call_stack_tip () with
  | None -> Ok ()
  | Some (Stack_frame_with_state.T caller) -> (
    let deps_so_far_of_caller = caller.running_state.deps_so_far in
    match
      Id.Map.mem deps_so_far_of_caller.added_to_dag dep_node.without_state.id
    with
    | true ->
      Deps_so_far.add_dep deps_so_far_of_caller dep_node.without_state.id
        (Dep_node.T dep_node);
      Ok ()
    | false -> (
      let cycle_error =
        match sample_attempt with
        | Finished _ -> None
        | Running { dag_node; _ } -> (
          match
            Dag.add_assuming_missing global_dep_dag
              caller.running_state.dag_node dag_node
          with
          | () -> None
          | exception Dag.Cycle cycle ->
            Some
              { Cycle_error.stack = Call_stack.get_call_stack_without_state ()
              ; cycle = List.map cycle ~f:(fun dag_node -> dag_node.Dag.data)
              })
      in
      match cycle_error with
      | None ->
        Deps_so_far.add_dep deps_so_far_of_caller dep_node.without_state.id
          (Dep_node.T dep_node);
        Ok ()
      | Some cycle_error -> Error cycle_error))

type ('input, 'output) t =
  { spec : ('input, 'output) Spec.t
  ; cache : ('input, ('input, 'output) Dep_node.t) Store.t
  }

module Stack_frame = struct
  type ('input, 'output) memo = ('input, 'output) t

  include Stack_frame_without_state

  let as_instance_of (type i) (Dep_node_without_state.T t)
      ~of_:(memo : (i, _) memo) : i option =
    match Type_eq.Id.same memo.spec.witness t.spec.witness with
    | Some Type_eq.T -> Some t.input
    | None -> None
end

let create_with_cache (type i o) name ~cache ?doc ~input ~visibility ~output
    (f : i -> o Fiber.t) =
  let spec =
    Spec.create ~info:(Some { name; doc }) ~input ~output ~visibility ~f
  in
  (match visibility with
  | Public _ -> Spec.register spec
  | Hidden -> ());
  Caches.register ~clear:(fun () -> Store.clear cache);
  { cache; spec }

let create_with_store (type i) name
    ~store:(module S : Store_intf.S with type key = i) ?doc ~input ~visibility
    ~output f =
  let cache = Store.make (module S) in
  create_with_cache name ~cache ?doc ~input ~output ~visibility f

let create (type i) name ?doc ~input:(module Input : Input with type t = i)
    ~visibility ~output f =
  (* This mutable table is safe: the implementation tracks all dependencies. *)
  let cache = Store.of_table (Table.create (module Input) 16) in
  let input = (module Input : Store_intf.Input with type t = i) in
  create_with_cache name ~cache ?doc ~input ~visibility ~output f

let create_hidden name ?doc ~input impl =
  create ~output:(Output.simple ()) ~visibility:Hidden name ?doc ~input impl

let make_dep_node ~spec ~input : _ Dep_node.t =
  let dep_node_without_state : _ Dep_node_without_state.t =
    { id = Id.gen (); input; spec }
  in
  { without_state = dep_node_without_state
  ; last_cached_value = None
  ; state = Not_considering
  }

let dep_node (type i o) (t : (i, o) t) input =
  match Store.find t.cache input with
  | Some dep_node -> dep_node
  | None ->
    let dep_node = make_dep_node ~spec:t.spec ~input in
    Store.set t.cache input dep_node;
    dep_node

(* Checking dependencies of a node can lead to one of these outcomes:

   - [Unchanged]: all the dependencies of the current node are up to date and we
   can therefore skip recomputing the node and can reuse the value computed in
   the previuos run.

   - [Changed]: one of the dependencies has changed since the previous run and
   the current node should therefore be recomputed.

   - [Cycle_error _]: one of the dependencies leads to a dependency cycle. In
   this case, there is no point in recomputing the current node: it's impossible
   to bring its dependencies up to date! *)
module Changed_or_not = struct
  type t =
    | Unchanged
    | Changed
    | Cancelled of { dependency_cycle : Cycle_error.t }
end

module Exec : sig
  (* [exec_dep_node] is a variant of [consider_and_compute] but with a simpler
     type, convenient for external usage. *)
  val exec_dep_node : ('i, 'o) Dep_node.t -> 'o Fiber.t
end = struct
  let rec restore_from_cache (type o)
      (last_cached_value : o Cached_value.t option) :
      o Cached_value.t Cache_lookup.Result.t Fiber.t =
    match last_cached_value with
    | None -> Fiber.return (Error Cache_lookup.Failure.Not_found)
    | Some cached_value -> (
      match cached_value.value with
      | Cancelled _dependency_cycle ->
        (* Dependencies of cancelled computations are not accurate, so we can't
           use [deps_changed] in this case. *)
        Fiber.return (Error Cache_lookup.Failure.Not_found)
      | Error _ ->
        (* We always recompute errors, so there is no point in checking if any
           of their dependencies changed. In principle, we could introduce
           "persistent errors" that are recomputed only when their dependencies
           have changed. *)
        Fiber.return (Error Cache_lookup.Failure.Not_found)
      | Ok _ -> (
        let+ deps_changed =
          let rec go deps =
            match deps with
            | [] -> Fiber.return Changed_or_not.Unchanged
            | Last_dep.T (dep, v_id) :: deps -> (
              match dep.without_state.spec.allow_cutoff with
              | No -> (
                (* If [dep] has no cutoff, it is sufficient to check whether it
                   is up to date. If not, we must recompute [last_cached_value]. *)
                let* restore_result = consider_and_restore_from_cache dep in
                match restore_result with
                | Ok cached_value -> (
                  match Value_id.equal cached_value.id v_id with
                  | true -> go deps
                  | false -> Fiber.return Changed_or_not.Changed)
                | Error (Cancelled { dependency_cycle }) ->
                  Fiber.return (Changed_or_not.Cancelled { dependency_cycle })
                | Error (Not_found | Out_of_date _) ->
                  Fiber.return Changed_or_not.Changed)
              | Yes _equal -> (
                (* If [dep] has a cutoff predicate, it is not sufficient to
                   check whether it is up to date: even if it isn't, after we
                   recompute it, the resulting [Value_id] may remain unchanged,
                   allowing us to skip recomputing [last_cached_value]. *)
                match consider_and_compute dep with
                | Error dependency_cycle ->
                  Fiber.return (Changed_or_not.Cancelled { dependency_cycle })
                | Ok cached_value -> (
                  let* cached_value = cached_value in
                  (* Note that [cached_value.value] will be [Cancelled _] if
                     [dep] itself doesn't introduce a dependency cycle but one
                     of its transitive dependencies does. In this case, the
                     value [id] will be new, so we will take the [false] branch. *)
                  match Value_id.equal cached_value.id v_id with
                  | true -> go deps
                  | false -> Fiber.return Changed_or_not.Changed)))
          in
          go cached_value.deps
        in
        match deps_changed with
        | Unchanged ->
          cached_value.last_validated_at <- Run.current ();
          Ok cached_value
        | Changed -> Error (Cache_lookup.Failure.Out_of_date cached_value)
        | Cancelled { dependency_cycle } ->
          Error (Cancelled { dependency_cycle })))

  and compute (dep_node : _ Dep_node.t) cache_lookup_failure deps_so_far =
    Deps_so_far.start_compute deps_so_far;
    let compute_value_and_deps_rev () =
      (* A consequence of using [Fiber.collect_errors] is that memoized
         functions don't report errors promptly - errors are reported once all
         child fibers terminate. To fix this, we should use
         [Fiber.with_error_handler], but we don't have access to dune's error
         reporting mechanism in memo *)
      let+ res =
        Fiber.collect_errors (fun () ->
            dep_node.without_state.spec.f dep_node.without_state.input)
      in
      let value =
        match res with
        | Ok res -> Value.Ok res
        | Error exns -> Error (Exn_set.of_list exns)
      in
      (value, Deps_so_far.get_compute_deps_rev deps_so_far)
    in
    match cache_lookup_failure with
    | Cache_lookup.Failure.Cancelled { dependency_cycle } ->
      Fiber.return (Cached_value.create_cancelled ~dependency_cycle)
    | Not_found ->
      let+ value, deps_rev = compute_value_and_deps_rev () in
      Cached_value.create value ~deps_rev
    | Out_of_date (old_cv : _ Cached_value.t) -> (
      let+ value, deps_rev = compute_value_and_deps_rev () in
      match Cached_value.value_changed dep_node old_cv.value value with
      | true -> Cached_value.create value ~deps_rev
      | false -> Cached_value.confirm_old_value ~deps_rev old_cv)

  and newly_considering (dep_node : _ Dep_node.t) =
    let dag_node : Dag.node =
      { info = Dag.create_node_info global_dep_dag
      ; data = Dep_node_without_state.T dep_node.without_state
      }
    in
    let running_state : Running_state.t =
      { dag_node; deps_so_far = Deps_so_far.create () }
    in
    let frame : Stack_frame_with_state.t =
      T { without_state = dep_node.without_state; running_state }
    in
    let restore_from_cache =
      Once.create ~must_not_raise:(fun () ->
          Call_stack.push_async_frame frame (fun () ->
              let+ restore_result =
                restore_from_cache dep_node.last_cached_value
              in
              (match restore_result with
              | Ok _ -> dep_node.state <- Not_considering
              | Error _ -> ());
              restore_result))
    in
    let compute =
      Once.and_then restore_from_cache ~f_must_not_raise:(function
        | Ok cached_value -> Fiber.return cached_value
        | Error cache_lookup_failure ->
          Call_stack.push_async_frame frame (fun () ->
              dep_node.last_cached_value <- None;
              let+ cached_value =
                compute dep_node cache_lookup_failure running_state.deps_so_far
              in
              dep_node.last_cached_value <- Some cached_value;
              dep_node.state <- Not_considering;
              cached_value))
    in
    let result : _ Sample_attempt.Result.t = { restore_from_cache; compute } in
    dep_node.state <-
      Considering
        { run = Run.current ()
        ; running = running_state
        ; sample_attempt_result = result
        };
    Sample_attempt.Running { dag_node; result }

  and start_considering (dep_node : _ Dep_node.t) =
    match currently_considering dep_node.state with
    | Not_considering -> (
      match get_cached_value_in_current_cycle dep_node with
      | None -> newly_considering dep_node
      | Some cv -> Finished cv)
    | Considering
        { running = { dag_node; deps_so_far = _ }
        ; sample_attempt_result = result
        ; _
        } ->
      Running { dag_node; result }

  and consider :
        'i 'o.    ('i, 'o) Dep_node.t
        -> ('o Cached_value.t Sample_attempt.t, Cycle_error.t) result =
   fun dep_node ->
    let sample_attempt = start_considering dep_node in
    add_dep_from_caller dep_node sample_attempt
    |> Result.map ~f:(fun () -> sample_attempt)

  and consider_and_compute :
        'i 'o.    ('i, 'o) Dep_node.t
        -> ('o Cached_value.t Fiber.t, Cycle_error.t) result =
   fun dep_node -> Result.map (consider dep_node) ~f:Sample_attempt.compute

  and consider_and_restore_from_cache :
        'i 'o.    ('i, 'o) Dep_node.t
        -> 'o Cached_value.t Cache_lookup.Result.t Fiber.t =
   fun dep_node ->
    match consider dep_node with
    | Ok sample_attempt -> Sample_attempt.restore sample_attempt
    | Error dependency_cycle ->
      Fiber.return (Error (Cache_lookup.Failure.Cancelled { dependency_cycle }))

  let exec_dep_node dep_node =
    Fiber.of_thunk (fun () ->
        match consider_and_compute dep_node with
        | Ok res ->
          let* res = res in
          Value.get_exn res.value
        | Error cycle_error -> raise (Cycle_error.E cycle_error))
end

let exec (type i o) (t : (i, o) t) i = Exec.exec_dep_node (dep_node t i)

let get_deps (type i o) (t : (i, o) t) inp =
  match Store.find t.cache inp with
  | None -> None
  | Some dep_node -> (
    match get_cached_value_in_current_cycle dep_node with
    | None -> None
    | Some cv ->
      Some
        (List.map cv.deps ~f:(fun (Last_dep.T (dep, _value)) ->
             ( Option.map dep.without_state.spec.info ~f:(fun x -> x.name)
             , ser_input dep.without_state ))))

let get_func name =
  match Spec.find name with
  | None -> User_error.raise [ Pp.textf "function %s doesn't exist!" name ]
  | Some spec -> spec

let call name input =
  let (Spec.T spec) = get_func name in
  let (module Output : Output_simple with type t = _) = spec.output in
  let input = Dune_lang.Decoder.parse spec.decode Univ_map.empty input in
  let+ output = spec.f input in
  Output.to_dyn output

let function_info_of_spec (Spec.T spec) =
  match spec.info with
  | Some info -> info
  | None -> Code_error.raise "[function_info_of_spec] got an unnamed spec" []

let registered_functions () =
  String.Table.to_seq_values Spec.by_name
  |> Seq.fold_left
       ~f:(fun xs x -> List.cons (function_info_of_spec x) xs)
       ~init:[]
  |> List.sort ~compare:(fun x y -> String.compare x.Info.name y.Info.name)

let function_info ~name = get_func name |> function_info_of_spec

let get_call_stack = Call_stack.get_call_stack_without_state

(* There are two approaches to invalidating memoization nodes. Currently, when a
   node is invalidated by calling [invalidate_dep_node], only the node itself is
   marked as "changed" (by setting [node.last_cached_value] to [None]). Then,
   the whole graph is marked as "possibly changed" by calling [Run.restart ()],
   which in O(1) time makes all [last_validated_at : Run.t] values out of date.
   In the subsequent computation phase, the whole graph is traversed from top to
   bottom to discover "actual changes" and recompute all the nodes affected by
   these changes. One disadvantage of this approach is that the whole graph
   needs to be traversed even if only a small part of it depends on the set of
   invalidated nodes.

   An alternative approach is as follows. Whenever the [invalidate_dep_node]
   function is called, we recursively mark all of its reverse dependencies as
   "possibly changed". Then, in the computation phase, we only need to traverse
   the marked part of graph (instead of the whole graph as we do currently). One
   disadvantage of this approach is that every node needs to store a list of its
   reverse dependencies, which introduces cyclic memory references and
   complicates garbage collection.

   Is it worth switching from the current approach to the alternative? It's best
   to answer this question by benchmarking. This is not urgent but is worth
   documenting in the code. *)
let invalidate_dep_node (node : _ Dep_node.t) = node.last_cached_value <- None

module Current_run = struct
  let f () = Run.current () |> Build.return

  let memo =
    create "current-run"
      ~input:(module Unit)
      ~output:(Simple (module Run))
      ~visibility:Hidden f

  let exec () = exec memo ()

  let invalidate () = invalidate_dep_node (dep_node memo ())
end

let current_run () = Current_run.exec ()

module Lazy_id = Stdune.Id.Make ()

module With_implicit_output = struct
  type ('i, 'o) t = 'i -> 'o Fiber.t

  let create (type o) name ?doc ~input ~visibility
      ~output:(module O : Output_simple with type t = o) ~implicit_output impl =
    let output =
      Output.simple
        ~to_dyn:(fun (o, _io) ->
          Dyn.List [ O.to_dyn o; Dyn.String "<implicit output is opaque>" ])
        ()
    in
    let memo =
      create name ?doc ~input ~visibility ~output (fun i ->
          Implicit_output.collect_async implicit_output (fun () -> impl i))
    in
    fun input ->
      Fiber.map (exec memo input) ~f:(fun (res, output) ->
          Implicit_output.produce_opt implicit_output output;
          res)

  let exec t = t
end

module Cell = struct
  type ('i, 'o) t = ('i, 'o) Dep_node.t

  let input (t : (_, _) t) = t.without_state.input

  let read (type i o) (dep_node : (i, o) Dep_node.t) =
    Exec.exec_dep_node dep_node

  let invalidate = invalidate_dep_node
end

let cell t inp = dep_node t inp

module Implicit_output = Implicit_output
module Store = Store_intf

let lazy_cell ?cutoff ?to_dyn f =
  let output = Output.create ?cutoff ?to_dyn () in
  let visibility = Visibility.Hidden in
  let spec =
    Spec.create ~info:None ~input:(module Unit) ~output ~visibility ~f
  in
  make_dep_node ~spec ~input:()

let lazy_ ?cutoff ?to_dyn f =
  let cell = lazy_cell ?cutoff ?to_dyn f in
  fun () -> Cell.read cell

module Lazy = struct
  type 'a t = unit -> 'a Fiber.t

  let of_val a () = Fiber.return a

  let create = lazy_

  let force f = f ()

  let map t ~f = create (fun () -> Fiber.map ~f (t ()))
end

module Run = struct
  module Fdecl = struct
    (* [Lazy.t] is the simplest way to create a node in the memoization dag. *)
    type nonrec 'a t = 'a Fdecl.t Lazy.t

    let create to_dyn =
      lazy_ ~to_dyn:Fdecl.to_dyn (fun () ->
          let+ (_ : Run.t) = current_run () in
          Fdecl.create to_dyn)

    let set t x = Lazy.force t >>| fun value -> Fdecl.set value x

    let get t = Lazy.force t >>| Fdecl.get
  end

  include Run
end

module Poly = struct
  module type Function_interface = sig
    type 'a input

    type 'a output

    val name : string

    val id : 'a input -> 'a Type_eq.Id.t

    val to_dyn : _ input -> Dyn.t
  end

  module Mono (F : Function_interface) = struct
    open F

    type key = K : 'a input -> key

    module Key = struct
      type t = key

      let to_dyn (K t) = to_dyn t

      let hash (K t) = Type_eq.Id.hash (id t)

      let equal (K x) (K y) = Type_eq.Id.equal (id x) (id y)
    end

    type value = V : ('a Type_eq.Id.t * 'a output) -> value

    let get (type a) ~value ~(input_with_matching_id : a input) : a output =
      match value with
      | V (id_v, res) -> (
        match Type_eq.Id.same id_v (id input_with_matching_id) with
        | None ->
          Code_error.raise
            "Type_eq.Id.t mismatch in Memo.Poly: the likely reason is that the \
             provided Function.id returns different ids for the same input."
            [ ("Function.name", Dyn.String name) ]
        | Some Type_eq.T -> res)
  end

  module Async (Function : sig
    include Function_interface

    val eval : 'a input -> 'a output Fiber.t
  end) =
  struct
    open Function
    include Mono (Function)

    let impl = function
      | K input -> Fiber.map (eval input) ~f:(fun v -> V (id input, v))

    let memo = create_hidden name ~input:(module Key) impl

    let eval x =
      Fiber.map (exec memo (K x)) ~f:(fun value ->
          get ~value ~input_with_matching_id:x)
  end
end

let should_clear_caches =
  match Sys.getenv_opt "DUNE_WATCHING_MODE_INCREMENTAL" with
  | Some "true" -> false
  | Some "false"
  | None ->
    true
  | Some _ ->
    User_error.raise
      [ Pp.text "Invalid value of DUNE_WATCHING_MODE_INCREMENTAL" ]

let restart_current_run () =
  Current_run.invalidate ();
  Run.restart ()

let reset () =
  restart_current_run ();
  if should_clear_caches then Caches.clear ()
