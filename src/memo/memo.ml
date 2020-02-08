open! Stdune
open Fiber.O
module Function = Function

let on_already_reported = ref Exn_with_backtrace.reraise

module Code_error_with_memo_backtrace = struct
  (* A single memo frame and the OCaml frames it called which lead to the error *)
  type frame =
    { ocaml : string
    ; memo : Dyn.t
    }

  type t =
    { exn : Code_error.t
    ; reverse_backtrace : frame list
    ; (* [outer_call_stack] is a trick to capture some of the information that's
         lost by the async memo error handler. It can be safely ignored by the
         sync error handler. *)
      outer_call_stack : Dyn.t
    }

  type exn += E of t

  let frame_to_dyn { ocaml; memo } =
    Dyn.Record [ ("ocaml", Dyn.String ocaml); ("memo", memo) ]

  let to_dyn { exn; reverse_backtrace; outer_call_stack } =
    Dyn.Record
      [ ("exn", Code_error.to_dyn exn)
      ; ("backtrace", Dyn.Encoder.list frame_to_dyn (List.rev reverse_backtrace))
      ; ("outer_call_stack", outer_call_stack)
      ]

  let () =
    Printexc.register_printer (function
      | E t -> Some (Dyn.to_string (to_dyn t))
      | _ -> None)
end

let already_reported exn = Nothing.unreachable_code (!on_already_reported exn)

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
end

module Visibility = struct
  type 'i t =
    | Hidden
    | Public of 'i Dune_lang.Decoder.t
end

module Spec = struct
  type ('a, 'b, 'f) t =
    { name : Function.Name.t
    ; input : (module Store_intf.Input with type t = 'a)
    ; output : (module Output_simple with type t = 'b)
    ; allow_cutoff : 'b Allow_cutoff.t
    ; decode : 'a Dune_lang.Decoder.t
    ; witness : 'a Type_eq.Id.t
    ; f : ('a, 'b, 'f) Function.t
    ; doc : string option
    }

  type packed = T : (_, _, _) t -> packed [@@unboxed]

  let by_name = Function.Name.Table.create ~default_value:None

  let find name = Function.Name.Table.get by_name name

  let register t =
    match find t.name with
    | Some _ ->
      Code_error.raise "[Spec.register] called twice on the same function" []
    | None -> Function.Name.Table.set by_name ~key:t.name ~data:(Some (T t))

  let create (type o) name ~input ~visibility ~(output : o Output.t) ~f ~doc =
    let name = Function.Name.make name in
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
    { name
    ; input
    ; output
    ; allow_cutoff
    ; decode
    ; witness = Type_eq.Id.create ()
    ; f
    ; doc
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

let reset () =
  Caches.clear ();
  Run.restart ()

module M = struct
  module Generic_dag = Dag

  module rec Cached_value : sig
    type 'a t =
      { data : 'a
      ; (* When was the value calculated. *)
        mutable calculated_at : Run.t
      ; (* The values stored in [deps] must have been calculated at
           [calculated_at] too.

           Note that [deps] should ideally be listed in the order in which they
           were depended on to avoid recomputations of the dependencies that are
           no longer relevant (see an example below). [Async] functions induce a
           partial (rather than a total) order on dependencies, and so [deps]
           should ideally be a linearisation of this partial order. It is also
           worth noting that the problem only occurs with dynamic dependencies,
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
           wasted since [f 0] does depend on it. *)
        deps : Last_dep.t list
      }
  end =
    Cached_value

  and State : sig
    type ('a, 'b, 'f) t =
      (* [Running] includes computations that already terminated with an
         exception or cancelled because we've advanced to the next run.

         [Init] should be treated exactly the same as [Running*] with a stale
         value of [Run.t]. *)
      | Init
      | Running_sync : Run.t -> ('a, 'b, 'a -> 'b) t
      | Running_async : Run.t * 'b Fiber.Ivar.t -> ('a, 'b, 'a -> 'b Fiber.t) t
      | Failed of Run.t * Exn_with_backtrace.t
      | Done of 'b Cached_value.t
  end =
    State

  and Dep_node : sig
    type ('a, 'b, 'f) t =
      { spec : ('a, 'b, 'f) Spec.t
      ; input : 'a
      ; id : Id.t
      ; dag_node : Dag.node
      ; mutable state : ('a, 'b, 'f) State.t
      }

    type packed = T : (_, _, _) t -> packed [@@unboxed]
  end =
    Dep_node

  (* We store the last value ['b] we depended on to support early cutoff. *)
  and Last_dep : sig
    type t = T : ('a, 'b, 'f) Dep_node.t * 'b -> t
  end =
    Last_dep

  and Dag : (Generic_dag.S with type value := Dep_node.packed) =
  Generic_dag.Make (struct
    type t = Dep_node.packed
  end)
end

module State = M.State
module Dep_node = M.Dep_node
module Last_dep = M.Last_dep
module Dag = M.Dag

module Cached_value = struct
  include M.Cached_value

  let create x ~deps = { deps; data = x; calculated_at = Run.current () }

  let dep_changed (type a) (node : (_, a, _) Dep_node.t) prev_output curr_output
      =
    match node.spec.allow_cutoff with
    | Yes equal -> not (equal prev_output curr_output)
    | No -> true

  (* Check if a cached value is up to date. If yes, return it. *)
  let rec get_sync : type a. a t -> a option =
   fun t ->
    if Run.is_current t.calculated_at then
      Some t.data
    else
      let dep_changed = function
        | Last_dep.T (node, prev_output) -> (
          match node.state with
          | Init -> true
          | Failed (run, exn) ->
            if Run.is_current run then
              already_reported exn
            else
              true
          | Running_sync run ->
            if Run.is_current run then
              Code_error.raise
                "Unreported dependency cycle in [Cached_value.get_sync] (this \
                 case should be unreachable)."
                []
            else
              true
          | Running_async _ ->
            Code_error.raise
              "Synchronous function depends on an asynchronous one. This is \
               not allowed (in fact this case should be unreachable)."
              []
          | Done t' -> (
            match get_sync t' with
            | None -> true
            | Some curr_output -> dep_changed node prev_output curr_output ) )
      in
      match List.exists ~f:dep_changed t.deps with
      | true -> None
      | false ->
        t.calculated_at <- Run.current ();
        Some t.data

  (* Check if a cached value is up to date. If yes, return it. *)
  let rec get_async : type a. a t -> a option Fiber.t =
   fun t ->
    if Run.is_current t.calculated_at then
      Fiber.return (Some t.data)
    else
      let rec deps_changed acc = function
        | [] ->
          (* Here is an interesting trade-off!

             Currently we run all jobs in [acc] in parallel, via [parallel_map],
             which may be wasteful in the case of dynamic dependencies -- see
             the comment in [deps : Last_dep.t list].

             A simple alternative is to run all jobs in sequence, respecting the
             dependency order. This never wastes any work but is slow.

             Finally, we could implement a speculative computation primitive
             [parallel_or] that would run all jobs in parallel and terminate as
             soon as any of them evaluates to [true]. This is somewhere in
             between the above alternatives but is more complex. *)
          Fiber.parallel_map acc ~f:Fn.id >>| List.exists ~f:Fn.id
        | Last_dep.T (node, prev_output) :: deps -> (
          match node.state with
          | Init -> Fiber.return true
          | Failed (run, exn) ->
            if Run.is_current run then
              already_reported exn
            else
              Fiber.return true
          | Running_sync _ ->
            Code_error.raise
              "[Running_sync] encountered in [Cached_value.get_async]: this \
               means a synchronous computation is still running and it somehow \
               managed to call an asynchronous one which depended on its \
               results in a cyclic manner."
              []
          | Running_async (run, ivar) ->
            if not (Run.is_current run) then
              Fiber.return true
            else
              let changed =
                let+ curr_output = Fiber.Ivar.read ivar in
                dep_changed node prev_output curr_output
              in
              deps_changed (changed :: acc) deps
          | Done t' ->
            if Run.is_current t'.calculated_at then
              if
                (* handle common case separately to avoid feeding more fibers to
                   [parallel_map] *)
                dep_changed node prev_output t'.data
              then
                Fiber.return true
              else
                deps_changed acc deps
            else
              let changed =
                ( match node.spec.f with
                | Function.Sync _ -> Fiber.return (get_sync t')
                | Function.Async _ -> get_async t' )
                >>| function
                | None -> true
                | Some curr_output -> dep_changed node prev_output curr_output
              in
              deps_changed (changed :: acc) deps )
      in
      deps_changed [] t.deps >>| function
      | true -> None
      | false ->
        t.calculated_at <- Run.current ();
        Some t.data
end

let ser_input (type a) (node : (a, _, _) Dep_node.t) =
  let (module Input : Store_intf.Input with type t = a) = node.spec.input in
  Input.to_dyn node.input

module Stack_frame0 = struct
  open Dep_node

  type t = packed

  let name (T t) = Function.Name.to_string t.spec.name

  let input (T t) = ser_input t

  let equal (T a) (T b) = Id.equal a.id b.id

  let compare (T a) (T b) = Id.compare a.id b.id

  let to_dyn t = Dyn.Tuple [ String (name t); input t ]
end

module To_open = struct
  module Stack_frame = Stack_frame0
end

open To_open

module Cycle_error = struct
  type t =
    { cycle : Stack_frame.t list
    ; stack : Stack_frame.t list
    }

  exception E of t

  let get t = t.cycle

  let stack t = t.stack
end

let global_dep_dag = Dag.create ()

module Call_stack = struct
  (* fiber context variable keys *)
  let call_stack_key = Fiber.Var.create ()

  let get_call_stack () =
    Fiber.Var.get call_stack_key |> Option.value ~default:[]

  let get_call_stack_as_dyn () =
    Dyn.Encoder.list Stack_frame.to_dyn (get_call_stack ())

  let get_call_stack_tip () = List.hd_opt (get_call_stack ())

  let push_async_frame frame f =
    let stack = get_call_stack () in
    Fiber.Var.set call_stack_key (frame :: stack) (fun () ->
        Implicit_output.forbid_async f)

  let push_sync_frame frame f =
    let stack = get_call_stack () in
    Fiber.Var.set_sync call_stack_key (frame :: stack) (fun () ->
        Implicit_output.forbid_sync f)
end

let pp_stack () =
  let open Pp.O in
  let stack = Call_stack.get_call_stack () in
  Pp.vbox
    ( Pp.box (Pp.text "Memoized function stack:")
    ++ Pp.cut
    ++ Pp.chain stack ~f:(fun frame -> Dyn.pp (Stack_frame.to_dyn frame)) )

let dump_stack () = Format.eprintf "%a" Pp.render_ignore_tags (pp_stack ())

(* Add a dependency on [dep_node] from the caller, if there is one.

   CR-someday amokhov: In principle, it's best to always have a caller, perhaps
   a dummy one, to catch potential errors that result in the empty stack. *)
let add_rev_dep (type i o f) ~called_from_peek (dep_node : (i, o, f) Dep_node.t)
    =
  match Call_stack.get_call_stack_tip () with
  | None -> ()
  | Some (Dep_node.T rev_dep) -> (
    let () =
      match (rev_dep.spec.f, dep_node.spec.f) with
      | Async _, Async _ -> ()
      | Async _, Sync _ -> ()
      | Sync _, Sync _ -> ()
      | Sync _, Async _ ->
        if not called_from_peek then
          Code_error.raise
            "[Memo.add_rev_dep ~called_from_peek:false] Synchronous functions \
             are not allowed to depend on asynchronous ones."
            [ ("stack", Call_stack.get_call_stack_as_dyn ())
            ; ("adding", Stack_frame.to_dyn (T dep_node))
            ]
    in
    let dag_node = dep_node.dag_node in
    let rev_dep = rev_dep.dag_node in
    try Dag.add_idempotent global_dep_dag rev_dep dag_node
    with Dag.Cycle cycle ->
      raise
        (Cycle_error.E
           { stack = Call_stack.get_call_stack ()
           ; cycle = List.map cycle ~f:(fun node -> node.Dag.data)
           }) )

(* CR-soon amokhov: The order of dependencies in the resulting list seems to be
   wrong: [Dag.children] returns children in the reverse order instead of the
   order in which they were added. See the comment for [deps : Last_dep.t list]. *)
let get_deps_from_graph_exn (dep_node : _ Dep_node.t) =
  Dag.children dep_node.dag_node
  |> List.map ~f:(fun { Dag.data = Dep_node.T node; _ } ->
         match node.state with
         | Init -> assert false
         | Failed _ -> assert false
         | Running_sync _ -> assert false
         | Running_async _ -> assert false
         | Done res -> Last_dep.T (node, res.data))

type ('input, 'output, 'f) t =
  { spec : ('input, 'output, 'f) Spec.t
  ; cache : ('input, ('input, 'output, 'f) Dep_node.t) Store.t
  }

module Stack_frame = struct
  type ('input, 'output, 'f) memo = ('input, 'output, 'f) t

  include Stack_frame0

  let as_instance_of (type i) (Dep_node.T t) ~of_:(memo : (i, _, _) memo) :
      i option =
    match Type_eq.Id.same memo.spec.witness t.spec.witness with
    | Some Type_eq.T -> Some t.input
    | None -> None
end

let create_with_cache (type i o f) name ~cache ?doc ~input ~visibility ~output
    (typ : (i, o, f) Function.Type.t) (f : f) =
  let f = Function.of_type typ f in
  let spec = Spec.create name ~input ~output ~visibility ~doc ~f in
  ( match visibility with
  | Public _ -> Spec.register spec
  | Hidden -> () );
  Caches.register ~clear:(fun () -> Store.clear cache);
  { cache; spec }

let create_with_store (type i) name
    ~store:(module S : Store_intf.S with type key = i) ?doc ~input ~visibility
    ~output typ f =
  let cache = Store.make (module S) in
  create_with_cache name ~cache ?doc ~input ~output ~visibility typ f

let create (type i) name ?doc ~input:(module Input : Input with type t = i)
    ~visibility ~output typ f =
  let cache = Store.of_table (Table.create (module Input) 16) in
  let input = (module Input : Store_intf.Input with type t = i) in
  create_with_cache name ~cache ?doc ~input ~visibility ~output typ f

let create_hidden (type output) name ?doc ~input typ impl =
  let module O = struct
    type t = output

    let to_dyn (_ : t) = Dyn.Opaque
  end in
  create
    ~output:(Simple (module O))
    ~visibility:Hidden name ?doc ~input typ impl

module Exec = struct
  let make_dep_node ~spec ~state ~input =
    let rec dep_node : _ Dep_node.t =
      { id = Id.gen (); input; spec; dag_node; state }
    and dag_node : Dag.node =
      { info = Dag.create_node_info global_dep_dag; data = Dep_node.T dep_node }
    in
    dep_node
end

let dep_node (type i o f) (t : (i, o, f) t) inp =
  match Store.find t.cache inp with
  | Some dep_node -> dep_node
  | None ->
    let dep_node = Exec.make_dep_node ~spec:t.spec ~input:inp ~state:Init in
    Store.set t.cache inp dep_node;
    dep_node

module Exec_sync = struct
  let compute run inp dep_node =
    (* define the function to update / double check intermediate result *)
    (* set context of computation then run it *)
    let res =
      match
        Call_stack.push_sync_frame (T dep_node) (fun () ->
            match dep_node.spec.f with
            | Function.Sync f ->
              (* If [f] raises an exception, [push_sync_frame] re-raises it
                 twice, so you'd end up with ugly "re-raised by" stack frames.
                 Catching it here cuts the backtrace to just the desired part. *)
              Exn_with_backtrace.try_with (fun () -> f inp))
      with
      | Error exn -> (
        dep_node.state <- Failed (run, exn);
        let code_error (e : Code_error_with_memo_backtrace.t) =
          let bt = exn.backtrace in
          let { Code_error_with_memo_backtrace.exn
              ; reverse_backtrace
              ; outer_call_stack = _
              } =
            e
          in
          Code_error_with_memo_backtrace.E
            { exn
            ; reverse_backtrace =
                { ocaml = Printexc.raw_backtrace_to_string bt
                ; memo = Stack_frame.to_dyn (T dep_node)
                }
                :: reverse_backtrace
            ; outer_call_stack = Call_stack.get_call_stack_as_dyn ()
            }
        in
        match exn.exn with
        | Code_error.E exn ->
          raise
            (code_error
               { Code_error_with_memo_backtrace.exn
               ; reverse_backtrace = []
               ; outer_call_stack = Dyn.String "<n/a>"
               })
        | Code_error_with_memo_backtrace.E e -> raise (code_error e)
        | _exn -> Exn_with_backtrace.reraise exn )
      | Ok res -> res
    in
    (* update the output cache with the correct value *)
    let deps = get_deps_from_graph_exn dep_node in
    dep_node.state <- Done (Cached_value.create res ~deps);
    res

  let recompute inp (dep_node : _ Dep_node.t) =
    let run = Run.current () in
    dep_node.state <- Running_sync run;
    compute run inp dep_node

  let exec_dep_node (dep_node : _ Dep_node.t) inp =
    add_rev_dep ~called_from_peek:false dep_node;
    match dep_node.state with
    | Init -> recompute inp dep_node
    | Failed (run, exn) ->
      if Run.is_current run then
        already_reported exn
      else
        recompute inp dep_node
    | Running_sync run ->
      if Run.is_current run then
        (* hopefully this branch should be unreachable and [add_rev_dep] reports
           a cycle above instead *)
        Code_error.raise "bug: unreported sync dependency_cycle"
          [ ("stack", Call_stack.get_call_stack_as_dyn ())
          ; ("adding", Stack_frame.to_dyn (T dep_node))
          ]
      else
        (* CR-soon amokhov: How can we end up here? If we can't raise an error. *)
        recompute inp dep_node
    | Done cv -> (
      match Cached_value.get_sync cv with
      | Some v -> v
      | None -> recompute inp dep_node )

  let exec t inp = exec_dep_node (dep_node t inp) inp
end

module Exec_async = struct
  let compute inp ivar dep_node =
    (* define the function to update / double check intermediate result *)
    (* set context of computation then run it *)
    let* res =
      Call_stack.push_async_frame (T dep_node) (fun () ->
          match dep_node.spec.f with
          | Function.Async f -> f inp)
    in
    (* update the output cache with the correct value *)
    let deps = get_deps_from_graph_exn dep_node in
    (* CR-someday aalekseyev: Set [dep_node.state] to [Failed] if there are
       errors file running [f]. Currently not doing that because sometimes [f]
       both returns a result and keeps producing errors. Not sure why. *)
    dep_node.state <- Done (Cached_value.create res ~deps);
    (* fill the ivar for any waiting threads *)
    let+ () = Fiber.Ivar.fill ivar res in
    res

  (* the computation that force computes the fiber *)
  let recompute inp (dep_node : _ Dep_node.t) =
    (* create an ivar so other threads can wait for the computation to finish *)
    let ivar : 'b Fiber.Ivar.t = Fiber.Ivar.create () in
    dep_node.state <- Running_async (Run.current (), ivar);
    compute inp ivar dep_node

  let exec_dep_node (dep_node : _ Dep_node.t) inp =
    add_rev_dep ~called_from_peek:false dep_node;
    match dep_node.state with
    | Init -> recompute inp dep_node
    | Failed (run, exn) ->
      if Run.is_current run then
        already_reported exn
      else
        recompute inp dep_node
    | Running_async (run, ivar) ->
      if Run.is_current run then
        Fiber.Ivar.read ivar
      else
        (* In this case we know that: (i) the [ivar] will never be filled
           because the computation was cancelled in the previous run, and
           furthermore (ii) even if [ivar] was still running, we couldn't use
           its result because it would have been out of date. *)
        recompute inp dep_node
    | Done cv -> (
      Cached_value.get_async cv >>= function
      | Some v -> Fiber.return v
      | None -> recompute inp dep_node )

  let exec t inp = exec_dep_node (dep_node t inp) inp
end

let exec (type i o f) (t : (i, o, f) t) =
  match t.spec.f with
  | Function.Async _ -> (Exec_async.exec t : f)
  | Function.Sync _ -> (Exec_sync.exec t : f)

let peek (type i o f) (t : (i, o, f) t) inp =
  match Store.find t.cache inp with
  | None -> None
  | Some dep_node -> (
    add_rev_dep ~called_from_peek:true dep_node;
    match dep_node.state with
    | Init -> None
    | Running_sync _ -> None
    | Running_async _ -> None
    | Failed _ -> None
    | Done cv ->
      if Run.is_current cv.calculated_at then
        Some cv.data
      else
        None )

let peek_exn t inp = Option.value_exn (peek t inp)

let get_deps (type i o f) (t : (i, o, f) t) inp =
  match Store.find t.cache inp with
  | None -> None
  | Some { state = Init; _ } -> None
  | Some { state = Running_async _; _ } -> None
  | Some { state = Running_sync _; _ } -> None
  | Some { state = Failed _; _ } -> None
  | Some { state = Done cv; _ } ->
    Some
      (List.map cv.deps ~f:(fun (Last_dep.T (n, _u)) ->
           (Function.Name.to_string n.spec.name, ser_input n)))

let get_func name =
  match
    let open Option.O in
    Function.Name.get name >>= Spec.find
  with
  | None -> User_error.raise [ Pp.textf "function %s doesn't exist!" name ]
  | Some spec -> spec

let call name input =
  let (Spec.T spec) = get_func name in
  let (module Output : Output_simple with type t = _) = spec.output in
  let input = Dune_lang.Decoder.parse spec.decode Univ_map.empty input in
  let+ output =
    ( match spec.f with
    | Function.Async f -> f
    | Function.Sync f -> fun x -> Fiber.return (f x) )
      input
  in
  Output.to_dyn output

module Function_info = struct
  include Function.Info

  let of_spec (Spec.T spec) = { name = spec.name; doc = spec.doc }
end

let registered_functions () =
  Function.Name.all ()
  |> List.filter_map ~f:(Function.Name.Table.get Spec.by_name)
  |> List.map ~f:Function_info.of_spec
  |> List.sort ~compare:(fun a b ->
         Function.Name.compare a.Function_info.name b.Function_info.name)

let function_info name = get_func name |> Function_info.of_spec

let get_call_stack = Call_stack.get_call_stack

module Sync = struct
  type nonrec ('i, 'o) t = ('i, 'o, 'i -> 'o) t
end

module Async = struct
  type nonrec ('i, 'o) t = ('i, 'o, 'i -> 'o Fiber.t) t
end

let current_run =
  let f () = Run.current () in
  let memo =
    create "current-run"
      ~input:(module Unit)
      ~output:(Simple (module Run))
      ~visibility:Hidden Sync f
  in
  fun () -> exec memo ()

module Lazy_id = Stdune.Id.Make ()

module With_implicit_output = struct
  type ('i, 'o, 'f) t = 'f

  let create (type i o f io) name ?doc ~input ~visibility
      ~output:(module O : Output_simple with type t = o) ~implicit_output
      (typ : (i, o, f) Function.Type.t) (impl : f) =
    let output =
      Output.Simple
        ( module struct
          type t = o * io option

          let to_dyn ((o, _io) : t) =
            Dyn.List [ O.to_dyn o; Dyn.String "<implicit output is opaque>" ]
        end )
    in
    match typ with
    | Function.Type.Sync ->
      let memo =
        create name ?doc ~input ~visibility ~output Sync (fun i ->
            Implicit_output.collect_sync implicit_output (fun () -> impl i))
      in
      ( fun input ->
          let res, output = exec memo input in
          Implicit_output.produce_opt implicit_output output;
          res
        : f )
    | Function.Type.Async ->
      let memo =
        create name ?doc ~input ~visibility ~output Async (fun i ->
            Implicit_output.collect_async implicit_output (fun () -> impl i))
      in
      ( fun input ->
          Fiber.map (exec memo input) ~f:(fun (res, output) ->
              Implicit_output.produce_opt implicit_output output;
              res)
        : f )

  let exec t = t
end

module Cell = struct
  type ('a, 'b, 'f) t = ('a, 'b, 'f) Dep_node.t

  let input (t : (_, _, _) t) = t.input

  let get_sync (type a b) (dep_node : (a, b, a -> b) Dep_node.t) =
    Exec_sync.exec_dep_node dep_node dep_node.input

  let get_async (type a b) (dep_node : (a, b, a -> b Fiber.t) Dep_node.t) =
    Exec_async.exec_dep_node dep_node dep_node.input
end

let cell t inp = dep_node t inp

module Implicit_output = Implicit_output
module Store = Store_intf

let on_already_reported f = on_already_reported := f

let lazy_ (type a) f =
  let module Output = struct
    type t = a

    let to_dyn _ = Dyn.Opaque

    let equal = ( == )
  end in
  let id = Lazy_id.gen () in
  let name = sprintf "lazy-%d" (Lazy_id.to_int id) in
  let visibility = Visibility.Hidden in
  let f = Function.of_type Function.Type.Sync f in
  let spec =
    Spec.create name
      ~input:(module Unit)
      ~output:(Allow_cutoff (module Output))
      ~visibility ~f ~doc:None
  in
  let cell = Exec.make_dep_node ~spec ~state:Init ~input:() in
  fun () -> Cell.get_sync cell

let lazy_async (type a) f =
  let module Output = struct
    type t = a

    let to_dyn _ = Dyn.Opaque

    let equal = ( == )
  end in
  let id = Lazy_id.gen () in
  let name = sprintf "lazy-async-%d" (Lazy_id.to_int id) in
  let visibility = Visibility.Hidden in
  let f = Function.of_type Function.Type.Async f in
  let spec =
    Spec.create name
      ~input:(module Unit)
      ~output:(Allow_cutoff (module Output))
      ~visibility ~f ~doc:None
  in
  let cell = Exec.make_dep_node ~spec ~state:Init ~input:() in
  fun () -> Cell.get_async cell

module Lazy = struct
  type 'a t = unit -> 'a

  let of_val x () = x

  let create f = lazy_ f

  let force f = f ()

  let map x ~f = create (fun () -> f (force x))

  let map2 x y ~f = create (fun () -> f (x ()) (y ()))

  let bind x ~f = create (fun () -> force (f (force x)))

  module Async = struct
    type 'a t = unit -> 'a Fiber.t

    let of_val a () = Fiber.return a

    let create f = lazy_async f

    let force f = f ()

    let map t ~f = create (fun () -> Fiber.map ~f (t ()))
  end
end

module Run = struct
  module Fdecl = struct
    (* [Lazy.t] is the simplest way to create a node in the memoization dag. *)
    type nonrec 'a t = 'a Fdecl.t Lazy.t

    let create to_dyn =
      Lazy.create (fun () ->
          let (_ : Run.t) = current_run () in
          Fdecl.create to_dyn)

    let set t x = Fdecl.set (Lazy.force t) x

    let get t = Fdecl.get (Lazy.force t)
  end

  include Run
end

module Poly (Function : sig
  type 'a input

  type 'a output

  val name : string

  val eval : 'a input -> 'a output

  val to_dyn : _ input -> Dyn.t

  val id : 'a input -> 'a Type_eq.Id.t
end) =
struct
  open Function

  module Key = struct
    type t = T : 'a input -> t

    let to_dyn (T t) = to_dyn t

    let hash (T t) = Type_eq.Id.hash (id t)

    let equal (T x) (T y) = Type_eq.Id.equal (id x) (id y)
  end

  module Value = struct
    type t = T : ('a Type_eq.Id.t * 'a output) -> t
  end

  let impl (key : Key.t) : Value.t =
    match key with
    | Key.T input -> Value.T (id input, eval input)

  let memo = create_hidden name ~input:(module Key) Sync impl

  let eval (type a) (x : a input) : a output =
    match exec memo (Key.T x) with
    | Value.T (id, res) -> (
      match Type_eq.Id.same id (Function.id x) with
      | None ->
        Code_error.raise
          "Type_eq.Id.t mismatch in Memo.Poly: the most likely reason is that \
           the provided Function.id returns different ids for the same input."
          [ ("Function.name", Dyn.String Function.name) ]
      | Some Type_eq.T -> res )
end
