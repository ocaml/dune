open! Stdune
open Fiber.O
module Run = Run
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
    ; (* [outer_call_stack] is a trick to capture some of the information
         that's lost by the async memo error handler. It can be safely ignored
         by the sync error handler. *)
      outer_call_stack : Dyn.t
    }

  type exn += E of t

  let frame_to_dyn { ocaml; memo } =
    Dyn.Record [ ("ocaml", Dyn.String ocaml); ("memo", memo) ]

  let to_dyn { exn; reverse_backtrace; outer_call_stack } =
    Dyn.Record
      [ ("exn", Code_error.to_dyn exn)
      ; ( "backtrace"
        , Dyn.Encoder.list frame_to_dyn (List.rev reverse_backtrace) )
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

module Spec = struct
  type ('a, 'b, 'f) t =
    { name : Function.Name.t
    ; input : (module Store_intf.Input with type t = 'a)
    ; output : (module Output_simple with type t = 'b)
    ; allow_cutoff : 'b Allow_cutoff.t
    ; decode : 'a Dune_lang.Decoder.t
    ; witness : 'a Type_eq.Id.t
    ; f : ('a, 'b, 'f) Function.t
    ; doc : string
    }

  type packed = T : (_, _, _) t -> packed [@@unboxed]

  let by_name = Function.Name.Table.create ~default_value:None

  let find name = Function.Name.Table.get by_name name

  let register t =
    match find t.name with
    | Some _ ->
      Code_error.raise "[Spec.register] called twice on the same function" []
    | None -> Function.Name.Table.set by_name ~key:t.name ~data:(Some (T t))
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
      ; (* When was the value computed *)
        mutable calculated_at : Run.t
      ; deps : Last_dep.t list
      }
  end =
    Cached_value

  and State : sig
    type 'a t =
      (* [Running] includes computations that already terminated with an
         exception or cancelled because we've advanced to the next run. *)
      | Running_sync of Run.t
      | Running_async of Run.t * 'a Fiber.Ivar.t
      | Failed of Run.t * Exn_with_backtrace.t
      | Done of 'a Cached_value.t
  end =
    State

  and Dep_node : sig
    type ('a, 'b, 'f) t =
      { spec : ('a, 'b, 'f) Spec.t
      ; input : 'a
      ; id : Id.t
      ; mutable dag_node : Dag.node Lazy.t
      ; mutable state : 'b State.t
      }

    type packed = T : (_, _, _) t -> packed [@@unboxed]
  end =
    Dep_node

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

  let dep_changed (type a) (node : (_, a, _) Dep_node.t) prev_output
      curr_output =
    match node.spec.allow_cutoff with
    | Yes equal -> not (equal prev_output curr_output)
    | No -> true

  let rec get_sync : type a. a t -> a option =
   fun t ->
    if Run.is_current t.calculated_at then
      Some t.data
    else
      let dep_changed = function
        | Last_dep.T (node, prev_output) -> (
          match node.state with
          | Failed (run, exn) ->
            if Run.is_current run then
              already_reported exn
            else
              true
          | Running_sync run ->
            if Run.is_current run then
              Code_error.raise "dependency_cycle 1" []
            else
              true
          | Running_async _ ->
            Code_error.raise
              "Synchronous function depends on an asynchronous one. That is \
               not allowed. (in fact this case should be unreachable)"
              []
          | Done t' -> (
            get_sync t'
            |> function
            | None -> true
            | Some curr_output -> dep_changed node prev_output curr_output ) )
      in
      match List.exists ~f:dep_changed t.deps with
      | true -> None
      | false ->
        t.calculated_at <- Run.current ();
        Some t.data

  (* Check if a cached value is up to date. If yes, return it *)
  let rec get_async : type a. a t -> a option Fiber.t =
   fun t ->
    if Run.is_current t.calculated_at then
      Fiber.return (Some t.data)
    else
      let rec deps_changed acc = function
        | [] -> Fiber.parallel_map acc ~f:Fn.id >>| List.exists ~f:Fn.id
        | Last_dep.T (node, prev_output) :: deps -> (
          match node.state with
          | Failed (run, exn) ->
            if Run.is_current run then
              already_reported exn
            else
              Fiber.return true
          | Running_sync _ ->
            Code_error.raise
              "Synchronous function called [Cached_value.get_async]" []
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
                (* handle common case separately to avoid feeding more fibers
                   to [parallel_map] *)
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
      deps_changed [] t.deps
      >>| function
      | true -> None
      | false ->
        t.calculated_at <- Run.current ();
        Some t.data
end

let ser_input (type a) (node : (a, _, _) Dep_node.t) =
  let (module Input : Store_intf.Input with type t = a) = node.spec.input in
  Input.to_dyn node.input

let dag_node (dep_node : _ Dep_node.t) = Lazy.force dep_node.dag_node

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

let add_rev_dep dep_node =
  match Call_stack.get_call_stack_tip () with
  | None -> ()
  | Some (Dep_node.T rev_dep) -> (
    (* if the caller doesn't already contain this as a dependent *)
    let rev_dep = dag_node rev_dep in
    try
      if Dag.is_child rev_dep dep_node |> not then
        Dag.add global_dep_dag rev_dep dep_node
    with Dag.Cycle cycle ->
      raise
        (Cycle_error.E
           { stack = Call_stack.get_call_stack ()
           ; cycle = List.map cycle ~f:(fun node -> node.Dag.data)
           }) )

let get_deps_from_graph_exn dep_node =
  Dag.children (dag_node dep_node)
  |> List.map ~f:(fun { Dag.data = Dep_node.T node; _ } ->
         match node.state with
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

module Visibility = struct
  type 'i t =
    | Hidden
    | Public of 'i Dune_lang.Decoder.t
end

module Output = struct
  type 'o t =
    | Simple of (module Output_simple with type t = 'o)
    | Allow_cutoff of (module Output_allow_cutoff with type t = 'o)
end

let create_with_cache (type i o f) name ~cache ~doc
    ~input:(module Input : Store_intf.Input with type t = i) ~visibility
    ~(output : o Output.t) (typ : (i, o, f) Function.Type.t) (f : f) =
  let name = Function.Name.make name in
  let decode : i Dune_lang.Decoder.t =
    match visibility with
    | Visibility.Hidden ->
      let open Dune_lang.Decoder in
      let+ loc = loc in
      User_error.raise ~loc [ Pp.text "<not-implemented>" ]
    | Public decode -> decode
  in
  let (output : (module Output_simple with type t = o)), allow_cutoff =
    match output with
    | Simple (module Output) -> ((module Output), Allow_cutoff.No)
    | Allow_cutoff (module Output) -> ((module Output), Yes Output.equal)
  in
  let spec =
    { Spec.name
    ; input = (module Input)
    ; output
    ; allow_cutoff
    ; decode
    ; witness = Type_eq.Id.create ()
    ; f = Function.of_type typ f
    ; doc
    }
  in
  ( match visibility with
  | Public _ -> Spec.register spec
  | Hidden -> () );
  Caches.register ~clear:(fun () -> Store.clear cache);
  { cache; spec }

let create_with_store (type i) name
    ~store:(module S : Store_intf.S with type key = i) ~doc ~input ~visibility
    ~output typ f =
  let cache = Store.make (module S) in
  create_with_cache name ~cache ~doc ~input ~output ~visibility typ f

let create (type i) name ~doc ~input:(module Input : Input with type t = i)
    ~visibility ~output typ f =
  let cache = Store.of_table (Table.create (module Input) 16) in
  let input = (module Input : Store_intf.Input with type t = i) in
  create_with_cache name ~cache ~doc ~input ~visibility ~output typ f

let create_hidden (type output) name ~doc ~input typ impl =
  let module O = struct
    type t = output

    let to_dyn (_ : t) = Dyn.Opaque
  end in
  create
    ~output:(Simple (module O))
    ~visibility:Hidden name ~doc ~input typ impl

module Exec = struct
  let make_dep_node t ~state ~input =
    let dep_node : _ Dep_node.t =
      { id = Id.gen ()
      ; input
      ; spec = t.spec
      ; dag_node = lazy (assert false)
      ; state
      }
    in
    let dag_node : Dag.node =
      { info = Dag.create_node_info global_dep_dag
      ; data = Dep_node.T dep_node
      }
    in
    add_rev_dep dag_node;
    dep_node.dag_node <- lazy dag_node;
    dep_node
end

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

  let exec t inp =
    match Store.find t.cache inp with
    | None ->
      let run = Run.current () in
      let dep_node =
        Exec.make_dep_node t ~input:inp ~state:(Running_sync run)
      in
      Store.set t.cache inp dep_node;
      compute run inp dep_node
    | Some dep_node -> (
      add_rev_dep (dag_node dep_node);
      match dep_node.state with
      | Failed (run, exn) ->
        if Run.is_current run then
          Nothing.unreachable_code (!on_already_reported exn)
        else
          recompute inp dep_node
      | Running_async _ -> assert false
      | Running_sync run ->
        if Run.is_current run then
          (* hopefully this branch should be unreachable and [add_rev_dep]
             reports a cycle above instead *)
          Code_error.raise "bug: unreported sync dependency_cycle"
            [ ("stack", Call_stack.get_call_stack_as_dyn ())
            ; ("adding", Stack_frame.to_dyn (T dep_node))
            ]
        else
          recompute inp dep_node
      | Done cv -> (
        Cached_value.get_sync cv
        |> function
        | Some v -> v
        | None -> recompute inp dep_node ) )
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

  let exec t inp =
    match Store.find t.cache inp with
    | None ->
      let ivar = Fiber.Ivar.create () in
      let dep_node =
        Exec.make_dep_node t ~input:inp
          ~state:(Running_async (Run.current (), ivar))
      in
      Store.set t.cache inp dep_node;
      compute inp ivar dep_node
    | Some dep_node -> (
      add_rev_dep (dag_node dep_node);
      match dep_node.state with
      | Failed (run, exn) ->
        if Run.is_current run then
          already_reported exn
        else
          recompute inp dep_node
      | Running_sync _ -> assert false
      | Running_async (run, fut) ->
        if Run.is_current run then
          Fiber.Ivar.read fut
        else
          recompute inp dep_node
      | Done cv -> (
        Cached_value.get_async cv
        >>= function
        | Some v -> Fiber.return v
        | None -> recompute inp dep_node ) )
end

let exec (type i o f) (t : (i, o, f) t) =
  match t.spec.f with
  | Function.Async _ -> (Exec_async.exec t : f)
  | Function.Sync _ -> (Exec_sync.exec t : f)

let peek t inp =
  match Store.find t.cache inp with
  | None -> None
  | Some dep_node -> (
    add_rev_dep (dag_node dep_node);
    match dep_node.state with
    | Running_sync _ -> None
    | Running_async _ -> None
    | Failed _ -> None
    | Done cv ->
      if Run.is_current cv.calculated_at then
        Some cv.data
      else
        None )

let peek_exn t inp = Option.value_exn (peek t inp)

let get_deps t inp =
  match Store.find t.cache inp with
  | None
  | Some { state = Running_async _; _ } ->
    None
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
    create "current-run" ~doc:"current run"
      ~input:(module Unit)
      ~output:(Simple (module Run))
      ~visibility:Hidden Sync f
  in
  fun () -> exec memo ()

module Lazy_id = Stdune.Id.Make ()

let lazy_ (type a) f =
  let module Output = struct
    type t = a

    let to_dyn _ = Dyn.Opaque

    let equal = ( == )
  end in
  let id = Lazy_id.gen () in
  let memo =
    create
      (sprintf "lazy-%d" (Lazy_id.to_int id))
      ~doc:"a lazy value"
      ~input:(module Unit)
      ~visibility:Hidden
      ~output:(Allow_cutoff (module Output))
      Sync f
  in
  fun () -> exec memo ()

module Lazy = struct
  type 'a t = unit -> 'a

  let of_val x () = x

  let create f = lazy_ f

  let force f = f ()

  let map x ~f = create (fun () -> f (force x))

  let map2 x y ~f = create (fun () -> f (x ()) (y ()))

  let bind x ~f = create (fun () -> force (f (force x)))
end

module With_implicit_output = struct
  type ('i, 'o, 'f) t = 'f

  let create (type i o f io) name ~doc ~input ~visibility
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
        create name ~doc ~input ~visibility ~output Sync (fun i ->
            Implicit_output.collect_sync implicit_output (fun () -> impl i))
      in
      ( fun input ->
          let res, output = exec memo input in
          Implicit_output.produce_opt implicit_output output;
          res
        : f )
    | Function.Type.Async ->
      let memo =
        create name ~doc ~input ~visibility ~output Async (fun i ->
            Implicit_output.collect_async implicit_output (fun () -> impl i))
      in
      ( fun input ->
          Fiber.map (exec memo input) ~f:(fun (res, output) ->
              Implicit_output.produce_opt implicit_output output;
              res)
        : f )

  let exec t = t
end

module Implicit_output = Implicit_output
module Store = Store_intf

let on_already_reported f = on_already_reported := f
