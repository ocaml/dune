open! Stdune
open Fiber.O

module type Input = Memo_intf.Input
module type Data = Memo_intf.Data
module type Sexpable = Memo_intf.Sexpable
module type Decoder = Memo_intf.Decoder

module Function_name = Interned.Make(struct
    let initial_size = 1024
    let resize_policy = Interned.Greedy
    let order = Interned.Fast
  end) ()

module Function_type = struct
  type ('a, 'b, 'f) t =
    | Sync : ('a, 'b, ('a -> 'b)) t
    | Async : ('a, 'b, ('a -> 'b Fiber.t)) t
end

module Function = struct
  type ('a, 'b, 'f) t =
    | Sync : ('a -> 'b) -> ('a, 'b, ('a -> 'b)) t
    | Async : ('a -> 'b Fiber.t) -> ('a, 'b, ('a -> 'b Fiber.t)) t

  let of_type (type a b f) (t : (a, b, f) Function_type.t) (f : f) : (a, b, f) t =
    match t with
    | Sync -> Sync f
    | Async -> Async f
end

module Witness : sig
  type 'a t

  val create : unit -> 'a t

  val same : 'a t -> 'b t -> ('a, 'b) Type_eq.t option
end = struct

  type _ w = ..

  type 'a t = {
    w : 'a w;
    same : 'b . 'b w -> ('a, 'b) Type_eq.t option
  }

  let create (type a) () =
    let module M = struct
      type _ w += W : a w

      let same (type b) (w : b w) : (a, b) Type_eq.t option = match w with
        | W -> Some Type_eq.T
        | _ -> None
    end
    in
    {
      w = M.W;
      same = M.same
    }

  let same { w = _; same = same1 } { w = w2; same = _ } =
    same1 w2

end

module Allow_cutoff = struct
  type 'o t =
    | No
    | Yes of (module Data with type t = 'o)
end

module Spec = struct

  type ('a, 'b, 'f) t =
    { name : Function_name.t
    ; input : (module Input with type t = 'a)
    ; output : (module Sexpable with type t = 'b)
    ; allow_cutoff : 'b Allow_cutoff.t
    ; decode : 'a Dune_lang.Decoder.t
    ; witness : 'a Witness.t
    ; f : ('a, 'b, 'f) Function.t
    ; doc : string
    }

  type packed = T : (_, _, _) t -> packed [@@unboxed]

  let by_name = Function_name.Table.create ~default_value:None

  let register t =
    Function_name.Table.set by_name ~key:t.name ~data:(Some (T t))

  let find name =
    Function_name.Table.get by_name name
end

module Id = Id.Make()

module Run : sig
  (** Represent a run of the system *)
  type t

  (** Return the current run *)
  val current : unit -> t

  (** Whether this run is the current one *)
  val is_current : t -> bool

  (** End the current run and start a new one *)
  val restart : unit -> unit
end = struct
  type t = bool ref

  let current = ref (ref true)

  let restart () =
    !current := false;
    current := ref true

  let current () = !current
  let is_current t = !t
end

let reset = Run.restart

module M = struct
  module Generic_dag = Dag

  module rec Cached_value : sig
    type 'a t =
      { data : 'a
      ; (* When was the value computed *)
        mutable calculated_at : Run.t
      ; deps : Last_dep.t list
      }
  end = Cached_value

  and State : sig
    type 'a t =
      (* [Running] includes computations that already terminated with an exception
         or cancelled because we've advanced to the next run. *)
      | Running_sync of Run.t
      | Running_async of Run.t * 'a Fiber.Ivar.t
      | Done of 'a Cached_value.t
  end = State

  and Dep_node : sig
    type ('a, 'b, 'f) t = {
      spec : ('a, 'b, 'f) Spec.t;
      input : 'a;
      id : Id.t;
      mutable dag_node : Dag.node Lazy.t;
      mutable state : 'b State.t;
    }

    type packed = T : (_, _, _) t -> packed [@@unboxed]
  end = Dep_node

  and Last_dep : sig
    type t = T : ('a, 'b, 'f) Dep_node.t * 'b -> t
  end = Last_dep

  and Dag : Generic_dag.S with type value := Dep_node.packed
    = Generic_dag.Make(struct type t = Dep_node.packed end)
end

module State = M.State
module Dep_node = M.Dep_node
module Last_dep = M.Last_dep
module Dag = M.Dag

module Cached_value = struct
  include M.Cached_value

  let create x ~deps =
    { deps
    ; data = x
    ; calculated_at = Run.current ()
    }

  let dep_changed (type a) (node : (_, a, _) Dep_node.t) prev_output curr_output =
    match node.spec.allow_cutoff with
    | Yes (module Output : Data with type t = a) ->
      not (Output.equal prev_output curr_output)
    | No ->
      true

  (* Check if a cached value is up to date. If yes, return it *)
  let rec get_async : type a. a t -> a option Fiber.t = fun t ->
    if Run.is_current t.calculated_at then
      Fiber.return (Some t.data)
    else begin
      let rec deps_changed acc = function
        | [] ->
          Fiber.parallel_map acc ~f:Fn.id >>| List.exists ~f:Fn.id
        | Last_dep.T (node, prev_output) :: deps ->
          match node.state with
          | Running_sync _ ->
            failwith "Synchronous function called [Cached_value.get_async]"
          | Running_async (run, ivar) ->
            if not (Run.is_current run) then
              Fiber.return true
            else
              let changed =
                Fiber.Ivar.read ivar >>| fun curr_output ->
                dep_changed node prev_output curr_output
              in
              deps_changed (changed :: acc) deps
          | Done t' ->
            if Run.is_current t'.calculated_at then begin
              (* handle common case separately to avoid feeding more
                 fibers to [parallel_map] *)
              if dep_changed node prev_output t'.data then
                Fiber.return true
              else
                deps_changed acc deps
            end else
              let changed =
                (match node.spec.f with
                 | Sync _ ->
                   Fiber.return (get_sync t')
                 | Async _ ->
                   get_async t') >>| function
                | None -> true
                | Some curr_output ->
                  dep_changed node prev_output curr_output
              in
              deps_changed (changed :: acc) deps
      in
      deps_changed [] t.deps >>| function
      | true -> None
      | false ->
        t.calculated_at <- Run.current ();
        Some t.data
    end
  and get_sync : type a. a t -> a option = fun t ->
    if Run.is_current t.calculated_at then
      Some t.data
    else begin
      let dep_changed = function
        | Last_dep.T (node, prev_output) ->
          match node.state with
          | Running_sync run ->
            if not (Run.is_current run)
            then
              true
            else
              failwith "dependency cycle: "
          | Running_async _ ->
            failwith
              "Synchronous function depends on an asynchronous one. That is not allowed. \
               (in fact this case should be unreachable)"
          | Done t' ->
            get_sync t' |> function
            | None -> true
            | Some curr_output ->
              dep_changed node prev_output curr_output
      in
      match List.exists ~f:dep_changed t.deps with
      | true -> None
      | false ->
        t.calculated_at <- Run.current ();
        Some t.data
    end


end

let ser_input (type a) (node : (a, _, _) Dep_node.t) =
  let (module Input : Input with type t = a) = node.spec.input in
  Input.to_sexp node.input

let dag_node (dep_node : _ Dep_node.t) = Lazy.force dep_node.dag_node

module Stack_frame0 = struct

  open Dep_node

  type t = packed

  let name (T t) = Function_name.to_string t.spec.name
  let input (T t) = ser_input t

  let equal (T a) (T b) = Id.equal a.id b.id
  let compare (T a) (T b) = Id.compare a.id b.id
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

module type S = Memo_intf.S
module type S_sync = Memo_intf.S_sync

let global_dep_dag = Dag.create ()

(* call stack consists of two components: asynchronous call stack managed with a fiber
   context variable and synchronous call stack on top of that managed with an explicit ref *)
module Call_stack = struct

  let synchronous_call_stack = ref []

  let list_first = function
    | [] -> None
    | x :: _ -> Some x

  (* fiber context variable keys *)
  let call_stack_key = Fiber.Var.create ()
  let get_call_stack_tip () =
    match !synchronous_call_stack with
    | [] ->
      list_first (Fiber.Var.get call_stack_key |> Option.value ~default:[])
    | tip :: _ -> Some tip

  let get_call_stack () =
    let async = Fiber.Var.get call_stack_key |> Option.value ~default:[] in
    let sync = !synchronous_call_stack in
    sync @ async

  let push_async_frame frame f =
    assert (List.is_empty !synchronous_call_stack);
    let stack = get_call_stack () in
    Fiber.Var.set call_stack_key (frame :: stack) f

  let protect f ~finally = match f () with
    | res ->
      finally ();
      res
    | exception exn ->
      finally ();
      reraise exn

  let push_sync_frame frame f =
    let old = !synchronous_call_stack in
    let new_ = frame :: old in
    synchronous_call_stack := new_;
    protect f ~finally:(fun () ->
      assert ((==) !synchronous_call_stack new_);
      synchronous_call_stack := old)

end

let dump_stack () =
  let stack = Call_stack.get_call_stack () in
  Printf.eprintf "Memoized function stack:\n";
  List.iter stack ~f:(fun st ->
    Printf.eprintf "   %s %s\n"
      (Stack_frame.name st)
      (Stack_frame.input st |> Sexp.to_string))

module Visibility_simple = struct
  type t =
    | Public (* available via [dune compute] *)
    | Private (* not available via [dune compute] *)
end
module type Visibility_simple = sig
  val visibility : Visibility_simple.t
end
module Public = struct let visibility = Visibility_simple.Public end
module Private = struct let visibility = Visibility_simple.Private end

let add_rev_dep dep_node =
  match Call_stack.get_call_stack_tip () with
  | None ->
    ()
  | Some (Dep_node.T rev_dep) ->
    (* if the caller doesn't already contain this as a dependent *)
    let rev_dep = dag_node rev_dep in
    try
      if Dag.is_child rev_dep dep_node |> not then
        Dag.add global_dep_dag rev_dep dep_node
    with Dag.Cycle cycle ->
      raise (Cycle_error.E {
        stack = Call_stack.get_call_stack ();
        cycle = List.map cycle ~f:(fun node -> node.Dag.data)
      })

let get_deps_from_graph_exn dep_node =
  Dag.children (dag_node dep_node)
  |> List.map ~f:(fun { Dag.data = Dep_node.T node; _ } ->
    match node.state with
    | Running_sync _ -> assert false
    | Running_async _ -> assert false
    | Done res ->
      Last_dep.T (node, res.data))

type ('input, 'output, 'f) t =
  { spec  : ('input, 'output, 'f) Spec.t
  ; cache : ('input, ('input, 'output, 'f) Dep_node.t) Table.t
  ; fdecl : 'f Fdecl.t option
  }

type ('input, 'output) t_sync = ('input, 'output, ('input -> 'output)) t
type ('input, 'output) t_async = ('input, 'output, ('input -> 'output Fiber.t)) t


module Stack_frame = struct
  type ('input, 'output, 'fdecl) memo = ('input, 'output, 'fdecl) t

  include Stack_frame0

  let as_instance_of (type i) (Dep_node.T t) ~of_:(memo : (i, _, _) memo) : i option =
    match Witness.same memo.spec.witness t.spec.witness with
    | Some T ->
      Some t.input
    | None -> None
end

module Visibility = struct
  type 'i t =
    | Hidden
    | Public of 'i Dune_lang.Decoder.t
end

module Output = struct
  type 'o t =
    | Simple of (module Memo_intf.Sexpable with type t = 'o)
    | Allow_cutoff of (module Data with type t = 'o)
end

let create_gen (type i o f)
      name
      ~doc
      ~input:(module Input : Input with type t = i)
      ~visibility
      ~(output : o Output.t)
      (typ : (i, o, f) Function_type.t)
      (body : f option)
  =
  let name = Function_name.make name in
  let fdecl, f =
    match body with
    | None ->
      let decl_and_get () =
        let f = Fdecl.create () in
        Some f, (fun x -> Fdecl.get f x)
      in
      ((match typ with
       | Sync -> decl_and_get ()
       | Async -> decl_and_get ()) : f Fdecl.t option * f)
    | Some f ->
      None, f
  in
  let decode : i Dune_lang.Decoder.t =
    match visibility with
    | Visibility.Hidden ->
      let open Dune_lang.Decoder in
      loc >>= fun loc ->
      Exn.fatalf ~loc "<not-implemented>"
    | Public decode -> decode
  in
  let (output : (module Sexpable with type t = o)), allow_cutoff =
    match output with
    | Simple (module Output) ->
      (module Output), Allow_cutoff.No
    | Allow_cutoff (module Output) ->
      (module Output), (Yes (module Output))
  in
  let spec =
    { Spec.
      name
    ; input = (module Input)
    ; output
    ; allow_cutoff
    ; decode
    ; witness = Witness.create ()
    ; f = Function.of_type typ f
    ; doc
    }
  in
  (match visibility with
   | Public _ -> Spec.register spec
   | Hidden -> ());
  { cache = Table.create (module Input) 1024
  ; spec
  ; fdecl
  }

module Make_gen_sync
    (Visibility : Visibility_simple)
    (Input : Input)
    (Decoder : Decoder with type t := Input.t)
= struct

  type 'a t = (Input.t, 'a) t_sync

  let create_internal
        (type o) name ?(allow_cutoff=true) ~doc
        (module Output : Data with type t = o) body =
    create_gen name ~doc ~input:(module Input)
      ~visibility:(match Visibility.visibility with
        | Private -> Hidden
        | Public -> Public Decoder.decode)
      ~output:(match allow_cutoff with
        | false -> Simple (module Output)
        | true -> Allow_cutoff (module Output))
      Function_type.Sync
      body

  let create name ?allow_cutoff ~doc output f =
    create_internal name ?allow_cutoff ~doc output (Some f)

  let fcreate name ?allow_cutoff ~doc output =
    create_internal name ?allow_cutoff ~doc output None

end

module Exec_sync = struct
    let compute t inp dep_node =
    (* define the function to update / double check intermediate result *)
    (* set context of computation then run it *)
    let res = Call_stack.push_sync_frame (T dep_node) (fun () -> match t.spec.f with
      | Sync f -> f inp)
    in
    (* update the output cache with the correct value *)
    let deps =
      get_deps_from_graph_exn dep_node
    in
    dep_node.state <- Done (Cached_value.create res ~deps);
    res

  (* the computation that force computes the fiber *)
  let recompute t inp (dep_node : _ Dep_node.t) =
    (* create an ivar so other threads can wait for the computation to
       finish *)
    dep_node.state <- Running_sync (Run.current ());
    compute t inp dep_node

  let exec t inp =
    match Table.find t.cache inp with
    | None ->
      let dep_node : _ Dep_node.t =
        { id = Id.gen ()
        ; input = inp
        ; spec = t.spec
        ; dag_node = lazy (assert false)
        ; state = Running_sync (Run.current ())
        }
      in
      let dag_node : Dag.node =
        { info = Dag.create_node_info global_dep_dag
        ; data = Dep_node.T dep_node
        }
      in
      dep_node.dag_node <- lazy dag_node;
      Table.add t.cache inp dep_node;
      add_rev_dep dag_node;
      compute t inp dep_node
    | Some dep_node ->
      add_rev_dep (dag_node dep_node);
      match dep_node.state with
      | Running_async _ ->
        assert false
      | Running_sync run ->
        if Run.is_current run then
          failwith "dependency cycle"
        else
          recompute t inp dep_node
      | Done cv ->
        Cached_value.get_sync cv |> function
        | Some v -> v
        | None -> recompute t inp dep_node

end

module Make_gen
    (Visibility : Visibility_simple)
    (Input : Input)
    (Decoder : Decoder with type t := Input.t)
= struct

  type 'a t = (Input.t, 'a) t_async

  let create_internal
        (type o) name ?(allow_cutoff=true) ~doc
        (module Output : Data with type t = o) body =
    create_gen name ~doc ~input:(module Input)
      ~visibility:(match Visibility.visibility with
        | Private -> Hidden
        | Public -> Public Decoder.decode)
      ~output:(match allow_cutoff with
        | false -> Simple (module Output)
        | true -> Allow_cutoff (module Output))
      Function_type.Async
      body


  let create name ?allow_cutoff ~doc output f =
    create_internal name ?allow_cutoff ~doc output (Some f)

  let fcreate name ?allow_cutoff ~doc output =
    create_internal name ?allow_cutoff ~doc output None
end

module Exec_async = struct
  let compute t inp ivar dep_node =
    (* define the function to update / double check intermediate result *)
    (* set context of computation then run it *)
    Call_stack.push_async_frame (T dep_node) (fun () -> match t.spec.f with
      | Async f -> f inp) >>= fun res ->
    (* update the output cache with the correct value *)
    let deps =
      get_deps_from_graph_exn dep_node
    in
    dep_node.state <- Done (Cached_value.create res ~deps);
    (* fill the ivar for any waiting threads *)
    Fiber.Ivar.fill ivar res >>= fun () ->
    Fiber.return res

  (* the computation that force computes the fiber *)
  let recompute t inp (dep_node : _ Dep_node.t) =
    (* create an ivar so other threads can wait for the computation to
       finish *)
    let ivar : 'b Fiber.Ivar.t = Fiber.Ivar.create () in
    dep_node.state <- Running_async (Run.current (), ivar);
    compute t inp ivar dep_node

  let exec t inp =
    match Table.find t.cache inp with
    | None ->
      let ivar = Fiber.Ivar.create () in
      let dep_node : _ Dep_node.t =
        { id = Id.gen ()
        ; input = inp
        ; spec = t.spec
        ; dag_node = lazy (assert false)
        ; state = Running_async (Run.current (), ivar)
        }
      in
      let dag_node : Dag.node =
        { info = Dag.create_node_info global_dep_dag
        ; data = Dep_node.T dep_node
        }
      in
      dep_node.dag_node <- lazy dag_node;
      Table.add t.cache inp dep_node;
      add_rev_dep dag_node;
      compute t inp ivar dep_node
    | Some dep_node ->
      add_rev_dep (dag_node dep_node);
      match dep_node.state with
      | Running_sync _ -> assert false
      | Running_async (run, fut) ->
        if Run.is_current run then
          Fiber.Ivar.read fut
        else
          recompute t inp dep_node
      | Done cv ->
        Cached_value.get_async cv >>= function
        | Some v -> Fiber.return v
        | None -> recompute t inp dep_node
end

let peek t inp =
  match Table.find t.cache inp with
  | None -> None
  | Some dep_node ->
    add_rev_dep (dag_node dep_node);
    match dep_node.state with
    | Running_sync _ -> None
    | Running_async _ -> None
    | Done cv ->
      if Run.is_current cv.calculated_at then
        Some cv.data
      else
        None

let peek_exn t inp = Option.value_exn (peek t inp)

let set_impl t f =
  match t.fdecl with
  | None -> invalid_arg "Memo.set_impl"
  | Some fdecl -> Fdecl.set fdecl f

let get_deps t inp =
  match Table.find t.cache inp with
  | None | Some { state = Running_async _; _ } -> None
  | Some { state = Running_sync _; _ } ->
    None
  | Some { state = Done cv; _ } ->
    Some (List.map cv.deps ~f:(fun (Last_dep.T (n,_u)) ->
      (Function_name.to_string n.spec.name, ser_input n)))

module Make(Input : Input)(Decoder : Decoder with type t := Input.t) = struct
  include Make_gen(Public)(Input)(Decoder)
  include Exec_async
end

module Make_sync(Input : Input)(Decoder : Decoder with type t := Input.t) = struct
  include Make_gen_sync(Public)(Input)(Decoder)
  include Exec_sync
end

module Make_hidden(Input : Input) = struct
  include Make_gen(Private)(Input)(struct
    let decode : Input.t Dune_lang.Decoder.t =
      let open Dune_lang.Decoder in
      loc >>= fun loc ->
      Exn.fatalf ~loc "<not-implemented>"
  end)
  include Exec_async
end

let get_func name =
  match
    let open Option.O in
    Function_name.get name >>= Spec.find
  with
  | None -> Exn.fatalf "@{<error>Error@}: function %s doesn't exist!" name
  | Some spec -> spec

let call name input =
  let (Spec.T spec) = get_func name in
  let (module Output : Sexpable with type t = _) = spec.output in
  let input = Dune_lang.Decoder.parse spec.decode Univ_map.empty input in
  (match spec.f with
   | Async f -> f
   | Sync f -> (fun x -> Fiber.return (f x))) input >>| fun output ->
  Output.to_sexp output

module Function_info = struct
  type t =
    { name : string
    ; doc  : string
    }

  let of_spec (Spec.T spec) =
    { name = Function_name.to_string spec.name
    ; doc = spec.doc
    }
end

let registered_functions () =
  Function_name.all ()
  |> List.filter_map ~f:(Function_name.Table.get Spec.by_name)
  |> List.map ~f:Function_info.of_spec
  |> List.sort ~compare:(fun a b ->
    String.compare a.Function_info.name b.Function_info.name)

let function_info name =
  get_func name |> Function_info.of_spec

let get_call_stack = Call_stack.get_call_stack
