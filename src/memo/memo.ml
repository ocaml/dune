open !Stdune
open Fiber.O

module type Input = Memo_intf.Input
module type Output = Memo_intf.Output

module Function_name = Interned.Make(struct
    let initial_size = 1024
    let resize_policy = Interned.Greedy
    let order = Interned.Fast
  end) ()

module Spec = struct
  type _ witness = ..

  type ('a, 'b) t =
    { name : Function_name.t
    ; allow_cutoff : bool
    ; input : (module Input with type t = 'a)
    ; output : (module Output with type t = 'b)
    ; witness : 'a witness
    ; f : 'a -> 'b Fiber.t
    ; doc : string
    }

  type packed = T : (_, _) t -> packed [@@unboxed]

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
      | Running of Run.t * 'a Fiber.Ivar.t
      | Done of 'a Cached_value.t
  end = State

  and Dep_node : sig
    type ('a, 'b) t = {
      spec : ('a, 'b) Spec.t;
      input : 'a;
      id : Id.t;
      mutable dag_node : Dag.node Lazy.t;
      mutable state : 'b State.t;
    }

    type packed = T : (_, _) t -> packed [@@unboxed]
  end = Dep_node

  and Last_dep : sig
    type t = T : ('a, 'b) Dep_node.t * 'b -> t
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

  let dep_changed (type a) (node : (_, a) Dep_node.t) prev_output curr_output =
    if node.spec.allow_cutoff then
      let (module Output : Output with type t = a) = node.spec.output in
      not (Output.equal prev_output curr_output)
    else
      true

  (* Check if a cached value is up to date. If yes, return it *)
  let rec get : type a. a t -> a option Fiber.t = fun t ->
    if Run.is_current t.calculated_at then
      Fiber.return (Some t.data)
    else begin
      let rec deps_changed acc = function
        | [] ->
          Fiber.parallel_map acc ~f:(fun x -> x) >>| List.exists ~f:(fun x -> x)
        | Last_dep.T (node, prev_output) :: deps ->
          match node.state with
          | Running (run, ivar) ->
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
                get t' >>| function
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
end

let ser_input (type a) (node : (a, _) Dep_node.t) =
  let (module Input : Input with type t = a) = node.spec.input in
  Input.to_sexp node.input

let dag_node (dep_node : _ Dep_node.t) = Lazy.force dep_node.dag_node

module Stack_frame = struct
  open Dep_node

  type t = packed

  let name (T t) = Function_name.to_string t.spec.name
  let input (T t) = ser_input t

  let equal (T a) (T b) = Id.equal a.id b.id
  let compare (T a) (T b) = Id.compare a.id b.id
end

module Cycle_error = struct
  type t =
    { cycle : Stack_frame.t list
    ; stack : Stack_frame.t list
    }

  exception E of t

  let get t = t.cycle
  let stack t = t.stack
end

module type S = Memo_intf.S with type stack_frame := Stack_frame.t

let global_dep_dag = Dag.create ()

(* fiber context variable keys *)
let call_stack_key = Fiber.Var.create ()
let get_call_stack =
  Fiber.Var.get call_stack_key (* get call stack *)
  >>| Option.value ~default:[] (* default call stack is empty *)

let push_stack_frame frame f =
  get_call_stack >>= fun stack ->
  Fiber.Var.set call_stack_key (frame :: stack) f

let dump_stack v =
  get_call_stack
  >>|
  (Printf.printf "Memoized function stack:\n";
   List.iter ~f:(
     fun st -> Printf.printf "   %s %s\n"
                 (Stack_frame.name st)
                 (Stack_frame.input st |> Sexp.to_string)
   )
  )
  >>| (fun _ -> v)

module Make(Input : Input) : S with type input := Input.t = struct
  module Table = Hashtbl.Make(Input)

  type 'a t =
    { spec  : (Input.t, 'a) Spec.t
    ; cache : (Input.t, 'a) Dep_node.t Table.t
    }

  type _ Spec.witness += W : Input.t Spec.witness

  let add_rev_dep dep_node =
    get_call_stack >>| function
    | [] -> ()
    | (Dep_node.T rev_dep) :: _ as stack ->
      (* if the caller doesn't already contain this as a dependent *)
      let rev_dep = dag_node rev_dep in
      try
        if Dag.is_child rev_dep dep_node |> not then
          Dag.add global_dep_dag rev_dep dep_node
      with Dag.Cycle cycle ->
        Cycle_error.E {
          stack = stack;
          cycle = List.map cycle ~f:(fun node -> node.Dag.data)
        } |> raise

  let get_deps t inp =
    match Table.find t.cache inp with
    | None | Some { state = Running _; _ } -> None
    | Some { state = Done cv; _ } ->
      Some (List.map cv.deps ~f:(fun (Last_dep.T (n,_u)) ->
        (Function_name.to_string n.spec.name, ser_input n)))

  let create name ?(allow_cutoff=true) ~doc output f =
    let name = Function_name.make name in
    let spec =
      { Spec.
        name
      ; input = (module Input); output
      ; allow_cutoff
      ; witness = W
      ; f
      ; doc
      }
    in
    Spec.register spec;
    { cache = Table.create 1024
    ; spec
    }

  let compute t inp ivar dep_node =
    (* define the function to update / double check intermediate result *)
    (* set context of computation then run it *)
    push_stack_frame (T dep_node) (t.spec.f inp) >>= fun res ->
    (* update the output cache with the correct value *)
    let deps =
      Dag.children (dag_node dep_node)
      |> List.map ~f:(fun { Dag.data = Dep_node.T node; _ } ->
        match node.state with
        | Running _ -> assert false
        | Done res ->
          Last_dep.T (node, res.data)
      )
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
    dep_node.state <- Running (Run.current (), ivar);
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
        ; state = Running (Run.current (), ivar)
        }
      in
      let dag_node : Dag.node =
        { info = Dag.create_node_info global_dep_dag
        ; data = Dep_node.T dep_node
        }
      in
      dep_node.dag_node <- lazy dag_node;
      Table.add t.cache inp dep_node;
      add_rev_dep dag_node >>= fun () ->
      compute t inp ivar dep_node
    | Some dep_node ->
      add_rev_dep (dag_node dep_node) >>= fun () ->
      match dep_node.state with
      | Running (run, fut) ->
        if Run.is_current run then
          Fiber.Ivar.read fut
        else
          recompute t inp dep_node
      | Done cv ->
        Cached_value.get cv >>= function
        | Some v -> Fiber.return v
        | None -> recompute t inp dep_node

  let peek t inp =
    (* This doesn't add a reverse dependency, which is wrong, see
       https://github.com/ocaml/dune/issues/1583 for details.  *)
    match Table.find t.cache inp with
    | None -> None
    | Some dep_node ->
      match dep_node.state with
      | Running _ -> None
      | Done cv ->
        if Run.is_current cv.calculated_at then
          Some cv.data
        else
          None

  let peek_exn t inp = Option.value_exn (peek t inp)

  module Stack_frame = struct
    let input (Dep_node.T dep_node) : Input.t option =
      match dep_node.spec.witness with
      | W -> Some dep_node.input
      | _ -> None

    let instance_of (Dep_node.T dep_node) ~of_ =
      dep_node.spec.name = of_.spec.name
  end
end

let call name input =
  match
    let open Option.O in
    Function_name.get name >>= Spec.find
  with
  | None -> Exn.fatalf "@{<error>Error@}: function %s doesn't exist!" name
  | Some (Spec.T spec) ->
    let (module Input : Input with type t = _) = spec.input in
    let (module Output : Output with type t = _) = spec.output in
    let input = Dune_lang.Decoder.parse Input.decode Univ_map.empty input in
    spec.f input >>| fun output ->
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
