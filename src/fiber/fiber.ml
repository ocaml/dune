open! Stdune

(* This module tries to enforce the following invariants:

   - the execution context passed to a continuation is the same as the current
   one

   - the execution of a fiber always ends with [deref]

   - when an exception is raised by the user code, the exception must be
   forwarded to the execution context that was active at the time the exception
   was raised

   - when an exception is raised by the user code, then we assume that the
   current fiber didn't reach the [deref] point. As a result we have to call
   [deref] at this point on the current execution context *)

module Execution_context : sig
  type t

  module K : sig
    type execution_context := t

    (* Represent a suspended fiber *)
    type 'a t

    (* Create a continuation that captures the current execution context *)
    val create : execution_context -> ('a -> unit) -> 'a t

    (* Enqueue the restarting of a suspended fiber. *)
    val enqueue : 'a t -> 'a -> unit
  end

  type 'a fiber = t -> ('a -> unit) -> unit

  (* Start a background fiber. Before calling [spawn] n times, the caller must
     call [add_refs t n]. *)
  val spawn : ('a -> 'b fiber) -> 'a -> 'b fiber

  val spawn2 : ('a -> 'b -> 'c fiber) -> 'a -> 'b -> 'c fiber

  (* Add [n] references to the current execution context *)
  val add_refs : t -> int -> unit

  (* Decrease the reference count of the current execution context *)
  val deref : t -> unit

  val map_reduce_errors :
       (module Monoid with type t = 'a)
    -> on_error:(Exn_with_backtrace.t -> 'a fiber)
    -> (unit -> 'b fiber)
    -> ('b, 'a) result fiber

  val with_error_handler :
       (unit -> 'a fiber)
    -> on_error:(Exn_with_backtrace.t -> Nothing.t fiber)
    -> 'a fiber

  val vars : t -> Univ_map.t

  val set_vars : t -> Univ_map.t -> t

  val run : 'a fiber -> iter:(unit -> unit) -> 'a

  val reraise_all : t -> Exn_with_backtrace.t list -> unit
end = struct
  type t =
    { on_error : Exn_with_backtrace.t k
    ; vars : Univ_map.t
    ; on_release : on_release
    ; jobs : job Queue.t
    }

  and ('a, 'b) on_release' =
    { k : ('a, 'b) result k
    ; mutable ref_count : int
    ; mutable acc : 'b
    }

  and on_release = On_release : _ on_release' -> on_release [@@unboxed]

  and 'a k =
    { run : 'a -> unit
    ; ctx : t
    }

  and job = Job : 'a k * 'a -> job

  type 'a fiber = t -> ('a -> unit) -> unit

  module K = struct
    type 'a t = 'a k

    let create ctx run = { run; ctx }

    let enqueue k x = Queue.push k.ctx.jobs (Job (k, x))
  end

  let add_refs t n =
    let (On_release r) = t.on_release in
    r.ref_count <- r.ref_count + n

  (* Exception used to unwind the stack *)
  exception K : t * ('a -> unit) * 'a -> exn

  let rec forward_exn_with_backtrace t exn =
    run_k t.on_error.ctx t.on_error.run exn

  and forward_error t exn =
    let exn = Exn_with_backtrace.capture exn in
    forward_exn_with_backtrace t exn

  and run_k : type a. t -> (a -> unit) -> a -> unit =
   fun t run x ->
    try run x with
    | K (t, k, x) -> run_k t k x
    | exn -> forward_error t exn

  let really_deref r =
    let ref_count = r.ref_count - 1 in
    r.ref_count <- ref_count;
    match ref_count with
    | 0 -> raise_notrace (K (r.k.ctx, r.k.run, Error r.acc))
    | _ -> assert (ref_count > 0)

  let deref t =
    let (On_release r) = t.on_release in
    really_deref r

  let exec_in_sub_context f t k =
    try f () t k with
    | K (_, k, x) -> k x
    | exn -> forward_error t exn

  let map_reduce_errors (type a) (module M : Monoid with type t = a) ~on_error f
      t k =
    let on_release =
      { k = { ctx = t; run = k }; ref_count = 1; acc = M.empty }
    in
    let on_error =
      { ctx = t
      ; run =
          (fun exn ->
            on_error exn t (fun m ->
                on_release.acc <- M.combine on_release.acc m;
                really_deref on_release))
      }
    in
    let t' = { t with on_error; on_release = On_release on_release } in
    exec_in_sub_context f t' (fun x ->
        let ref_count = on_release.ref_count - 1 in
        on_release.ref_count <- ref_count;
        assert (ref_count = 0);
        raise_notrace (K (on_release.k.ctx, on_release.k.run, Ok x)))

  let with_error_handler f ~on_error t k =
    let on_error =
      { ctx = t; run = (fun exn -> on_error exn t Nothing.unreachable_code) }
    in
    let t' = { t with on_error } in
    exec_in_sub_context f t' (fun x -> raise_notrace (K (t, k, x)))

  let vars t = t.vars

  let set_vars t vars = { t with vars }

  let spawn f x t k =
    try f x t k with
    | K (t, k, x) -> run_k t k x
    | exn -> forward_error t exn

  let spawn2 f x y t k =
    try f x y t k with
    | K (t, k, x) -> run_k t k x
    | exn -> forward_error t exn

  let reraise_all t exns =
    add_refs t (List.length exns - 1);
    List.iter exns ~f:(forward_exn_with_backtrace t)

  let rec run_jobs jobs =
    match Queue.pop jobs with
    | None -> ()
    | Some (Job ({ run; ctx }, x)) ->
      run_k ctx run x;
      run_jobs jobs

  let run fiber ~iter =
    let result = ref None in
    let rec t =
      { on_error = { ctx = t; run = (fun exn -> result := Some (Error exn)) }
      ; vars = Univ_map.empty
      ; on_release =
          On_release { k = { ctx = t; run = ignore }; ref_count = 1; acc = () }
      ; jobs = Queue.create ()
      }
    in
    spawn (fun () -> fiber) () t (fun x -> result := Some (Ok x));
    run_jobs t.jobs;
    let rec loop () =
      match !result with
      | Some (Ok res) -> res
      | Some (Error exn) -> Exn_with_backtrace.reraise exn
      | None ->
        iter ();
        run_jobs t.jobs;
        loop ()
    in
    loop ()
end

module EC = Execution_context
module K = EC.K

type 'a t = 'a EC.fiber

let of_thunk f ctx k = f () ctx k

let return x _ctx k = k x

let never _ _ = ()

type ('a, 'b) fork_and_join_state =
  | Nothing_yet
  | Got_a of 'a
  | Got_b of 'b

let fork_and_join fa fb ctx k =
  let state = ref Nothing_yet in
  EC.add_refs ctx 1;
  EC.spawn fa () ctx (fun a ->
      match !state with
      | Nothing_yet ->
        state := Got_a a;
        EC.deref ctx
      | Got_a _ -> assert false
      | Got_b b -> k (a, b));
  fb () ctx (fun b ->
      match !state with
      | Nothing_yet ->
        state := Got_b b;
        EC.deref ctx
      | Got_a a -> k (a, b)
      | Got_b _ -> assert false)

let fork_and_join_unit fa fb ctx k =
  let state = ref Nothing_yet in
  EC.add_refs ctx 1;
  EC.spawn fa () ctx (fun () ->
      match !state with
      | Nothing_yet ->
        state := Got_a ();
        EC.deref ctx
      | Got_a _ -> assert false
      | Got_b b -> k b);
  fb () ctx (fun b ->
      match !state with
      | Nothing_yet ->
        state := Got_b b;
        EC.deref ctx
      | Got_a () -> k b
      | Got_b _ -> assert false)

module O = struct
  let ( >>> ) a b ctx k = a ctx (fun () -> b ctx k)

  let ( >>= ) t f ctx k = t ctx (fun x -> f x ctx k)

  let ( >>| ) t f ctx k = t ctx (fun x -> k (f x))

  let ( let+ ) = ( >>| )

  let ( let* ) = ( >>= )

  let ( and* ) a b = fork_and_join (fun () -> a) (fun () -> b)

  let ( and+ ) = ( and* )
end

open O

let map t ~f = t >>| f

let bind t ~f = t >>= f

let both a b =
  let* x = a in
  let* y = b in
  return (x, y)

let sequential_map l ~f =
  let rec loop l acc =
    match l with
    | [] -> return (List.rev acc)
    | x :: l ->
      let* x = f x in
      loop l (x :: acc)
  in
  loop l []

let sequential_iter l ~f =
  let rec loop l =
    match l with
    | [] -> return ()
    | x :: l ->
      let* () = f x in
      loop l
  in
  loop l

let all = sequential_map ~f:Fun.id

let list_of_option_array =
  let rec loop arr i acc =
    if i = 0 then
      acc
    else
      let i = i - 1 in
      match arr.(i) with
      | None -> assert false
      | Some x -> loop arr i (x :: acc)
  in
  fun a -> loop a (Array.length a) []

let parallel_map l ~f ctx k =
  match l with
  | [] -> k []
  | [ x ] -> f x ctx (fun x -> k [ x ])
  | _ ->
    let n = List.length l in
    EC.add_refs ctx (n - 1);
    let left_over = ref n in
    let results = Array.make n None in
    List.iteri l ~f:(fun i x ->
        EC.spawn f x ctx (fun y ->
            results.(i) <- Some y;
            decr left_over;
            if !left_over = 0 then
              k (list_of_option_array results)
            else
              EC.deref ctx))

let all_concurrently = parallel_map ~f:Fun.id

let[@inline always] parallel_iter_generic ~n ~iter ~f ctx k =
  EC.add_refs ctx (n - 1);
  let left_over = ref n in
  let k () =
    decr left_over;
    if !left_over = 0 then
      k ()
    else
      EC.deref ctx
  in
  iter ~f:(fun x -> EC.spawn f x ctx k)

let parallel_iter l ~f ctx k =
  match l with
  | [] -> k ()
  | [ x ] -> f x ctx k
  | _ -> parallel_iter_generic ~n:(List.length l) ~iter:(List.iter l) ~f ctx k

let all_concurrently_unit l = parallel_iter l ~f:Fun.id

let parallel_iter_set (type a s)
    (module S : Set.S with type elt = a and type t = s) t ~(f : a -> unit t) ctx
    k =
  let len = S.cardinal t in
  match len with
  | 0 -> k ()
  | 1 -> f (Option.value_exn (S.min_elt t)) ctx k
  | n -> parallel_iter_generic ~n ~iter:(S.iter t) ~f ctx k

let record_metrics t ~tag =
  of_thunk (fun () ->
      let timer = Metrics.Timer.start tag in
      let+ res = t in
      Metrics.Timer.stop timer;
      res)

let rec sequential_iter_seq (seq : _ Seq.t) ~f =
  match seq () with
  | Nil -> return ()
  | Cons (x, seq) ->
    let* () = f x in
    sequential_iter_seq seq ~f

module Make_map_traversals (Map : Map.S) = struct
  let parallel_iter t ~f ctx k =
    match Map.cardinal t with
    | 0 -> k ()
    | 1 ->
      let x, y = Map.choose t |> Option.value_exn in
      f x y ctx k
    | n ->
      EC.add_refs ctx (n - 1);
      let left_over = ref n in
      let k () =
        decr left_over;
        if !left_over = 0 then
          k ()
        else
          EC.deref ctx
      in
      Map.iteri t ~f:(fun x y -> EC.spawn2 f x y ctx k)

  let parallel_map t ~f ctx k =
    match Map.cardinal t with
    | 0 -> k Map.empty
    | 1 ->
      let x, y = Map.choose t |> Option.value_exn in
      f x y ctx (fun y -> k (Map.singleton x y))
    | n ->
      EC.add_refs ctx (n - 1);
      let left_over = ref n in
      let cell = ref None in
      let k (refs : _ option ref Map.t) =
        k (Map.mapi refs ~f:(fun _ r -> Option.value_exn !r))
      in
      let refs =
        Map.mapi t ~f:(fun x y ->
            let res = ref None in
            EC.spawn2 f x y ctx (fun z ->
                res := Some z;
                decr left_over;
                if !left_over = 0 then
                  Option.iter !cell ~f:k
                else
                  EC.deref ctx);
            res)
      in
      if !left_over = 0 then
        k refs
      else
        cell := Some refs
end
[@@inline always]

let rec repeat_while : 'a. f:('a -> 'a option t) -> init:'a -> unit t =
 fun ~f ~init ->
  let* result = f init in
  match result with
  | None -> return ()
  | Some init -> repeat_while ~f ~init

module Var = struct
  include Univ_map.Key

  let get var ctx k = k (Univ_map.find (EC.vars ctx) var)

  let get_exn var ctx k = k (Univ_map.find_exn (EC.vars ctx) var)

  let set var x f ctx k =
    f () (EC.set_vars ctx (Univ_map.set (EC.vars ctx) var x)) k

  let unset var f ctx k =
    f () (EC.set_vars ctx (Univ_map.remove (EC.vars ctx) var)) k

  let create () = create ~name:"var" (fun _ -> Dyn.string "var")
end

let with_error_handler = EC.with_error_handler

let map_reduce_errors = EC.map_reduce_errors

let collect_errors f =
  let module Exns = Monoid.Appendable_list (Exn_with_backtrace) in
  let+ res =
    map_reduce_errors
      (module Exns)
      f
      ~on_error:(fun e -> return (Appendable_list.singleton e))
  in
  match res with
  | Ok x -> Ok x
  | Error l -> Error (Appendable_list.to_list l)

let reraise_all l ctx _k =
  match l with
  | [] -> ()
  | [ exn ] -> Exn_with_backtrace.reraise exn
  | exns -> EC.reraise_all ctx exns

let finalize f ~finally =
  let* res1 = collect_errors f in
  let* res2 = collect_errors finally in
  let res =
    match (res1, res2) with
    | Ok x, Ok () -> Ok x
    | Error l, Ok _
    | Ok _, Error l ->
      Error l
    | Error l1, Error l2 -> Error (l1 @ l2)
  in
  match res with
  | Ok x -> return x
  | Error l -> reraise_all l

module Ivar = struct
  type 'a state =
    | Full of 'a
    | Empty of 'a K.t Queue.t

  type 'a t = { mutable state : 'a state }

  let create () = { state = Empty (Queue.create ()) }

  let fill t x =
    match t.state with
    | Full _ -> failwith "Fiber.Ivar.fill"
    | Empty q ->
      t.state <- Full x;
      Queue.iter q ~f:(fun k -> K.enqueue k x)

  let read t ctx k =
    match t.state with
    | Full x -> k x
    | Empty q -> Queue.push q (K.create ctx k)

  let peek t =
    match t.state with
    | Full x -> Some x
    | Empty _ -> None
end

module Mvar = struct
  type 'a t =
    { writers : ('a * unit K.t) Queue.t
    ; readers : 'a K.t Queue.t
    ; mutable value : 'a option
    }

  (* Invariant enforced on mvars. We don't actually call this function, but we
     keep it here for documentation and to help understand the
     implementation: *)
  let _invariant t =
    match t.value with
    | None -> Queue.is_empty t.writers
    | Some _ -> Queue.is_empty t.readers

  let create () =
    { value = None; writers = Queue.create (); readers = Queue.create () }

  let create_full x =
    { value = Some x; writers = Queue.create (); readers = Queue.create () }

  let read t ctx k =
    match t.value with
    | None -> Queue.push t.readers (K.create ctx k)
    | Some v -> (
      match Queue.pop t.writers with
      | None ->
        t.value <- None;
        k v
      | Some (v', w) ->
        t.value <- Some v';
        K.enqueue w ();
        k v)

  let write t x ctx k =
    match t.value with
    | Some _ -> Queue.push t.writers (x, K.create ctx k)
    | None -> (
      match Queue.pop t.readers with
      | None ->
        t.value <- Some x;
        k ()
      | Some r ->
        K.enqueue r x;
        k ())
end

module Mutex = struct
  type t =
    { mutable locked : bool
    ; mutable waiters : unit K.t Queue.t
    }

  let lock t ctx k =
    if t.locked then
      Queue.push t.waiters (K.create ctx k)
    else (
      t.locked <- true;
      k ()
    )

  let unlock t _ctx k =
    assert t.locked;
    match Queue.pop t.waiters with
    | None ->
      t.locked <- false;
      k ()
    | Some next ->
      K.enqueue next ();
      k ()

  let with_lock t f =
    let* () = lock t in
    finalize f ~finally:(fun () -> unlock t)

  let create () = { locked = false; waiters = Queue.create () }
end

module Throttle = struct
  type t =
    { mutable size : int
    ; mutable running : int
    ; waiting : unit Ivar.t Queue.t
    }

  let create size = { size; running = 0; waiting = Queue.create () }

  let size t = t.size

  let running t = t.running

  let rec restart t =
    if t.running >= t.size then
      return ()
    else
      match Queue.pop t.waiting with
      | None -> return ()
      | Some ivar ->
        t.running <- t.running + 1;
        Ivar.fill ivar ();
        restart t

  let resize t n =
    t.size <- n;
    restart t

  let run t ~f =
    finalize
      ~finally:(fun () ->
        t.running <- t.running - 1;
        restart t)
      (fun () ->
        if t.running < t.size then (
          t.running <- t.running + 1;
          f ()
        ) else
          let waiting = Ivar.create () in
          Queue.push t.waiting waiting;
          let* () = Ivar.read waiting in
          f ())
end

module Stream = struct
  module In = struct
    (* Invariant: once [read] has returned [None], it always returns [None] *)
    type nonrec 'a t =
      { mutable read : unit -> 'a option t
      ; mutable reading : bool
      }

    let create_unchecked read = { read; reading = false }

    let create read =
      let t = { read; reading = false } in
      let read () =
        let+ x = read () in
        if Option.is_none x then t.read <- (fun () -> return None);
        x
      in
      t.read <- read;
      t

    let lock t =
      if t.reading then Code_error.raise "Fiber.Stream.In: already reading" [];
      t.reading <- true

    let unlock t = t.reading <- false

    let read t =
      lock t;
      let+ x = t.read () in
      unlock t;
      x

    let empty () = create_unchecked (fun () -> return None)

    let concat (type a) (xs : a t list) =
      let remains = ref xs in
      let rec go () =
        match !remains with
        | [] -> return None
        | x :: xs -> (
          let* v = read x in
          match v with
          | Some v -> return (Some v)
          | None ->
            remains := xs;
            go ())
      in
      create go

    let append x y = concat [ x; y ]

    let of_list xs =
      let xs = ref xs in
      create_unchecked (fun () ->
          match !xs with
          | [] -> return None
          | x :: xs' ->
            xs := xs';
            return (Some x))

    let cons a t = concat [ of_list [ a ]; t ]

    let filter_map t ~f =
      let rec read () =
        t.read () >>= function
        | None ->
          unlock t;
          return None
        | Some x -> (
          match f x with
          | None -> read ()
          | Some y -> return (Some y))
      in
      lock t;
      create_unchecked read

    let sequential_iter t ~f =
      let rec loop t ~f =
        t.read () >>= function
        | None ->
          unlock t;
          return ()
        | Some x ->
          let* () = f x in
          loop t ~f
      in
      lock t;
      loop t ~f

    let parallel_iter t ~f ctx k =
      let n = ref 1 in
      let k () =
        decr n;
        if !n = 0 then (
          unlock t;
          k ()
        ) else
          EC.deref ctx
      in
      let rec loop t =
        t.read () ctx (function
          | None -> k ()
          | Some x ->
            EC.add_refs ctx 1;
            incr n;
            EC.spawn f x ctx k;
            loop t)
      in
      loop t
  end

  module Out = struct
    type nonrec 'a t =
      { mutable write : 'a option -> unit t
      ; mutable writing : bool
      }

    let lock t =
      if t.writing then Code_error.raise "Fiber.Stream.Out: already writing" [];
      t.writing <- true

    let unlock t = t.writing <- false

    let create write =
      let t = { write; writing = false } in
      let write x =
        if Option.is_none x then
          t.write <-
            (function
            | None -> return ()
            | Some _ ->
              Code_error.raise "Fiber.Stream.Out: stream output closed" []);
        write x
      in
      t.write <- write;
      t

    let write t x =
      lock t;
      let+ () = t.write x in
      unlock t

    let null () = create (fun _ -> return ())
  end

  let connect i o =
    In.lock i;
    Out.lock o;
    let rec go () =
      let* a = i.read () in
      let* () = o.write a in
      match a with
      | None ->
        In.unlock i;
        Out.unlock o;
        return ()
      | Some _ -> go ()
    in
    go ()

  let supply i o =
    In.lock i;
    Out.lock o;
    let rec go () =
      let* a = i.read () in
      match a with
      | None ->
        In.unlock i;
        Out.unlock o;
        return ()
      | Some _ ->
        let* () = o.write a in
        go ()
    in
    go ()

  let pipe () =
    let mvar = Mvar.create () in
    let i = In.create (fun () -> Mvar.read mvar) in
    let o = Out.create (fun x -> Mvar.write mvar x) in
    (i, o)
end

module Pool = struct
  type mvar =
    | Done
    | Task of (unit -> unit t)

  type status =
    | Open
    | Closed

  type t =
    { mvar : mvar Mvar.t
    ; mutable status : status
    }

  let running t =
    match t.status with
    | Open -> true
    | Closed -> false

  let create () = { mvar = Mvar.create (); status = Open }

  let task t ~f k =
    match t.status with
    | Closed ->
      Code_error.raise "pool is closed. new tasks may not be submitted" []
    | Open -> Mvar.write t.mvar (Task f) k

  let stream t =
    Stream.In.create (fun () ->
        let+ next = Mvar.read t.mvar in
        match next with
        | Done -> None
        | Task task -> Some task)

  let stop t ctx k =
    match t.status with
    | Closed -> k ()
    | Open ->
      t.status <- Closed;
      Mvar.write t.mvar Done ctx k

  let run t = stream t |> Stream.In.parallel_iter ~f:(fun task -> task ())
end

let run t ~iter = EC.run t ~iter
