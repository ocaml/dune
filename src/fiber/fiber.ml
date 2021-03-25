open! Stdune

type ('a, 'ctx) fiber = 'ctx -> ('a -> unit) -> unit

(* This module tries to enforce the following invariants:

   - the execution context passed to a continuation is the same as the current
   one

   - the execution of a fiber always ends with [deref]

   - when an exception is raised by the user code, the exception must be
   forwarded to the execution context that was active at the time the exception
   was raised

   - when an exception is raised by the user code, then we assume that the
   current fiber didn't reach the [deref] point. As a result we have to call
   [deref] at this point on the current execution context

   Remarks:

   - most of the code assumes that errors will be caught by the caller, so when
   we do a context switch, we simply change the current execution context and
   chain to the continuation without catching errors. The current [try..with]
   will catch any raised error and forward to the current execution context. The
   only place we add a [try..with] is at the toplevel or when forking. *)

let of_thunk f k = f () k

module Execution_context : sig
  type t

  type nonrec 'a fiber = ('a, t) fiber

  module K : sig
    (* Represent a suspended fiber *)
    type 'a t

    type ctx

    (* Create a continuation that captures the current execution context *)
    val create : ctx -> ('a -> unit) -> 'a t

    (* Restart a suspended fiber. [run] doesn't preserve the current execution
       context and should always be called last. *)
    val run : 'a t -> 'a -> unit
  end
  with type ctx := t

  (* Execute the current continuation, making sure to forward errors to the
     current execution context. This function doesn't preserve the current
     execution context. It should be used to execute the current continuation
     before calling [K.run] *)
  val safe_run_k : t -> ('a -> unit) -> 'a -> unit

  (* Execute a function returning a fiber, passing any raised exception to the
     current execution context. This function preserve the current execution
     context. It should be called when creating forks.*)
  val apply : ('a -> 'b fiber) -> 'a -> 'b fiber

  val vars : t -> Univ_map.t

  val apply2 : ('a -> 'b -> 'c fiber) -> 'a -> 'b -> 'c fiber

  (* Add [n] references to the current execution context *)
  val add_refs : t -> int -> unit

  (* Decrese the reference count of the current execution context *)
  val deref : t -> unit

  (* [wait_errors f] executes [f ()] inside a new execution contexts. Returns a
     fiber that terminates when all the fiber in the sub-context have
     terminated. *)
  val wait_errors : (unit -> 'a fiber) -> ('a, unit) result fiber

  (* Set the current error handler. [on_error] is called in the current
     execution context. *)
  val set_error_handler :
       on_error:(Exn_with_backtrace.t -> unit fiber)
    -> ('a -> 'b fiber)
    -> 'a
    -> 'b fiber

  val set_vars : Univ_map.t -> ('a -> 'b fiber) -> 'a -> 'b fiber

  val create : unit -> t

  val reraise_all : t -> Exn_with_backtrace.t list -> unit
end = struct
  type t =
    { on_error : Exn_with_backtrace.t k option
          (* This handler must never raise *)
    ; vars : Univ_map.t
    ; on_release : on_release
    }

  and 'a on_release_exec =
    { k : ('a, unit) result k
    ; mutable ref_count : int
    }

  and on_release =
    | Do_nothing : on_release
    | Exec : _ on_release_exec -> on_release

  and 'a k =
    { run : 'a -> unit
    ; ctx : t
    }

  type nonrec 'a fiber = ('a, t) fiber

  let vars t = t.vars

  let create () =
    { on_error = None; vars = Univ_map.empty; on_release = Do_nothing }

  let add_refs t n =
    match t.on_release with
    | Do_nothing -> ()
    | Exec r -> r.ref_count <- r.ref_count + n

  let rec safe_run_k : type a. t -> (a -> unit) -> a -> unit =
   fun t k x ->
    try k x with
    | exn -> forward_error t exn

  and forward_exn_with_bt t exn =
    match t.on_error with
    | None -> Exn_with_backtrace.reraise exn
    | Some { ctx; run } -> safe_run_k ctx run exn

  and forward_error t exn =
    let exn = Exn_with_backtrace.capture exn in
    forward_exn_with_bt t exn

  let deref t =
    match t.on_release with
    | Do_nothing -> ()
    | Exec r ->
      r.ref_count <- r.ref_count - 1;
      if r.ref_count = 0 then
        (* We need to call [safe_run_k] as we might be the in handler of the
           [try...with] block inside [apply] and so we are no more in a
           [try...with] blocks *)
        safe_run_k r.k.ctx r.k.run (Error ())
      else
        assert (r.ref_count > 0)

  let deref_finalize t k x =
    match t.on_release with
    | Do_nothing -> ()
    | Exec r ->
      r.ref_count <- r.ref_count - 1;
      assert (r.ref_count = 0);
      (* We need to call [safe_run_k] as we might be the in handler of the
         [try...with] block inside [apply] and so we are no more in a
         [try...with] blocks *)
      safe_run_k r.k.ctx k x

  let wait_errors f t k =
    let on_release = { k = { ctx = t; run = k }; ref_count = 1 } in
    let child = { t with on_release = Exec on_release } in
    f () child (fun x -> deref_finalize child k (Ok x))

  let set_error_handler :
        'a 'b.    on_error:(Exn_with_backtrace.t -> unit fiber)
        -> ('a -> 'b fiber) -> 'a -> 'b fiber =
   fun ~on_error f x t k ->
    let run exn = on_error exn t (fun () -> deref t) in
    let on_error = Some { run; ctx = t } in
    f x { t with on_error } (fun x -> k x)

  let set_vars : 'a 'b. Univ_map.t -> ('a -> 'b fiber) -> 'a -> 'b fiber =
   fun vars f x ctx k -> f x { ctx with vars } (fun x -> k x)

  module K = struct
    type 'a t = 'a k

    let create t run = { run; ctx = t }

    let run { run; ctx } x = safe_run_k ctx run x
  end

  let apply f x ctx k =
    try f x ctx k with
    | exn -> forward_error ctx exn

  let apply2 f x y ctx k =
    try f x y ctx k with
    | exn -> forward_error ctx exn

  let reraise_all t exns =
    add_refs t (List.length exns - 1);
    List.iter exns ~f:(forward_exn_with_bt t)
end

module EC = Execution_context
module K = EC.K

type 'a t = 'a EC.fiber

let return x _ k = k x

let never _ _ = ()

type ('a, 'b) fork_and_join_state =
  | Nothing_yet
  | Got_a of 'a
  | Got_b of 'b

let fork_and_join : 'a 'b. (unit -> 'a t) -> (unit -> 'b t) -> ('a * 'b) t =
 fun fa fb ctx k ->
  let state = ref Nothing_yet in
  EC.add_refs ctx 1;
  EC.apply fa () ctx (fun a ->
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
  EC.apply fa () ctx (fun () ->
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
        EC.apply f x ctx (fun y ->
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
  iter ~f:(fun x -> EC.apply f x ctx k)

let parallel_iter l ~f ctx k =
  match l with
  | [] -> k ()
  | [ x ] -> f x ctx k
  | _ -> parallel_iter_generic ~n:(List.length l) ~iter:(List.iter l) ~f ctx k

let parallel_iter_set (type a s)
    (module S : Set.S with type elt = a and type t = s) t ~(f : a -> unit t) ctx
    k =
  let len = S.cardinal t in
  match len with
  | 0 -> k ()
  | 1 -> f (Option.value_exn (S.min_elt t)) ctx k
  | n -> parallel_iter_generic ~n ~iter:(S.iter t) ~f ctx k

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
      Map.iteri t ~f:(fun x y -> EC.apply2 f x y ctx k)

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
            EC.apply2 f x y ctx (fun z ->
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

  let get var ctx k =
    let vars = EC.vars ctx in
    k (Univ_map.find vars var)

  let get_exn var ctx k =
    let vars = EC.vars ctx in
    k (Univ_map.find_exn vars var)

  let set var x f ctx k =
    let vars = EC.vars ctx in
    EC.set_vars (Univ_map.set vars var x) f () ctx k

  let unset var f ctx k =
    let vars = EC.vars ctx in
    EC.set_vars (Univ_map.remove vars var) f () ctx k

  let create () = create ~name:"var" (fun _ -> Dyn.Encoder.string "var")
end

let with_error_handler f ~on_error k = EC.set_error_handler ~on_error f () k

let wait_errors f k = EC.wait_errors f k

let map_reduce_errors (type a) (module M : Monoid with type t = a) ~on_error f =
  let acc = ref M.empty in
  let on_error exn =
    let+ m = on_error exn in
    acc := M.combine !acc m
  in
  wait_errors (fun () -> with_error_handler ~on_error f) >>| function
  | Ok _ as ok -> ok
  | Error () -> Error !acc

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

let reraise_all l =
  match l with
  | [] -> never
  | [ exn ] -> Exn_with_backtrace.reraise exn
  | exns -> fun ctx _ -> EC.reraise_all ctx exns

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

  let fill t x ctx k =
    match t.state with
    | Full _ -> failwith "Fiber.Ivar.fill"
    | Empty q ->
      t.state <- Full x;
      EC.safe_run_k ctx k ();
      Queue.iter q ~f:(fun k -> K.run k x)

  let read t ctx k =
    match t.state with
    | Full x -> k x
    | Empty q -> Queue.push q (K.create ctx k)

  let peek t _ k =
    k
      (match t.state with
      | Full x -> Some x
      | Empty _ -> None)
end

module Mvar = struct
  type 'a t =
    { writers : ('a * unit K.t) Queue.t
    ; readers : 'a K.t Queue.t
    ; mutable value : 'a option
    }

  (* Invariant enforced on mvars. We don't actually call this function, but we
     keep it here for documentation and to help understand the implementation: *)
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
        EC.safe_run_k ctx k v;
        K.run w ())

  let write t x ctx k =
    match t.value with
    | Some _ -> Queue.push t.writers (x, K.create ctx k)
    | None -> (
      match Queue.pop t.readers with
      | None ->
        t.value <- Some x;
        k ()
      | Some r ->
        EC.safe_run_k ctx k ();
        K.run r x)
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

  let unlock t ctx k =
    assert t.locked;
    match Queue.pop t.waiters with
    | None ->
      t.locked <- false;
      k ()
    | Some next ->
      EC.safe_run_k ctx k ();
      K.run next ()

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
        let* () = Ivar.fill ivar () in
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

    let of_list xs =
      let xs = ref xs in
      create_unchecked (fun () ->
          match !xs with
          | [] -> return None
          | x :: xs' ->
            xs := xs';
            return (Some x))

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
            EC.apply f x ctx k;
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

  let running t _ctx k =
    match t.status with
    | Open -> k true
    | Closed -> k false

  let create () = { mvar = Mvar.create (); status = Open }

  let task t ~f ctx k =
    match t.status with
    | Closed ->
      Code_error.raise "pool is closed. new tasks may not be submitted" []
    | Open -> Mvar.write t.mvar (Task f) ctx k

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

type fill = Fill : 'a Ivar.t * 'a -> fill

let run t ~iter =
  let ctx = EC.create () in
  let result = ref None in
  EC.apply (fun () -> t) () ctx (fun x -> result := Some x);
  let rec loop () =
    match !result with
    | Some res -> res
    | None ->
      let (Fill (ivar, v)) = iter () in
      (* We use [EC.apply] so that the current execution context is restored,
         ensuring that [iter] always run in the same execution context. *)
      EC.apply (fun () -> Ivar.fill ivar v) () ctx ignore;
      loop ()
  in
  loop ()
