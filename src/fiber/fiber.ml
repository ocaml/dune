open! Stdune

type 'a t = ('a -> eff) -> eff

and eff =
  | Read_ivar : 'a ivar * ('a -> eff) -> eff
  | Fill_ivar : 'a ivar * 'a * (unit -> eff) -> eff
  | Suspend : ('a k -> unit) * ('a -> eff) -> eff
  | Resume : 'a k * 'a * (unit -> eff) -> eff
  | Get_var : 'a Univ_map.Key.t * ('a option -> eff) -> eff
  | Set_var : 'a Univ_map.Key.t * 'a * (unit -> eff) -> eff
  | Unset_var : 'a Univ_map.Key.t * (unit -> eff) -> eff
  | With_error_handler :
      (Exn_with_backtrace.t -> Nothing.t t) * (unit -> eff)
      -> eff
  | Unwind : ('a -> eff) * 'a -> eff
  | Map_reduce_errors :
      (module Monoid with type t = 'a)
      * (Exn_with_backtrace.t -> 'a t)
      * (unit -> eff)
      * (('b, 'a) result -> eff)
      -> eff
  | Unwind_map_reduce : ('a -> eff) * 'a -> eff
  | End_of_map_reduce_error_handler : (_, _) map_reduce_context' -> eff
  | End_of_fiber of unit
  | Never of unit
  (* Add a dummy unit argument to [End_of_fiber] and [Never] so that all
     constructors are boxed, which removes a branch in the pattern match. *)
  | Fork : eff * (unit -> eff) -> eff
  | Reraise : Exn_with_backtrace.t -> eff
  | Reraise_all : Exn_with_backtrace.t list -> eff
  | Toplevel_exception : Exn_with_backtrace.t -> eff
  | Done of value

and 'a ivar = { mutable state : ('a, [ `Full | `Empty ]) ivar_state }

and ('a, _) ivar_state =
  | Full : 'a -> ('a, [> `Full ]) ivar_state
  | Empty : ('a, [> `Empty ]) ivar_state
  | Empty_with_readers :
      context * ('a -> eff) * ('a, [ `Empty ]) ivar_state
      -> ('a, [> `Empty ]) ivar_state

and value = ..

and context =
  { parent : context
  ; on_error : Exn_with_backtrace.t k
  ; vars : Univ_map.t
  ; map_reduce_context : map_reduce_context
  }

and ('a, 'b) map_reduce_context' =
  { k : ('a, 'b) result k
  ; mutable ref_count : int
  ; mutable errors : 'b
  }

(* map_reduce_context *)
and map_reduce_context =
  | Map_reduce_context : (_, _) map_reduce_context' -> map_reduce_context

and 'a k =
  { run : 'a -> eff
  ; ctx : context
  }

let return x k = k x

let bind t ~f k = t (fun x -> f x k)

let map t ~f k = t (fun x -> k (f x))

let with_error_handler f ~on_error k =
  With_error_handler (on_error, fun () -> f () (fun x -> Unwind (k, x)))

let map_reduce_errors m ~on_error f k =
  Map_reduce_errors
    (m, on_error, (fun () -> f () (fun x -> Unwind_map_reduce (k, Ok x))), k)

let suspend f k = Suspend (f, k)

let resume suspended x k = Resume (suspended, x, k)

let end_of_fiber = End_of_fiber ()

let never _k = Never ()

let apply f x =
  try f x
  with exn ->
    let exn = Exn_with_backtrace.capture exn in
    Reraise exn

let apply2 f x y =
  try f x y
  with exn ->
    let exn = Exn_with_backtrace.capture exn in
    Reraise exn

let[@inlined always] fork a b =
  match apply a () with
  | End_of_fiber () -> b ()
  | eff -> Fork (eff, b)

let rec nfork x l f =
  match l with
  | [] -> f x
  | y :: l -> (
    (* Manuall inline [fork] manually because the compiler is unfortunately not
       getting rid of the closures. *)
    match apply f x with
    | End_of_fiber () -> nfork y l f
    | eff -> Fork (eff, fun () -> nfork y l f))

let rec nforki i x l f =
  match l with
  | [] -> f i x
  | y :: l -> (
    match apply2 f i x with
    | End_of_fiber () -> nforki (i + 1) y l f
    | eff -> Fork (eff, fun () -> nforki (i + 1) y l f))

let nforki x l f = nforki 0 x l f

let rec nfork_seq left_over x (seq : _ Seq.t) f =
  match seq () with
  | Nil -> f x
  | Cons (y, seq) -> (
    incr left_over;
    match apply f x with
    | End_of_fiber () -> nfork_seq left_over y seq f
    | eff -> Fork (eff, fun () -> nfork_seq left_over y seq f))

let parallel_iter_seq (seq : _ Seq.t) ~f k =
  match seq () with
  | Nil -> k ()
  | Cons (x, seq) ->
    let left_over = ref 1 in
    let f x =
      f x (fun () ->
          decr left_over;
          if !left_over = 0 then k () else end_of_fiber)
    in
    nfork_seq left_over x seq f

type ('a, 'b) fork_and_join_state =
  | Nothing_yet
  | Got_a of 'a
  | Got_b of 'b

let fork_and_join fa fb k =
  let state = ref Nothing_yet in
  let ka a =
    match !state with
    | Nothing_yet ->
      state := Got_a a;
      end_of_fiber
    | Got_a _ -> assert false
    | Got_b b -> k (a, b)
  and kb b =
    match !state with
    | Nothing_yet ->
      state := Got_b b;
      end_of_fiber
    | Got_a a -> k (a, b)
    | Got_b _ -> assert false
  in
  match apply2 fa () ka with
  | End_of_fiber () -> fb () kb
  | eff -> Fork (eff, fun () -> fb () kb)

let fork_and_join_unit fa fb k =
  let state = ref Nothing_yet in
  match
    apply2 fa () (fun () ->
        match !state with
        | Nothing_yet ->
          state := Got_a ();
          end_of_fiber
        | Got_a _ -> assert false
        | Got_b b -> k b)
  with
  | End_of_fiber () -> fb () k
  | eff ->
    Fork
      ( eff
      , fun () ->
          fb () (fun b ->
              match !state with
              | Nothing_yet ->
                state := Got_b b;
                end_of_fiber
              | Got_a () -> k b
              | Got_b _ -> assert false) )

let rec length_and_rev l len acc =
  match l with
  | [] -> (len, acc)
  | x :: l -> length_and_rev l (len + 1) (x :: acc)

let length_and_rev l = length_and_rev l 0 []

let reraise_all l _k =
  match l with
  | [] -> Never ()
  | [ exn ] -> Exn_with_backtrace.reraise exn
  | _ -> Reraise_all l

module Ivar = struct
  type 'a t = 'a ivar

  let create () = { state = Empty }

  let read t k = Read_ivar (t, k)

  let fill t x k = Fill_ivar (t, x, k)

  let peek t k =
    k
      (match t.state with
      | Empty | Empty_with_readers _ -> None
      | Full x -> Some x)
end

module Var = struct
  include Univ_map.Key

  let get var k = Get_var (var, k)

  let get_exn var =
    map (get var) ~f:(function
      | None -> failwith "Fiber.Var.get_exn"
      | Some value -> value)

  let set var x f k = Set_var (var, x, fun () -> f () (fun x -> Unwind (k, x)))

  let unset var f k = Unset_var (var, fun () -> f () (fun x -> Unwind (k, x)))

  let create () = create ~name:"var" (fun _ -> Dyn.string "var")
end

let of_thunk f k = f () k

module O = struct
  let ( >>> ) a b k = a (fun () -> b k)

  let ( >>= ) t f k = t (fun x -> f x k)

  let ( >>| ) t f k = t (fun x -> k (f x))

  let ( let+ ) = ( >>| )

  let ( let* ) = ( >>= )

  let ( and* ) a b = fork_and_join (fun () -> a) (fun () -> b)

  let ( and+ ) = ( and* )
end

open O

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

let parallel_iter l ~f k =
  match l with
  | [] -> k ()
  | [ x ] -> f x k
  | x :: l ->
    let len = List.length l + 1 in
    let left_over = ref len in
    let f x =
      f x (fun () ->
          decr left_over;
          if !left_over = 0 then k () else end_of_fiber)
    in
    nfork x l f

let parallel_array_of_list_map' x l ~f k =
  let len = List.length l + 1 in
  let left_over = ref len in
  let results = ref [||] in
  let f i x =
    f x (fun y ->
        let a =
          match !results with
          | [||] ->
            let a = Array.make len y in
            results := a;
            a
          | a ->
            a.(i) <- y;
            a
        in
        decr left_over;
        if !left_over = 0 then k a else end_of_fiber)
  in
  nforki x l f

let parallel_array_of_list_map l ~f k =
  match l with
  | [] -> k [||]
  | [ x ] -> f x (fun x -> k [| x |])
  | x :: l -> parallel_array_of_list_map' x l ~f k

let parallel_map l ~f k =
  match l with
  | [] -> k []
  | [ x ] -> f x (fun x -> k [ x ])
  | x :: l -> parallel_array_of_list_map' x l ~f (fun a -> k (Array.to_list a))

let all = sequential_map ~f:Fun.id

let all_concurrently = parallel_map ~f:Fun.id

let all_concurrently_unit l = parallel_iter l ~f:Fun.id

let rec sequential_iter_seq (seq : _ Seq.t) ~f =
  match seq () with
  | Nil -> return ()
  | Cons (x, seq) ->
    let* () = f x in
    sequential_iter_seq seq ~f

let parallel_iter_set (type a s)
    (module S : Set.S with type elt = a and type t = s) set ~(f : a -> unit t) =
  parallel_iter_seq (S.to_seq set) ~f

let record_metrics t ~tag =
  of_thunk (fun () ->
      let timer = Metrics.Timer.start tag in
      let+ res = t in
      Metrics.Timer.stop timer;
      res)

module Make_map_traversals (Map : Map.S) = struct
  let parallel_iter t ~f =
    parallel_iter_seq (Map.to_seq t) ~f:(fun (k, v) -> f k v)

  let parallel_map t ~f =
    if Map.is_empty t then return Map.empty
    else
      let+ a =
        parallel_array_of_list_map (Map.to_list t) ~f:(fun (k, v) -> f k v)
      in
      let pos = ref 0 in
      Map.mapi t ~f:(fun _ _ ->
          let i = !pos in
          pos := i + 1;
          a.(i))
end
[@@inline always]

let rec repeat_while : 'a. f:('a -> 'a option t) -> init:'a -> unit t =
 fun ~f ~init ->
  let* result = f init in
  match result with
  | None -> return ()
  | Some init -> repeat_while ~f ~init

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

let finalize f ~finally =
  let* res1 = collect_errors f in
  let* res2 = collect_errors finally in
  let res =
    match (res1, res2) with
    | Ok x, Ok () -> Ok x
    | Error l, Ok _ | Ok _, Error l -> Error l
    | Error l1, Error l2 -> Error (l1 @ l2)
  in
  match res with
  | Ok x -> return x
  | Error l -> reraise_all l

module Mvar = struct
  type 'a t =
    { writers : ('a * unit k) Queue.t
    ; readers : 'a k Queue.t
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

  let read t k =
    match t.value with
    | None -> suspend (fun k -> Queue.push t.readers k) k
    | Some v -> (
      match Queue.pop t.writers with
      | None ->
        t.value <- None;
        k v
      | Some (v', w) ->
        t.value <- Some v';
        resume w () (fun () -> k v))

  let write t x k =
    match t.value with
    | Some _ -> suspend (fun k -> Queue.push t.writers (x, k)) k
    | None -> (
      match Queue.pop t.readers with
      | None ->
        t.value <- Some x;
        k ()
      | Some r -> resume r x (fun () -> k ()))
end

module Svar = struct
  type 'a t =
    { mutable current : 'a
    ; mutable waiters : (unit k * ('a -> bool)) list
    }

  let read t = t.current

  let wait =
    let suspend t ~until =
      suspend (fun k -> t.waiters <- (k, until) :: t.waiters)
    in
    let rec wait t ~until =
      if until t.current then return ()
      else
        let* () = suspend t ~until in
        wait t ~until
    in
    fun t ~until -> wait t ~until

  let create current = { current; waiters = [] }

  let write =
    let rec run_awakers final = function
      | [] -> final ()
      | k :: ks -> resume k () (fun () -> run_awakers final ks)
    in
    fun t a k ->
      t.current <- a;
      let sleep, awake =
        List.rev_partition_map t.waiters ~f:(fun (k, f) ->
            if f t.current then Right k else Left (k, f))
      in
      match awake with
      | [] -> k ()
      | awake ->
        t.waiters <- List.rev sleep;
        run_awakers k awake
end

module Mutex = struct
  type t =
    { mutable locked : bool
    ; mutable waiters : unit k Queue.t
    }

  let lock t k =
    if t.locked then suspend (fun k -> Queue.push t.waiters k) k
    else (
      t.locked <- true;
      k ())

  let unlock t k =
    assert t.locked;
    match Queue.pop t.waiters with
    | None ->
      t.locked <- false;
      k ()
    | Some next -> resume next () k

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
    if t.running >= t.size then return ()
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
          f ())
        else
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

    let parallel_iter t ~f k =
      let n = ref 1 in
      let k () =
        decr n;
        if !n = 0 then (
          unlock t;
          k ())
        else end_of_fiber
      in
      let rec loop t =
        t.read () (function
          | None -> k ()
          | Some x ->
            incr n;
            fork (fun () -> f x k) (fun () -> loop t))
      in
      lock t;
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

  let running t k =
    match t.status with
    | Open -> k true
    | Closed -> k false

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

  let stop t k =
    match t.status with
    | Closed -> k ()
    | Open ->
      t.status <- Closed;
      Mvar.write t.mvar Done k

  let run t = stream t |> Stream.In.parallel_iter ~f:(fun task -> task ())
end

type fill = Fill : 'a ivar * 'a -> fill

module Jobs = struct
  type t =
    | Empty
    | Job : context * ('a -> eff) * 'a * t -> t
    | Concat : t * t -> t

  let concat a b =
    match (a, b) with
    | Empty, x | x, Empty -> x
    | _ -> Concat (a, b)

  let rec enqueue_readers (readers : (_, [ `Empty ]) ivar_state) x jobs =
    match readers with
    | Empty -> jobs
    | Empty_with_readers (ctx, k, readers) ->
      enqueue_readers readers x (Job (ctx, k, x, jobs))

  let fill_ivar ivar x jobs =
    match ivar.state with
    | Full _ -> failwith "Fiber.Ivar.fill"
    | (Empty | Empty_with_readers _) as readers ->
      ivar.state <- Full x;
      enqueue_readers readers x jobs

  let rec exec_fills fills acc =
    match fills with
    | [] -> acc
    | Fill (ivar, x) :: fills ->
      let acc = fill_ivar ivar x acc in
      exec_fills fills acc

  let exec_fills fills = exec_fills (List.rev fills) Empty
end

module Scheduler = struct
  type step' =
    | Done of value
    | Stalled

  module type Witness = sig
    type t

    type value += X of t
  end

  type 'a stalled = (module Witness with type t = 'a)

  type 'a step =
    | Done of 'a
    | Stalled of 'a stalled

  let rec loop : Jobs.t -> step' = function
    | Empty -> Stalled
    | Job (ctx, run, x, jobs) -> exec ctx run x jobs
    | Concat (a, b) -> loop2 a b

  and loop2 a b =
    match a with
    | Empty -> loop b
    | Job (ctx, run, x, a) -> exec ctx run x (Jobs.concat a b)
    | Concat (a1, a2) -> loop2 a1 (Jobs.concat a2 b)

  and exec : 'a. context -> ('a -> eff) -> 'a -> Jobs.t -> step' =
   fun ctx k x jobs ->
    match k x with
    | exception exn ->
      let exn = Exn_with_backtrace.capture exn in
      exec ctx.on_error.ctx ctx.on_error.run exn jobs
    | Done v -> Done v
    | Toplevel_exception exn -> Exn_with_backtrace.reraise exn
    | Unwind (k, x) -> exec ctx.parent k x jobs
    | Read_ivar (ivar, k) -> (
      match ivar.state with
      | (Empty | Empty_with_readers _) as readers ->
        ivar.state <- Empty_with_readers (ctx, k, readers);
        loop jobs
      | Full x -> exec ctx k x jobs)
    | Fill_ivar (ivar, x, k) ->
      let jobs = Jobs.concat jobs (Jobs.fill_ivar ivar x Empty) in
      exec ctx k () jobs
    | Suspend (f, k) ->
      let k = { ctx; run = k } in
      f k;
      loop jobs
    | Resume (suspended, x, k) ->
      exec ctx k ()
        (Jobs.concat jobs (Job (suspended.ctx, suspended.run, x, Empty)))
    | Get_var (key, k) -> exec ctx k (Univ_map.find ctx.vars key) jobs
    | Set_var (key, x, k) ->
      let ctx = { ctx with parent = ctx; vars = Univ_map.set ctx.vars key x } in
      exec ctx k () jobs
    | Unset_var (key, k) ->
      let ctx =
        { ctx with parent = ctx; vars = Univ_map.remove ctx.vars key }
      in
      exec ctx k () jobs
    | With_error_handler (on_error, k) ->
      let on_error =
        { ctx; run = (fun exn -> on_error exn Nothing.unreachable_code) }
      in
      let ctx = { ctx with parent = ctx; on_error } in
      exec ctx k () jobs
    | Map_reduce_errors (m, on_error, f, k) ->
      map_reduce_errors ctx m on_error f k jobs
    | End_of_fiber () ->
      let (Map_reduce_context r) = ctx.map_reduce_context in
      deref r jobs
    | Unwind_map_reduce (k, x) ->
      let (Map_reduce_context r) = ctx.map_reduce_context in
      let ref_count = r.ref_count - 1 in
      r.ref_count <- ref_count;
      assert (ref_count = 0);
      exec ctx.parent k x jobs
    | End_of_map_reduce_error_handler map_reduce_context ->
      deref map_reduce_context jobs
    | Never () -> loop jobs
    | Fork (a, b) ->
      let (Map_reduce_context r) = ctx.map_reduce_context in
      r.ref_count <- r.ref_count + 1;
      exec ctx Fun.id a (Job (ctx, b, (), jobs))
    | Reraise exn ->
      let { ctx; run } = ctx.on_error in
      exec ctx run exn jobs
    | Reraise_all exns -> (
      match length_and_rev exns with
      | 0, _ -> loop jobs
      | n, exns ->
        let (Map_reduce_context r) = ctx.map_reduce_context in
        r.ref_count <- r.ref_count + (n - 1);
        let { ctx; run } = ctx.on_error in
        let jobs =
          List.fold_left exns ~init:jobs ~f:(fun jobs exn ->
              Jobs.Job (ctx, run, exn, jobs))
        in
        loop jobs)

  and deref : 'a 'b. ('a, 'b) map_reduce_context' -> Jobs.t -> step' =
   fun r jobs ->
    let ref_count = r.ref_count - 1 in
    r.ref_count <- ref_count;
    match ref_count with
    | 0 -> exec r.k.ctx r.k.run (Error r.errors) jobs
    | _ ->
      assert (ref_count > 0);
      loop jobs

  and map_reduce_errors :
      type errors b.
         context
      -> (module Monoid with type t = errors)
      -> (Exn_with_backtrace.t -> errors t)
      -> (unit -> eff)
      -> ((b, errors) result -> eff)
      -> Jobs.t
      -> step' =
   fun ctx (module M : Monoid with type t = errors) on_error f k jobs ->
    let map_reduce_context =
      { k = { ctx; run = k }; ref_count = 1; errors = M.empty }
    in
    let on_error =
      { ctx
      ; run =
          (fun exn ->
            on_error exn (fun m ->
                map_reduce_context.errors <-
                  M.combine map_reduce_context.errors m;
                End_of_map_reduce_error_handler map_reduce_context))
      }
    in
    let ctx =
      { ctx with
        parent = ctx
      ; on_error
      ; map_reduce_context = Map_reduce_context map_reduce_context
      }
    in
    exec ctx f () jobs

  let repack_step (type a) (module W : Witness with type t = a) (step' : step')
      =
    match step' with
    | Done (W.X a) -> Done a
    | Done _ ->
      Code_error.raise
        "advance: it's illegal to call advance with a fiber created in a \
         different scheduler"
        []
    | Stalled -> Stalled (module W)

  let advance (type a) (module W : Witness with type t = a) fill : a step =
    fill |> Nonempty_list.to_list |> Jobs.exec_fills |> loop
    |> repack_step (module W)

  let start (type a) (t : a t) =
    let module W = struct
      type t = a

      type value += X of a
    end in
    let rec ctx =
      { parent = ctx
      ; on_error = { ctx; run = (fun exn -> Toplevel_exception exn) }
      ; vars = Univ_map.empty
      ; map_reduce_context =
          Map_reduce_context
            { k = { ctx; run = (fun _ -> assert false) }
            ; ref_count = 1
            ; errors = ()
            }
      }
    in
    exec ctx t (fun x -> Done (W.X x)) Empty |> repack_step (module W)
end

let run =
  let rec loop ~iter (s : _ Scheduler.step) =
    match s with
    | Done a -> a
    | Stalled w -> loop ~iter (Scheduler.advance w (iter ()))
  in
  fun t ~iter -> loop ~iter (Scheduler.start t)

module Expert = struct
  type nonrec 'a k = 'a k

  let suspend f k = suspend f k

  let resume a x k = resume a x k
end
