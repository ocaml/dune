open! Stdune

(* Invariant: the execution contxt passed to the continuation is the
   same as the current one *)
type 'a t = ('a -> unit) -> unit

let of_thunk f k = f () k

module Execution_context : sig
  module K : sig
    type 'a t

    (* Create a continuation that captures the current execution
       context *)
    val create : ('a -> unit) -> 'a t
    val run : 'a t -> 'a -> unit
    val run_queue : 'a t Queue.t -> 'a -> unit
    val run_list : 'a t list -> 'a -> unit
  end

  (* Execute a function returning a fiber, passing any raised excetion
     to the current execution context. [apply] is guaranteed to not
     raise. *)
  val apply : ('a -> 'b t) -> 'a -> 'b t

  (* Add [n] references to the current execution context *)
  val add_refs : int -> unit

  (* Decrese the reference count of the current execution context *)
  val deref : unit -> unit

  (* [fork_and_wait_errors f x] executes [f x] inside a new execution
     contexts.  Returns a fiber that terminates when all the fiber in
     the sub-context have terminated. *)
  val fork_and_wait_errors : ('a -> 'b t) -> 'a -> ('b, unit) result t

  (* Set the current error handler. [on_error] is called in the
     current execution context. *)
  val set_error_handler :
    on_error:(Exn_with_backtrace.t -> unit) -> ('a -> 'b t) -> 'a -> 'b t

  val vars : unit -> Univ_map.t
  val set_vars : Univ_map.t -> ('a -> 'b t) -> 'a -> 'b t
  val set_vars_sync : Univ_map.t -> ('a -> 'b) -> 'a -> 'b
end = struct
  type t =
    { on_error : Exn_with_backtrace.t k option (* This handler must never raise *)
    ; fibers   : int ref (* Number of fibers running in this execution
                            context *)
    ; vars     : Univ_map.t
    ; on_release : unit k option
    }

  and 'a k =
    { run : 'a -> unit
    ; ctx : t
    }

  let current =
    ref { on_error   = None
        ; fibers     = ref 1
        ; vars       = Univ_map.empty
        ; on_release = None
        }

  let add_refs n =
    let t = !current in
    t.fibers := !(t.fibers) + n

  external sys_exit : int -> _ = "caml_sys_exit"

  let rec forward_error t exn =
    match t.on_error with
    | None ->
      (* We can't let the exception leak at this point, so we just
         dump the error on stderr and exit *)
      Format.eprintf "%a@.%!" Exn_with_backtrace.pp_uncaught exn;
      sys_exit 42
    | Some { ctx; run } ->
      current := ctx;
      try
        run exn
      with exn ->
        let exn = Exn_with_backtrace.capture exn in
        forward_error ctx exn

  let rec deref t =
    let n = !(t.fibers) - 1 in
    assert (n >= 0);
    t.fibers := n;
    if n = 0 then
      match t.on_release with
      | None -> ()
      | Some h -> run_k h ()

  and run_k : type a. a k -> a -> unit = fun k x ->
    current := k.ctx;
    try
      k.run x
    with exn ->
      let exn = Exn_with_backtrace.capture exn in
      forward_error k.ctx exn;
      deref k.ctx

  let exec_in ~parent ~child f x k =
    let k x =
      current := parent;
      k x
    in
    current := child;
    (try
       f x k
     with exn ->
       let exn = Exn_with_backtrace.capture exn in
       forward_error child exn;
       deref child);
    current := parent

  let fork_and_wait_errors f x k =
    let t = !current in
    let result = ref (Result.Error ()) in
    let on_release =
      Some { ctx = t
           ; run = fun () -> k !result
           }
    in
    let child = { t with on_release; fibers = ref 1 } in
    exec_in ~parent:t ~child f x
      (fun x -> result := Ok x; deref child)

  let set_error_handler ~on_error f x k =
    let t = !current in
    let on_error = Some { run = on_error; ctx = t } in
    exec_in ~parent:t ~child:{ t with on_error } f x k

  let vars () = (!current).vars
  let set_vars vars f x k =
    let t = !current in
    exec_in ~parent:t ~child:{ t with vars } f x k
  let set_vars_sync (type b) vars f x : b =
    let t = !current in
    current := { t with vars };
    Exn.protect ~finally:(fun () -> current := t) ~f:(fun () -> f x)

  module K = struct
    type 'a t = 'a k

    let create run =
      { run; ctx = !current }

    let run { run; ctx } x =
      let backup = !current in
      current := ctx;
      (try
         run x
       with exn ->
         let exn = Exn_with_backtrace.capture exn in
         forward_error ctx exn;
         deref ctx);
      current := backup

    let run_queue q x =
      let backup = !current in
      Queue.iter (fun k -> run_k k x) q;
      current := backup

    let run_list l x =
      let backup = !current in
      List.iter l ~f:(fun k -> run_k k x);
      current := backup
  end

  let apply f x k =
    let t = !current in
    try
      f x k
    with exn ->
      let exn = Exn_with_backtrace.capture exn in
      forward_error t exn;
      deref t;
      current := t

  let deref () = deref !current
end

module EC = Execution_context
module K = EC.K

let return x k = k x

let never _ = ()

module O = struct
  let (>>>) a b k =
    a (fun () -> b k)

  let (>>=) t f k =
    t (fun x -> f x k)

  let (>>|) t f k =
    t (fun x -> k (f x))

  let ( let+ ) = ( >>| )
  let ( let* ) = ( >>= )
end

open O

let map t ~f = t >>| f
let bind t ~f = t >>= f

let both a b =
  a >>= fun x ->
  b >>= fun y ->
  return (x, y)

let all l =
  let rec loop l acc =
    match l with
    | [] -> return (List.rev acc)
    | t :: l -> t >>= fun x -> loop l (x :: acc)
  in
  loop l []

let all_unit l = List.fold_left l ~init:(return ()) ~f:(>>>)

let map_all l ~f =
  let rec loop l acc =
    match l with
    | [] -> return (List.rev acc)
    | x :: l -> f x >>= fun x -> loop l (x :: acc)
  in
  loop l []

let map_all_unit l ~f =
  let rec loop l =
    match l with
    | [] -> return ()
    | x :: l -> f x >>= fun () -> loop l
  in
  loop l

type ('a, 'b) fork_and_join_state =
  | Nothing_yet
  | Got_a of 'a
  | Got_b of 'b

let fork_and_join fa fb k =
  let state = ref Nothing_yet in
  EC.add_refs 1;
  EC.apply fa () (fun a ->
    match !state with
    | Nothing_yet -> EC.deref (); state := Got_a a
    | Got_a _ -> assert false
    | Got_b b -> k (a, b));
  fb () (fun b ->
    match !state with
    | Nothing_yet -> EC.deref (); state := Got_b b
    | Got_a a -> k (a, b)
    | Got_b _ -> assert false)

let fork_and_join_unit fa fb k =
  let state = ref Nothing_yet in
  EC.add_refs 1;
  EC.apply fa () (fun () ->
    match !state with
    | Nothing_yet -> EC.deref (); state := Got_a ()
    | Got_a _ -> assert false
    | Got_b b -> k b);
  fb () (fun b ->
    match !state with
    | Nothing_yet -> EC.deref (); state := Got_b b
    | Got_a () -> k b
    | Got_b _ -> assert false)

let list_of_option_array =
  let rec loop arr i acc =
    if i = 0 then
      acc
    else
      let i = i - 1 in
      match arr.(i) with
      | None -> assert false
      | Some x ->
        loop arr i (x :: acc)
  in
  fun a -> loop a (Array.length a) []

let parallel_map l ~f k =
  match l with
  | [] -> k []
  | [x] -> f x (fun x -> k [x])
  | _ ->
    let n = List.length l in
    EC.add_refs (n - 1);
    let left_over = ref n in
    let results = Array.make n None in
    List.iteri l ~f:(fun i x ->
      EC.apply f x (fun y ->
        results.(i) <- Some y;
        decr left_over;
        if !left_over = 0 then
          k (list_of_option_array results)
        else
          EC.deref ()))

let parallel_iter l ~f k =
  match l with
  | [] -> k ()
  | [x] -> f x k
  | _ ->
    let n = List.length l in
    EC.add_refs (n - 1);
    let left_over = ref n in
    let k () =
      decr left_over;
      if !left_over = 0 then k () else EC.deref ()
    in
    List.iter l ~f:(fun x -> EC.apply f x k)

module Var = struct
  include Univ_map.Key

  let get     var = Univ_map.find     (EC.vars ()) var
  let get_exn var = Univ_map.find_exn (EC.vars ()) var

  let set_sync var x f =
    EC.set_vars_sync (Univ_map.add (EC.vars ()) var x) f ()

  let set var x f k =
    EC.set_vars (Univ_map.add (EC.vars ()) var x) f () k

  let create () =
    create ~name:"var" (fun _ -> Sexp.Encoder.string "var")
end

let with_error_handler f ~on_error k =
  EC.set_error_handler ~on_error f () k

let wait_errors f k = EC.fork_and_wait_errors f () k

let fold_errors f ~init ~on_error =
  let acc = ref init in
  let on_error exn =
    acc := on_error exn !acc
  in
  wait_errors (fun () -> with_error_handler ~on_error f)
  >>| function
  | Ok _ as ok -> ok
  | Error () -> Error !acc

let collect_errors f =
  fold_errors f
    ~init:[]
    ~on_error:(fun e l -> e :: l)

let finalize f ~finally =
  wait_errors f >>= fun res ->
  finally () >>= fun () ->
  match res with
  | Ok x -> return x
  | Error () -> never

module Ivar = struct
  type 'a state =
    | Full  of 'a
    | Empty of 'a K.t Queue.t

  type 'a t = { mutable state : 'a state }

  let create () = { state = Empty (Queue.create ()) }

  let fill t x k =
    match t.state with
    | Full  _ -> failwith "Fiber.Ivar.fill"
    | Empty q ->
      t.state <- Full x;
      K.run_queue q x;
      k ()

  let read t k =
    match t.state with
    | Full  x -> k x
    | Empty q ->
      Queue.push (K.create k) q

  let peek t =
    match t.state with
    | Full  x -> Some x
    | Empty _ -> None
end

module Future = struct
  type 'a t = 'a Ivar.t

  let wait = Ivar.read
  let peek = Ivar.peek
end

let fork f k =
  let ivar = Ivar.create () in
  EC.add_refs 1;
  EC.apply f () (fun x -> Ivar.fill ivar x ignore);
  k ivar

let nfork_map l ~f k =
  match l with
  | [] -> k []
  | [x] -> fork (fun () -> f x) (fun ivar -> k [ivar])
  | l ->
    let n = List.length l in
    EC.add_refs (n - 1);
    let ivars =
      List.map l ~f:(fun x ->
        let ivar = Ivar.create () in
        EC.apply f x (fun x -> Ivar.fill ivar x ignore);
        ivar)
    in
    k ivars

let nfork l : _ Future.t list t = nfork_map l ~f:(fun f -> f ())

module Once = struct
  type 'a state =
    | Running of 'a Future.t
    | Not_started of (unit -> 'a t)
    | Starting

  type 'a t = { mutable state : 'a state }

  let create f = { state = Not_started f }

  let get t =
    match t.state with
    | Running fut -> Future.wait fut
    | Not_started f ->
      t.state <- Starting;
      fork f >>= fun fut ->
      t.state <- Running fut;
      Future.wait fut
    | Starting ->
      failwith "Fiber.Once.get: recursive evaluation"

  let peek t =
    match t.state with
    | Running fut -> Future.peek fut
    | _ -> None

  let peek_exn t = Option.value_exn (peek t)
end

module Mutex = struct
  type t =
    { mutable locked  : bool
    ; mutable waiters : unit K.t Queue.t
    }

  let lock t k =
    if t.locked then
      Queue.push (K.create k) t.waiters
    else begin
      t.locked <- true;
      k ()
    end

  let unlock t k =
    assert t.locked;
    if Queue.is_empty t.waiters then
      t.locked <- false
    else
      K.run (Queue.pop t.waiters) ();
    k ()

  let with_lock t f =
    lock t >>= fun () ->
    finalize f ~finally:(fun () -> unlock t)

  let create () =
    { locked  = false
    ; waiters = Queue.create ()
    }
end

let suspended = ref []

let yield () k = suspended := K.create k :: !suspended

exception Never

let run t =
  let result = ref None in
  EC.apply (fun () -> t) () (fun x -> result := Some x);
  let rec loop () =
    match List.rev !suspended with
    | [] ->
      (match !result with
       | None -> raise Never
       | Some x -> x)
    | to_run ->
      suspended := [];
      K.run_list to_run ();
      loop ()
  in
  loop ()


