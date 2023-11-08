open Stdune
open Fiber.O

module Fd = struct
  type t = Unix.file_descr

  let equal = Poly.equal
  let hash = Poly.hash
  let to_dyn = Dyn.opaque
end

module type Scheduler = sig
  val fill_jobs : Fiber.fill list -> unit
  val register_job_started : unit -> unit
  val cancel_job_started : unit -> unit
  val spawn_thread : (unit -> unit) -> unit
end

let byte = Bytes.make 1 '0'

module Task_id = Id.Make ()

type t =
  { readers : (Unix.file_descr, packed_task Queue.t) Table.t
  ; writers : (Unix.file_descr, packed_task Queue.t) Table.t
  ; mutable to_close : Unix.file_descr list
  ; pipe_read : Unix.file_descr
  ; (* write a byte here to interrupt the select loop *)
    pipe_write : Unix.file_descr
  ; mutex : Mutex.t
  ; scheduler : (module Scheduler)
  ; mutable running : bool
  ; mutable started : bool
  ; (* this flag is to save a write to the pipe we used to interrupt select *)
    mutable interrupting : bool
  ; pipe_buf : Bytes.t
  }

and ('a, 'label) task =
  { job : 'label -> Unix.file_descr -> 'a
  ; ivar : ('a, [ `Cancelled | `Exn of exn ]) result Fiber.Ivar.t
  ; select : t
  ; what : [ `Read | `Write ]
  ; fds : Unix.file_descr list
  ; id : Task_id.t
  ; mutable status : [ `Filled | `Waiting ]
  }

and packed_task = Task : (_, 'label) task * 'label -> packed_task

let interrupt t =
  if not t.interrupting
  then (
    assert (Unix.single_write t.pipe_write byte 0 1 = 1);
    t.interrupting <- true)
;;

let filter_queue q ~f =
  let new_q = Queue.create () in
  Queue.iter q ~f:(fun x -> if f x then Queue.push new_q x);
  new_q
;;

module Task = struct
  type 'a t = User_task : ('a, 'label) task -> 'a t

  let await (User_task task) = Fiber.Ivar.read task.ivar

  let cancel (User_task t) =
    let* () = Fiber.return () in
    Mutex.lock t.select.mutex;
    let+ () =
      match t.status with
      | `Filled -> Fiber.return ()
      | `Waiting ->
        let table =
          match t.what with
          | `Read -> t.select.readers
          | `Write -> t.select.writers
        in
        let should_interrupt =
          let should_interrupt = ref false in
          List.iter t.fds ~f:(fun fd ->
            match Table.find table fd with
            | None -> ()
            | Some q ->
              let new_q =
                filter_queue q ~f:(fun (Task (t', _)) -> not (Task_id.equal t.id t'.id))
              in
              should_interrupt
              := !should_interrupt || Queue.length new_q <> Queue.length q;
              if Queue.is_empty new_q
              then Table.remove table fd
              else Table.set table fd new_q);
          !should_interrupt
        in
        if should_interrupt then interrupt t.select;
        let module Scheduler = (val t.select.scheduler) in
        t.status <- `Filled;
        Scheduler.cancel_job_started ();
        Fiber.Ivar.fill t.ivar (Error `Cancelled)
    in
    Mutex.unlock t.select.mutex
  ;;
end

let cleanup_other_tasks waiters task =
  List.iter task.fds ~f:(fun fd ->
    match Table.find waiters fd with
    | None ->
      (* XXX this should be impossible? *)
      ()
    | Some q ->
      let new_q =
        filter_queue q ~f:(fun (Task (t, _)) -> not (Task_id.equal t.id task.id))
      in
      if Queue.is_empty new_q then Table.remove waiters fd else Table.set waiters fd new_q)
;;

let drain_until_ready fd waiters queue acc =
  match Queue.pop queue with
  | None -> acc
  | Some (Task (task, label)) ->
    let result =
      try Ok (task.job label fd) with
      | exn -> Error (`Exn exn)
    in
    task.status <- `Filled;
    cleanup_other_tasks waiters task;
    Fiber.Fill (task.ivar, result) :: acc
;;

let make_fills fds pipe_fd waiters init =
  List.fold_left fds ~init:(false, init) ~f:(fun (pipe, acc) fd ->
    if Fd.equal fd pipe_fd
    then true, acc
    else (
      let acc =
        match Table.find waiters fd with
        | None -> acc
        | Some w ->
          let acc = drain_until_ready fd waiters w acc in
          if Queue.is_empty w then Table.remove waiters fd;
          acc
      in
      pipe, acc))
;;

let drain_pipe pipe buf =
  match Unix.read pipe buf 0 (Bytes.length buf) with
  | _ -> ()
  | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
;;

let rec drain_cancel q acc =
  match Queue.pop q with
  | None -> acc
  | Some (Task (task, _)) ->
    drain_cancel q (Fiber.Fill (task.ivar, Error `Cancelled) :: acc)
;;

let maybe_cancel table fd acc =
  match Table.find table fd with
  | None -> acc
  | Some q ->
    Table.remove table fd;
    drain_cancel q acc
;;

(* This exception is raised in [select_loop] whenever it fails and the mutex is
   unlocked. This makes sure that we don't unlock it again after [select_loop]
   finishes *)
exception Unlocked of Exn_with_backtrace.t

let rec select_loop t =
  (match t.to_close with
   | [] -> ()
   | to_close ->
     let fills =
       List.fold_left to_close ~init:[] ~f:(fun acc fd ->
         Unix.close fd;
         let acc = maybe_cancel t.readers fd acc in
         maybe_cancel t.writers fd acc)
     in
     t.to_close <- [];
     (match fills with
      | [] -> ()
      | _ :: _ ->
        let module Scheduler = (val t.scheduler) in
        Scheduler.fill_jobs fills));
  match t.running with
  | false ->
    Unix.close t.pipe_write;
    if not Sys.win32
    then
      Unix.close t.pipe_read
      (* On Win32, both ends of the "pipe" are the same UDP socket *)
  | true ->
    let read = t.pipe_read :: Table.keys t.readers in
    let write = Table.keys t.writers in
    Mutex.unlock t.mutex;
    (* At this point, if any [ready] acquires the lock, they need to check if
       [read] or [write] contain their fd. If it doesn't, the write
       [t.pipe_write] will interrupt this select *)
    (match Unix.select read write [] (-1.0) with
     | exception Unix.Unix_error (Unix.(EINTR | EAGAIN), _, _) ->
       Mutex.lock t.mutex;
       select_loop t
     | exception exn ->
       let exn = Exn_with_backtrace.capture exn in
       raise_notrace (Unlocked exn)
     | readers, writers, ex ->
       assert (ex = []);
       (* Before we acquire the lock, it's possible that new tasks were added.
          This is fine. *)
       Mutex.lock t.mutex;
       let seen_pipe, fills = make_fills readers t.pipe_read t.readers [] in
       (* we will never see [t.pipe_read] in the next list, but there's no harm in
          this *)
       let _, fills = make_fills writers t.pipe_read t.writers fills in
       if seen_pipe
       then (
         drain_pipe t.pipe_read t.pipe_buf;
         t.interrupting <- false);
       (match fills with
        | [] -> ()
        | _ :: _ ->
          let module Scheduler = (val t.scheduler) in
          Scheduler.fill_jobs fills);
       select_loop t)
;;

let start t =
  let module Scheduler = (val t.scheduler : Scheduler) in
  Scheduler.spawn_thread (fun () ->
    Mutex.lock t.mutex;
    match select_loop t with
    | () -> Mutex.unlock t.mutex
    | exception Unlocked exn -> Exn_with_backtrace.reraise exn
    | exception exn ->
      Mutex.unlock t.mutex;
      reraise exn)
;;

module T_var : sig
  (** Wrap the global t_var so that it is started whenever requested. *)

  val get_exn : unit -> t Fiber.t
  val setup : t -> (unit -> 'a Fiber.t) -> 'a Fiber.t
end = struct
  let t_var = Fiber.Var.create ()

  let get_exn () =
    let+ t = Fiber.Var.get_exn t_var in
    if not t.started
    then (
      start t;
      t.started <- true);
    t
  ;;

  let setup t f =
    Fiber.Var.set t_var t (fun () ->
      Fiber.finalize f ~finally:(fun () ->
        if t.started
        then (
          Mutex.lock t.mutex;
          t.running <- false;
          interrupt t;
          Mutex.unlock t.mutex);
        Fiber.return ()))
  ;;
end

let with_io scheduler f =
  let t =
    let pipe_read, pipe_write =
      if not Sys.win32
      then Unix.pipe ~cloexec:true ()
      else (
        (* Create a self-connected UDP socket *)
        let udp_sock = Unix.socket ~cloexec:true PF_INET SOCK_DGRAM 0 in
        Unix.bind udp_sock (ADDR_INET (Unix.inet_addr_loopback, 0));
        Unix.connect udp_sock (Unix.getsockname udp_sock);
        udp_sock, udp_sock)
    in
    Unix.set_nonblock pipe_read;
    { readers = Table.create (module Fd) 64
    ; writers = Table.create (module Fd) 64
    ; mutex = Mutex.create ()
    ; scheduler
    ; running = true
    ; pipe_read
    ; pipe_write
    ; pipe_buf = Bytes.create 512
    ; interrupting = false
    ; to_close = []
    ; started = false
    }
  in
  T_var.setup t f
;;

let with_ f =
  let+ t = T_var.get_exn () in
  Mutex.lock t.mutex;
  Exn.protect ~f:(fun () -> f t) ~finally:(fun () -> Mutex.unlock t.mutex)
;;

let cancel_fd scheduler table fd =
  match Table.find table fd with
  | None -> Fiber.return ()
  | Some tasks ->
    Table.remove table fd;
    let module Scheduler = (val scheduler : Scheduler) in
    Queue.to_list tasks
    |> Fiber.parallel_iter ~f:(fun (Task (t, _)) ->
      Scheduler.cancel_job_started ();
      Fiber.Ivar.fill t.ivar (Error `Cancelled))
;;

let close fd =
  let* t = T_var.get_exn () in
  Mutex.lock t.mutex;
  (* everything below is guaranteed not to raise so the mutex will be unlocked
     in the end. There's no need to use [protect] to make sure we don't deadlock *)
  t.to_close <- fd :: t.to_close;
  let+ () =
    Fiber.fork_and_join_unit
      (fun () -> cancel_fd t.scheduler t.readers fd)
      (fun () -> cancel_fd t.scheduler t.writers fd)
  in
  interrupt t;
  Mutex.unlock t.mutex
;;

let ready_one (type a label) (fds : (label * Unix.file_descr) list) what ~f:job
  : a Task.t Fiber.t
  =
  with_
  @@ fun t ->
  let module Scheduler = (val t.scheduler) in
  Scheduler.register_job_started ();
  let ivar = Fiber.Ivar.create () in
  let queues, skip_interrupt =
    let table =
      match what with
      | `Read -> t.readers
      | `Write -> t.writers
    in
    let skip_interrupt = ref false in
    ( List.map fds ~f:(fun (label, fd) ->
        let q =
          match Table.find table fd with
          | Some q -> q
          | None ->
            let q = Queue.create () in
            Table.add_exn table fd q;
            skip_interrupt := false;
            q
        in
        label, q)
    , !skip_interrupt )
  in
  let task =
    { ivar
    ; select = t
    ; job
    ; what
    ; id = Task_id.gen ()
    ; fds = List.map fds ~f:snd
    ; status = `Waiting
    }
  in
  List.iter queues ~f:(fun (label, q) -> Queue.push q (Task (task, label)));
  if not skip_interrupt then interrupt t;
  Task.User_task task
;;

let ready fd what ~f:job = ready_one [ (), fd ] what ~f:(fun () _fd -> job ())

let rec with_retry f fd =
  match f () with
  | () -> Fiber.return (Ok ())
  | exception Unix.Unix_error (EWOULDBLOCK, x, y) when Sys.win32 ->
    Fiber.return (Error (`Unix (Unix.EINPROGRESS, x, y)))
  | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
    let* task = ready fd `Write ~f:Fun.id in
    Task.await task
    >>= (function
     | Ok () -> with_retry f fd
     | Error `Cancelled as e -> Fiber.return e
     | Error (`Exn _) -> assert false)
  | exception Unix.Unix_error (err, x, y) -> Fiber.return (Error (`Unix (err, x, y)))
;;

let connect f fd socket =
  let* () = Fiber.return () in
  with_retry (fun () -> f fd socket) fd
  >>= function
  | Ok () -> Fiber.return (Ok ())
  | Error (`Unix (Unix.EISCONN, _, _)) when Sys.win32 -> Fiber.return (Ok ())
  | Error (`Unix (EINPROGRESS, _, _)) ->
    let* task = ready fd `Write ~f:(fun () -> Unix.getsockopt_error fd) in
    Task.await task
    >>| (function
     | Error _ as e -> e
     | Ok None -> Ok ()
     | Ok (Some err) -> Error (`Exn (Unix.Unix_error (err, "connect", ""))))
  | Error (`Unix (e, x, y)) -> Fiber.return @@ Error (`Exn (Unix.Unix_error (e, x, y)))
  | Error (`Exn _) as e -> Fiber.return e
  | Error `Cancelled as e -> Fiber.return e
;;
