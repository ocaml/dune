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
  ; (* this flag is to save a write to the pipe we used to interrupt select *)
    mutable interrupting : bool
  ; pipe_buf : Bytes.t
  }

and 'a task =
  { job : unit -> 'a
  ; ivar : ('a, [ `Cancelled | `Exn of exn ]) result Fiber.Ivar.t
  ; select : t
  ; what : [ `Read | `Write ]
  ; fd : Unix.file_descr
  ; id : Task_id.t
  }

and packed_task = Task : _ task -> packed_task

let interrupt t =
  if not t.interrupting then (
    assert (Unix.single_write t.pipe_write byte 0 1 = 1);
    t.interrupting <- true)

module Task = struct
  type 'a t = 'a task

  let await task = Fiber.Ivar.read task.ivar

  let cancel t =
    let* () = Fiber.return () in
    Mutex.lock t.select.mutex;
    let+ () =
      Fiber.Ivar.peek t.ivar >>= function
      | Some _ -> Fiber.return ()
      | None ->
        let module Scheduler = (val t.select.scheduler) in
        let table =
          match t.what with
          | `Read -> t.select.readers
          | `Write -> t.select.writers
        in
        let should_interrupt =
          match Table.find table t.fd with
          | None -> false
          | Some q ->
            let new_q = Queue.create () in
            Queue.iter q ~f:(fun (Task t' as task) ->
                if Task_id.equal t.id t'.id then Scheduler.cancel_job_started ()
                else Queue.push new_q task);
            if Queue.is_empty new_q then (
              Table.remove table t.fd;
              Queue.is_empty q)
            else (
              Table.add_exn table t.fd new_q;
              Queue.length new_q <> Queue.length q)
        in
        if should_interrupt then interrupt t.select;
        Fiber.Ivar.fill t.ivar (Error `Cancelled)
    in
    Mutex.unlock t.select.mutex
end

let drain_until_ready queue acc =
  match Queue.pop queue with
  | None -> acc
  | Some (Task task) ->
    let result = try Ok (task.job ()) with exn -> Error (`Exn exn) in
    Fiber.Fill (task.ivar, result) :: acc

let make_fills fds pipe_fd waiters init =
  List.fold_left fds ~init:(false, init) ~f:(fun (pipe, acc) fd ->
      if Fd.equal fd pipe_fd then (true, acc)
      else
        let acc =
          match Table.find waiters fd with
          | None -> acc
          | Some w ->
            let acc = drain_until_ready w acc in
            if Queue.is_empty w then Table.remove waiters fd;
            acc
        in
        (pipe, acc))

let drain_pipe pipe buf =
  match Unix.read pipe buf 0 (Bytes.length buf) with
  | _ -> ()
  | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> ()

let rec drain_cancel q acc =
  match Queue.pop q with
  | None -> acc
  | Some (Task task) ->
    drain_cancel q (Fiber.Fill (task.ivar, Error `Cancelled) :: acc)

let maybe_cancel table fd acc =
  match Table.find table fd with
  | None -> acc
  | Some q ->
    Table.remove table fd;
    drain_cancel q acc

let rec select_loop t =
  (match t.to_close with
  | [] -> ()
  | to_close -> (
    let fills =
      List.fold_left to_close ~init:[] ~f:(fun acc fd ->
          Unix.close fd;
          let acc = maybe_cancel t.readers fd acc in
          maybe_cancel t.writers fd acc)
    in
    t.to_close <- [];
    match fills with
    | [] -> ()
    | _ :: _ ->
      let module Scheduler = (val t.scheduler) in
      Scheduler.fill_jobs fills));
  match t.running with
  | false ->
    Unix.close t.pipe_write;
    Unix.close t.pipe_read
  | true ->
    let readers, writers, ex =
      let read = t.pipe_read :: Table.keys t.readers in
      let write = Table.keys t.writers in
      Mutex.unlock t.mutex;
      (* At this point, if any [ready] acquires the lock, they need to check if
         [read] or [write] contain their fd. If it doesn't, the write
         [t.pipe_write] will interrupt this select *)
      Unix.select read write [] (-1.0)
    in
    assert (ex = []);
    (* Before we acquire the lock, it's possible that new tasks were added.
       This is fine. *)
    Mutex.lock t.mutex;
    let seen_pipe, fills = make_fills readers t.pipe_read t.readers [] in
    (* we will never see [t.pipe_read] in the next list, but there's no harm in
       this *)
    let _, fills = make_fills writers t.pipe_read t.writers fills in
    if seen_pipe then (
      drain_pipe t.pipe_read t.pipe_buf;
      t.interrupting <- false);
    (match fills with
    | [] -> ()
    | _ :: _ ->
      let module Scheduler = (val t.scheduler) in
      Scheduler.fill_jobs fills);
    select_loop t

let t_var = Fiber.Var.create ()

let with_io scheduler f =
  let module Scheduler = (val scheduler : Scheduler) in
  let t =
    let pipe_read, pipe_write = Unix.pipe ~cloexec:true () in
    if not Sys.win32 then (
      Unix.set_nonblock pipe_read;
      Unix.set_nonblock pipe_write);
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
    }
  in
  let () =
    Scheduler.spawn_thread (fun () ->
        Mutex.lock t.mutex;
        Exn.protect
          ~f:(fun () -> select_loop t)
          ~finally:(fun () -> Mutex.unlock t.mutex))
  in
  Fiber.Var.set t_var t (fun () ->
      Fiber.finalize f ~finally:(fun () ->
          Mutex.lock t.mutex;
          t.running <- false;
          interrupt t;
          Mutex.unlock t.mutex;
          Fiber.return ()))

let with_ f =
  let+ t = Fiber.Var.get_exn t_var in
  Mutex.lock t.mutex;
  Exn.protect ~f:(fun () -> f t) ~finally:(fun () -> Mutex.unlock t.mutex)

let cancel_fd scheduler table fd =
  match Table.find table fd with
  | None -> Fiber.return ()
  | Some tasks ->
    Table.remove table fd;
    let module Scheduler = (val scheduler : Scheduler) in
    Queue.to_list tasks
    |> Fiber.parallel_iter ~f:(fun (Task t) ->
           Scheduler.cancel_job_started ();
           Fiber.Ivar.fill t.ivar (Error `Cancelled))

let close fd =
  let* t = Fiber.Var.get_exn t_var in
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

let ready fd what ~f:job =
  with_ @@ fun t ->
  let module Scheduler = (val t.scheduler) in
  Scheduler.register_job_started ();
  let ivar = Fiber.Ivar.create () in
  let q, interrupt_needed =
    let table =
      match what with
      | `Read -> t.readers
      | `Write -> t.writers
    in
    match Table.find table fd with
    | Some q -> (q, false)
    | None ->
      let q = Queue.create () in
      Table.add_exn table fd q;
      (q, true)
  in
  let task = { ivar; select = t; job; what; id = Task_id.gen (); fd } in
  Queue.push q (Task task);
  if interrupt_needed then interrupt t;
  task

let rec with_retry f fd =
  match f () with
  | () -> Fiber.return (Ok ())
  | exception Unix.Unix_error (EWOULDBLOCK, x, y) when Sys.win32 ->
    Fiber.return (Error (`Unix (Unix.EINPROGRESS, x, y)))
  | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) -> (
    let* task = ready fd `Write ~f:Fun.id in
    Task.await task >>= function
    | Ok () -> with_retry f fd
    | Error `Cancelled as e -> Fiber.return e
    | Error (`Exn _) -> assert false)
  | exception Unix.Unix_error (err, x, y) ->
    Fiber.return (Error (`Unix (err, x, y)))

let connect f fd socket =
  let* () = Fiber.return () in
  with_retry (fun () -> f fd socket) fd >>= function
  | Ok () -> Fiber.return (Ok ())
  | Error (`Unix (Unix.EISCONN, _, _)) when Sys.win32 -> Fiber.return (Ok ())
  | Error (`Unix (EINPROGRESS, _, _)) -> (
    let* task = ready fd `Write ~f:(fun () -> Unix.getsockopt_error fd) in
    Task.await task >>| function
    | Error _ as e -> e
    | Ok None -> Ok ()
    | Ok (Some err) -> Error (`Exn (Unix.Unix_error (err, "connect", ""))))
  | Error (`Unix (e, x, y)) ->
    Fiber.return @@ Error (`Exn (Unix.Unix_error (e, x, y)))
  | Error (`Exn _) as e -> Fiber.return e
  | Error `Cancelled as e -> Fiber.return e
