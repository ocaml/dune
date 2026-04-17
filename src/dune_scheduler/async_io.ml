open Stdune
open Fiber.O

let byte = Bytes.make 1 '0'

module Task_id = Id.Make ()
open Types
include Types.Async_io

let interrupt t =
  if not t.interrupting
  then (
    assert (Unix.single_write (Fd.unsafe_to_unix_file_descr t.pipe_write) byte 0 1 = 1);
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
        t.status <- `Filled;
        Event.Queue.cancel_work_task_started t.select.scheduler_queue;
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
      try Ok (task.job label (Fd.unsafe_to_unix_file_descr fd)) with
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
  match Unix.read (Fd.unsafe_to_unix_file_descr pipe) buf 0 (Bytes.length buf) with
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
         Fd.close fd;
         let acc = maybe_cancel t.readers fd acc in
         maybe_cancel t.writers fd acc)
     in
     t.to_close <- [];
     Event.Queue.send_worker_tasks_completed t.scheduler_queue fills);
  match t.running with
  | false ->
    Fd.close t.pipe_write;
    if not Sys.win32
    then
      Fd.close t.pipe_read (* On Win32, both ends of the "pipe" are the same UDP socket *)
  | true ->
    let read =
      Fd.unsafe_to_unix_file_descr t.pipe_read
      :: List.map (Table.keys t.readers) ~f:Fd.unsafe_to_unix_file_descr
    in
    let write = List.map (Table.keys t.writers) ~f:Fd.unsafe_to_unix_file_descr in
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
       let readers = List.map readers ~f:Fd.unsafe_of_unix_file_descr in
       let writers = List.map writers ~f:Fd.unsafe_of_unix_file_descr in
       let seen_pipe, fills = make_fills readers t.pipe_read t.readers [] in
       (* we will never see [t.pipe_read] in the next list, but there's no harm in
          this *)
       let _, fills = make_fills writers t.pipe_read t.writers fills in
       if seen_pipe
       then (
         drain_pipe t.pipe_read t.pipe_buf;
         t.interrupting <- false);
       Event.Queue.send_worker_tasks_completed t.scheduler_queue fills;
       select_loop t)
;;

let start t =
  Thread0.spawn (fun () ->
    Mutex.lock t.mutex;
    match select_loop t with
    | () -> Mutex.unlock t.mutex
    | exception Unlocked exn -> Exn_with_backtrace.reraise exn
    | exception exn ->
      Mutex.unlock t.mutex;
      reraise exn)
;;

let get_exn () =
  let+ t = Types.Scheduler.t () in
  let t = t.async_io in
  if not t.started
  then (
    let (_ : Thread.t) = start ~name:"async-io" t in
    t.started <- true);
  t
;;

let create scheduler_queue =
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
  let pipe_read = Fd.unsafe_of_unix_file_descr pipe_read in
  let pipe_write = Fd.unsafe_of_unix_file_descr pipe_write in
  { readers = Table.create (module Fd) 64
  ; writers = Table.create (module Fd) 64
  ; mutex = Mutex.create ()
  ; scheduler_queue
  ; running = true
  ; pipe_read
  ; pipe_write
  ; pipe_buf = Bytes.create 512
  ; interrupting = false
  ; to_close = []
  ; started = false
  }
;;

let with_ f =
  let+ t = get_exn () in
  Mutex.lock t.mutex;
  Exn.protect ~f:(fun () -> f t) ~finally:(fun () -> Mutex.unlock t.mutex)
;;

let cancel_fd scheduler_queue table fd =
  match Table.find table fd with
  | None -> Fiber.return ()
  | Some tasks ->
    Table.remove table fd;
    Queue.to_list tasks
    |> Fiber.parallel_iter ~f:(fun (Task (t, _)) ->
      Event.Queue.cancel_work_task_started scheduler_queue;
      Fiber.Ivar.fill t.ivar (Error `Cancelled))
;;

let close fd =
  let fd = Fd.unsafe_of_unix_file_descr fd in
  let* t = get_exn () in
  Mutex.lock t.mutex;
  (* everything below is guaranteed not to raise so the mutex will be unlocked
     in the end. There's no need to use [protect] to make sure we don't deadlock *)
  t.to_close <- fd :: t.to_close;
  let+ () =
    Fiber.fork_and_join_unit
      (fun () -> cancel_fd t.scheduler_queue t.readers fd)
      (fun () -> cancel_fd t.scheduler_queue t.writers fd)
  in
  interrupt t;
  Mutex.unlock t.mutex
;;

let ready_one (type a label) (fds : (label * Unix.file_descr) list) what ~f:job
  : a Task.t Fiber.t
  =
  with_
  @@ fun t ->
  let fds = List.map fds ~f:(fun (label, fd) -> label, Fd.unsafe_of_unix_file_descr fd) in
  Event.Queue.register_worker_task_started t.scheduler_queue;
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
