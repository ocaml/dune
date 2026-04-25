open Stdune
open Fiber.O
open Types
include Types.Async_io

let filter_queue q ~f =
  let new_q = Queue.create () in
  Queue.iter q ~f:(fun x -> if f x then Queue.push new_q x);
  new_q
;;

let with_mutex t ~f =
  Mutex.lock t.mutex;
  Exn.protect ~f ~finally:(fun () -> Mutex.unlock t.mutex)
;;

let wakeup_loop t = Lev.Async.send t.wakeup t.loop

let retire_watcher t fd ({ io; _ } as watcher : watcher) =
  if Lev.Io.is_active io then Lev.Io.stop io t.loop;
  Table.remove t.watchers fd;
  t.retired <- watcher :: t.retired
;;

let destroy_retired_watchers_locked t =
  let pending, ready =
    List.fold_left
      t.retired
      ~init:([], [])
      ~f:(fun (pending, ready) ({ io; _ } as watcher) ->
        if Lev.Io.is_pending io
        then watcher :: pending, ready
        else pending, watcher :: ready)
  in
  t.retired <- pending;
  List.iter ready ~f:(fun { io; _ } -> Lev.Io.destroy io)
;;

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

let rec drain_cancel waiters q acc =
  match Queue.pop q with
  | None -> acc
  | Some (Task (task, _)) ->
    task.status <- `Filled;
    cleanup_other_tasks waiters task;
    drain_cancel waiters q (Fiber.Fill (task.ivar, Error `Cancelled) :: acc)
;;

let maybe_cancel table fd acc =
  match Table.find table fd with
  | None -> acc
  | Some q ->
    Table.remove table fd;
    drain_cancel table q acc
;;

let add_desired desired fd event =
  let events =
    match Table.find desired fd with
    | None -> Lev.Io.Event.Set.create ()
    | Some events -> events
  in
  Table.set desired fd (Lev.Io.Event.Set.add events event)
;;

let mark_ready watcher ready =
  if Lev.Io.Event.Set.mem ready Read then watcher.read_ready <- true;
  if Lev.Io.Event.Set.mem ready Write then watcher.write_ready <- true
;;

let service_waiters waiters fd fills =
  match Table.find waiters fd with
  | None -> fills
  | Some q ->
    let fills = drain_until_ready fd waiters q fills in
    if Queue.is_empty q then Table.remove waiters fd;
    fills
;;

let service_ready_watchers_locked t fills =
  List.fold_left (Table.to_list t.watchers) ~init:fills ~f:(fun fills (fd, watcher) ->
    let fills =
      if watcher.read_ready then service_waiters t.readers fd fills else fills
    in
    let fills =
      if watcher.write_ready then service_waiters t.writers fd fills else fills
    in
    watcher.read_ready <- false;
    watcher.write_ready <- false;
    fills)
;;

let sync_watchers_locked t =
  let desired = Table.create (module Fd) 16 in
  List.iter (Table.keys t.readers) ~f:(fun fd -> add_desired desired fd Read);
  List.iter (Table.keys t.writers) ~f:(fun fd -> add_desired desired fd Write);
  List.iter (Table.keys t.watchers) ~f:(fun fd ->
    match Table.find t.watchers fd, Table.find desired fd with
    | None, _ -> ()
    | Some watcher, None -> retire_watcher t fd watcher
    | Some watcher, Some events ->
      if not (Lev.Io.Event.Set.equal watcher.events events)
      then (
        if Lev.Io.is_active watcher.io then Lev.Io.stop watcher.io t.loop;
        Lev.Io.modify watcher.io events;
        Lev.Io.start watcher.io t.loop;
        watcher.events <- events));
  List.iter (Table.keys desired) ~f:(fun fd ->
    match Table.find t.watchers fd with
    | Some _ -> ()
    | None ->
      let events =
        match Table.find desired fd with
        | Some events -> events
        | None -> assert false
      in
      let watcher_ref = ref None in
      let io =
        Lev.Io.create
          (fun _ _ ready ->
             Option.iter !watcher_ref ~f:(fun watcher -> mark_ready watcher ready))
          (Fd.unsafe_to_unix_file_descr fd)
          events
      in
      let watcher = { io; events; read_ready = false; write_ready = false } in
      watcher_ref := Some watcher;
      Lev.Io.start io t.loop;
      Table.add_exn t.watchers fd watcher)
;;

let process_closures_locked t fills =
  match Table.to_list t.to_close with
  | [] -> fills
  | to_close ->
    Table.clear t.to_close;
    List.fold_left to_close ~init:fills ~f:(fun fills (fd, request) ->
      (match Table.find t.watchers fd with
       | None -> Fd.close fd
       | Some watcher ->
         retire_watcher t fd watcher;
         Fd.close fd);
      let fills =
        let fills = maybe_cancel t.readers fd fills in
        maybe_cancel t.writers fd fills
      in
      Fiber.Fill (request, ()) :: fills)
;;

module Task = struct
  type 'a t = User_task : ('a, 'label) task -> 'a t

  let await (User_task task) = Fiber.Ivar.read task.ivar

  let cancel (User_task t) =
    let* () = Fiber.return () in
    with_mutex t.select ~f:(fun () ->
      match t.status with
      | `Filled -> Fiber.return ()
      | `Waiting ->
        let table =
          match t.what with
          | `Read -> t.select.readers
          | `Write -> t.select.writers
        in
        List.iter t.fds ~f:(fun fd ->
          match Table.find table fd with
          | None -> ()
          | Some q ->
            let new_q =
              filter_queue q ~f:(fun (Task (t', _)) -> not (Task_id.equal t.id t'.id))
            in
            if Queue.is_empty new_q
            then Table.remove table fd
            else Table.set table fd new_q);
        wakeup_loop t.select;
        t.status <- `Filled;
        Event.Queue.cancel_work_task_started t.select.scheduler_queue;
        Fiber.Ivar.fill t.ivar (Error `Cancelled))
  ;;
end

let destroy_loop_resources t =
  with_mutex t ~f:(fun () ->
    List.iter (Table.to_list t.watchers) ~f:(fun (fd, watcher) ->
      retire_watcher t fd watcher);
    List.iter t.retired ~f:(fun { io; _ } -> Lev.Io.destroy io);
    t.retired <- [];
    if Lev.Async.is_active t.wakeup then Lev.Async.stop t.wakeup t.loop;
    Lev.Async.destroy t.wakeup)
;;

let shutdown t =
  if not t.destroyed
  then (
    if t.started
    then (
      with_mutex t ~f:(fun () -> t.shutting_down <- true);
      wakeup_loop t;
      Option.iter t.thread ~f:Thread.join;
      t.thread <- None;
      t.started <- false)
    else (
      destroy_loop_resources t;
      Lev.Loop.destroy t.loop);
    t.destroyed <- true)
;;

let start ~name t =
  Thread0.spawn ~name (fun () ->
    let rec loop () =
      ignore
        (Lev.Loop.run t.loop Lev.Loop.Once : [ `No_more_active_watchers | `Otherwise ]);
      let shutting_down, fills =
        with_mutex t ~f:(fun () ->
          let fills = process_closures_locked t [] in
          let fills = service_ready_watchers_locked t fills in
          sync_watchers_locked t;
          destroy_retired_watchers_locked t;
          t.shutting_down, fills)
      in
      Event.Queue.send_worker_tasks_completed t.scheduler_queue fills;
      if not shutting_down then loop ()
    in
    Exn.protect ~f:loop ~finally:(fun () ->
      destroy_loop_resources t;
      Lev.Loop.destroy t.loop;
      t.destroyed <- true))
;;

let get_exn () =
  let t = Types.Scheduler.t () in
  let t = t.async_io in
  if not t.started
  then (
    t.thread <- Some (start ~name:"async-io" t);
    t.started <- true);
  t
;;

let create scheduler_queue =
  let loop = Lev.Loop.create () in
  let watchers = Table.create (module Fd) 16 in
  let wakeup = Lev.Async.create (fun _ -> ()) in
  let t =
    { readers = Table.create (module Fd) 64
    ; writers = Table.create (module Fd) 64
    ; to_close = Table.create (module Fd) 16
    ; mutex = Mutex.create ()
    ; scheduler_queue
    ; loop
    ; wakeup
    ; watchers
    ; retired = []
    ; thread = None
    ; started = false
    ; shutting_down = false
    ; destroyed = false
    }
  in
  Lev.Async.start wakeup loop;
  t
;;

let with_ f =
  let t = get_exn () in
  with_mutex t ~f:(fun () -> f t)
;;

let close fd =
  let t = get_exn () in
  let to_cancel =
    with_mutex t ~f:(fun () ->
      match Table.find t.to_close fd with
      | Some ivar -> `Wait ivar
      | None ->
        Event.Queue.register_worker_task_started t.scheduler_queue;
        let ivar = Fiber.Ivar.create () in
        Table.add_exn t.to_close fd ivar;
        let to_cancel = maybe_cancel t.readers fd [] in
        let fills = maybe_cancel t.readers fd to_cancel in
        let to_cancel = maybe_cancel t.writers fd [] in
        `Cancel (ivar, fills @ maybe_cancel t.writers fd to_cancel))
  in
  wakeup_loop t;
  match to_cancel with
  | `Wait ivar -> Fiber.Ivar.read ivar
  | `Cancel (ivar, to_cancel) ->
    let* () =
      Fiber.parallel_iter to_cancel ~f:(fun (Fiber.Fill (ivar, v)) ->
        Event.Queue.cancel_work_task_started t.scheduler_queue;
        Fiber.Ivar.fill ivar v)
    in
    Fiber.Ivar.read ivar
;;

let ready_one (type a label) (fds : (label * Fd.t) list) what ~f:job : a Task.t =
  with_
  @@ fun t ->
  Event.Queue.register_worker_task_started t.scheduler_queue;
  let ivar = Fiber.Ivar.create () in
  let table =
    match what with
    | `Read -> t.readers
    | `Write -> t.writers
  in
  let queues =
    List.map fds ~f:(fun (label, fd) ->
      let q =
        match Table.find table fd with
        | Some q -> q
        | None ->
          let q = Queue.create () in
          Table.add_exn table fd q;
          q
      in
      label, q)
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
  wakeup_loop t;
  Task.User_task task
;;

let ready fd what ~f:job = ready_one [ (), fd ] what ~f:(fun () _fd -> job ())

let rec with_retry_fd f fd =
  match f () with
  | () -> Fiber.return (Ok ())
  | exception Unix.Unix_error (EWOULDBLOCK, x, y) when Sys.win32 ->
    Fiber.return (Error (`Unix (Unix.EINPROGRESS, x, y)))
  | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
    let task = ready fd `Write ~f:Fun.id in
    Task.await task
    >>= (function
     | Ok () -> with_retry_fd f fd
     | Error `Cancelled as e -> Fiber.return e
     | Error (`Exn _) -> assert false)
  | exception Unix.Unix_error (err, x, y) -> Fiber.return (Error (`Unix (err, x, y)))
;;

let connect f fd socket =
  let* () = Fiber.return () in
  with_retry_fd (fun () -> f fd socket) fd
  >>= function
  | Ok () -> Fiber.return (Ok ())
  | Error (`Unix (Unix.EISCONN, _, _)) when Sys.win32 -> Fiber.return (Ok ())
  | Error (`Unix (EINPROGRESS, _, _)) ->
    let task =
      ready fd `Write ~f:(fun () ->
        Unix.getsockopt_error (Fd.unsafe_to_unix_file_descr fd))
    in
    Task.await task
    >>| (function
     | Error _ as e -> e
     | Ok None -> Ok ()
     | Ok (Some err) -> Error (`Exn (Unix.Unix_error (err, "connect", ""))))
  | Error (`Unix (e, x, y)) -> Fiber.return @@ Error (`Exn (Unix.Unix_error (e, x, y)))
  | Error (`Exn _) as e -> Fiber.return e
  | Error `Cancelled as e -> Fiber.return e
;;
