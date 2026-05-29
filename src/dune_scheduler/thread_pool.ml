open Stdune

(** The thread pool implementation allows callers to offload (non raising) work
    to background threads.

    The pool maintains a [min_workers, max_workers] number of threads active and
    dispatches the work to them.

    There's no attempt to dispatch the work fairly.

    There's no ways to for callers to pick an underlying thread to use for some
    specific task. *)

type t =
  { mutex : Mutex.t
  ; cv : Condition.t
  ; tasks : (unit -> unit) Queue.t
  ; min_workers : int
  ; max_workers : int
  ; (* number of threads waiting for a task *)
    mutable idle : int
  ; (* total number of running threads *)
    mutable running : int
  ; (* dead threads are collected here to [Thread.join] them. This is to
       cleanup the resources of dead threads. *)
    mutable dead : Thread.t list
  }

let worker_finished_and_unlock t =
  t.running <- t.running - 1;
  t.dead <- Thread.self () :: t.dead;
  Mutex.unlock t.mutex
;;

let run_task t task =
  Mutex.unlock t.mutex;
  match task () with
  | () -> Mutex.lock t.mutex
  | exception exn ->
    Mutex.lock t.mutex;
    worker_finished_and_unlock t;
    Code_error.raise "thread pool tasks must not raise" [ "exn", Exn.to_dyn exn ]
;;

let spawn_worker t =
  let rec loop () =
    match Queue.pop t.tasks with
    | Some task ->
      run_task t task;
      loop ()
    | None ->
      if t.running > t.min_workers then worker_finished_and_unlock t else wait_for_task ()
  and wait_for_task () =
    t.idle <- t.idle + 1;
    while Queue.is_empty t.tasks do
      (* TODO [pthread_cond_timedwait] to set a maximum time for idling *)
      Condition.wait t.cv t.mutex
    done;
    t.idle <- t.idle - 1;
    loop ()
  in
  t.running <- t.running + 1;
  match
    Thread0.spawn ~name:"thread-pool" (fun () ->
      Mutex.lock t.mutex;
      loop ())
  with
  | (_ : Thread.t) -> ()
  | exception exn ->
    t.running <- t.running - 1;
    raise exn
;;

let needs_worker t = Queue.length t.tasks > t.idle && t.running < t.max_workers

let create ~min_workers ~max_workers =
  if min_workers < 0 || max_workers <= 0 || min_workers > max_workers
  then
    Code_error.raise
      "Thread_pool.create got invalid worker bounds"
      [ "min_workers", Dyn.int min_workers; "max_workers", Dyn.int max_workers ];
  let t =
    { min_workers
    ; max_workers
    ; cv = Condition.create ()
    ; mutex = Mutex.create ()
    ; tasks = Queue.create ()
    ; idle = 0
    ; running = 0
    ; dead = []
    }
  in
  Mutex.protect t.mutex (fun () ->
    for _ = 0 to min_workers - 1 do
      spawn_worker t
    done);
  t
;;

let task t ~f =
  Mutex.protect t.mutex (fun () ->
    let dead =
      match t.dead with
      | [] -> []
      | dead ->
        t.dead <- [];
        dead
    in
    Queue.push t.tasks f;
    if needs_worker t then spawn_worker t;
    Condition.signal t.cv;
    dead)
  |> List.iter ~f:Thread.join
;;

let%expect_test "failed task updates worker accounting" =
  let t = create ~min_workers:0 ~max_workers:1 in
  t.running <- 1;
  Mutex.lock t.mutex;
  (match run_task t (fun () -> raise Exit) with
   | () -> Code_error.raise "run_task unexpectedly returned" []
   | exception Code_error.E _ -> ());
  Printf.printf "running: %d\n" t.running;
  Printf.printf "dead: %d\n" (List.length t.dead);
  Printf.printf "mutex unlocked: %b\n" (Mutex.try_lock t.mutex);
  Mutex.unlock t.mutex;
  [%expect
    {|
    running: 0
    dead: 1
    mutex unlocked: true |}]
;;

let%expect_test "queued work beyond idle capacity needs a worker" =
  let t = create ~min_workers:0 ~max_workers:2 in
  t.idle <- 1;
  t.running <- 1;
  Queue.push t.tasks (fun () -> ());
  Printf.printf "one task: %b\n" (needs_worker t);
  Queue.push t.tasks (fun () -> ());
  Printf.printf "two tasks: %b\n" (needs_worker t);
  [%expect
    {|
    one task: false
    two tasks: true |}]
;;
