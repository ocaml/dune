open Dune_thread_pool

let spawn_thread f = ignore (Thread.create f ())

let%bench "almost no-op" =
  let tp = Thread_pool.create ~min_workers:10 ~max_workers:50 ~spawn_thread in
  let tasks = 50_000 in
  let counter = Atomic.make tasks in
  let f () = Atomic.decr counter in
  for _ = 0 to tasks - 1 do
    Thread_pool.task tp ~f
  done;
  while Atomic.get counter > 0 do
    Thread.yield ()
  done
;;

let%bench "syscall" =
  let tp = Thread_pool.create ~min_workers:10 ~max_workers:50 ~spawn_thread in
  let tasks = 50_000 in
  let counter = Atomic.make tasks in
  let f () =
    Unix.sleepf 0.0;
    Atomic.decr counter
  in
  for _ = 0 to tasks - 1 do
    Thread_pool.task tp ~f
  done;
  while Atomic.get counter > 0 do
    Thread.yield ()
  done
;;

let%bench "syscall - no background" =
  let tasks = 50_000 in
  let counter = Atomic.make tasks in
  let f () =
    Unix.sleepf 0.0;
    Atomic.decr counter
  in
  for _ = 0 to tasks - 1 do
    f ()
  done;
  while Atomic.get counter > 0 do
    Thread.yield ()
  done
;;
