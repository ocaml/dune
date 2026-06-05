open! Import

(* These mirror the parallel combinators in [Fiber], but while a Memo computation is running
   they record the dependencies discovered by independent parallel branches as a parallel
   section (via [Deps_collector.run_parallel]) rather than sequentially, so that
   [restore_from_cache] can later check those dependencies concurrently. When no computation
   is running (no active collector) they behave exactly like the [Fiber] versions.

   Combinators that are not overridden here (e.g. [both], [parallel_iter_seq]) simply collect
   their dependencies sequentially, which is always correct -- overriding only adds the
   parallel-restore optimisation. *)

let fork_and_join x y =
  Deps_collector.run_parallel ~num_threads:2 (function
    | None -> Fiber.fork_and_join x y
    | Some { Deps_collector.run } ->
      Fiber.fork_and_join (fun () -> run ~f:x) (fun () -> run ~f:y))
;;

let fork_and_join_unit x y =
  Deps_collector.run_parallel ~num_threads:2 (function
    | None -> Fiber.fork_and_join_unit x y
    | Some { Deps_collector.run } ->
      Fiber.fork_and_join_unit (fun () -> run ~f:x) (fun () -> run ~f:y))
;;

let all_concurrently l =
  Deps_collector.run_parallel ~num_threads:(List.length l) (function
    | None -> Fiber.all_concurrently l
    | Some { Deps_collector.run } ->
      Fiber.parallel_map l ~f:(fun x -> run ~f:(fun () -> x)))
;;

let parallel_map l ~f =
  Deps_collector.run_parallel ~num_threads:(List.length l) (function
    | None -> Fiber.parallel_map l ~f
    | Some { Deps_collector.run } ->
      Fiber.parallel_map l ~f:(fun value -> run ~f:(fun () -> f value)))
;;

let parallel_iter l ~f =
  Deps_collector.run_parallel ~num_threads:(List.length l) (function
    | None -> Fiber.parallel_iter l ~f
    | Some { Deps_collector.run } ->
      Fiber.parallel_iter l ~f:(fun value -> run ~f:(fun () -> f value)))
;;

let map_reduce l ~f ~empty ~combine =
  Deps_collector.run_parallel ~num_threads:(List.length l) (function
    | None -> Fiber.map_reduce l ~f ~empty ~combine
    | Some { Deps_collector.run } ->
      Fiber.map_reduce l ~f:(fun x -> run ~f:(fun () -> f x)) ~empty ~combine)
;;

let map_reduce_array arr ~f ~empty ~combine =
  Deps_collector.run_parallel ~num_threads:(Array.length arr) (function
    | None -> Fiber.map_reduce_array arr ~f ~empty ~combine
    | Some { Deps_collector.run } ->
      Fiber.map_reduce_array arr ~f:(fun x -> run ~f:(fun () -> f x)) ~empty ~combine)
;;
