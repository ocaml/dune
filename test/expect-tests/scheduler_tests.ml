open Stdune
include Dune_scheduler
open Dune_tests_common
open Fiber.O

let () = init ()

let default =
  Clflags.display := Short;
  { Scheduler.Config.concurrency = 1
  ; priority_scheduling = false
  ; print_ctrl_c_warning = false
  ; watch_exclusions = []
  }
;;

let go ?(timeout = Time.Span.of_secs 0.3) ?(config = default) f =
  try Scheduler.Run.go ~timeout config ~file_watcher:No_watcher f with
  | Shutdown.E Requested -> ()
;;

let priority_config = { default with priority_scheduling = true }

let%expect_test "Action builder demand roots are eager and fresh" =
  let module Action_builder = Dune_engine.Action_builder in
  let module Demand_class = Memo.Job_priority.Demand_class in
  let observation =
    Memo.of_non_reproducible_fiber (Memo.Job_priority.For_tests.current_root ())
  in
  let builder =
    Action_builder.with_job_demand
      Demand_class.Direct
      (Action_builder.of_memo observation)
  in
  let result = ref None in
  go ~config:priority_config (fun () ->
    let* lazy_root, _ = Action_builder.evaluate_and_collect_deps builder |> Memo.run in
    let* eager_root_1, _ =
      Action_builder.evaluate_and_collect_facts builder |> Memo.run
    in
    let* eager_root_2, _ =
      Action_builder.evaluate_and_collect_facts builder |> Memo.run
    in
    result := Some (lazy_root, eager_root_1, eager_root_2);
    Fiber.return ());
  let demand_class = function
    | Demand_class.Bulk -> "bulk"
    | Demand_class.Normal -> "normal"
    | Demand_class.Direct -> "direct"
  in
  let root = function
    | None -> "none"
    | Some (demand_class_, _) -> demand_class demand_class_
  in
  let lazy_root, eager_root_1, eager_root_2 = Option.value_exn !result in
  Printf.printf "lazy: %s\n" (root lazy_root);
  Printf.printf "eager 1: %s\n" (root eager_root_1);
  Printf.printf "eager 2: %s\n" (root eager_root_2);
  let distinct_ids =
    match eager_root_1, eager_root_2 with
    | Some (_, id_1), Some (_, id_2) -> id_1 <> id_2
    | None, _ | _, None -> false
  in
  Printf.printf "distinct IDs: %b\n" distinct_ids;
  [%expect
    {|
    lazy: none
    eager 1: direct
    eager 2: direct
    distinct IDs: true |}]
;;

let%expect_test "job demand scopes reject active Memo stacks" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  let rejected config =
    let nested =
      Memo.Lazy.create ~name:"nested-job-demand" (fun () ->
        Memo.with_job_demand Demand_class.Direct (fun () -> Memo.return ()))
    in
    let result = ref None in
    go ~config (fun () ->
      let+ outcome =
        Fiber.collect_errors (fun () -> Memo.Lazy.force nested |> Memo.run)
      in
      result := Some (Result.is_error outcome));
    Option.value_exn !result
  in
  Printf.printf "enabled rejected: %b\n" (rejected priority_config);
  Printf.printf "disabled rejected: %b\n" (rejected default);
  [%expect
    {|
    enabled rejected: true
    disabled rejected: false |}]
;;

let run_with_demand demand_class memo =
  Memo.with_job_demand demand_class (fun () -> memo) |> Memo.run
;;

let print_registry_stats label =
  let+ roots, memberships = Memo.Job_priority.For_tests.current_registry_stats () in
  Printf.printf "%s: %d roots, %d memberships\n" label roots memberships
;;

let%expect_test "Build_system.run supplies Normal demand only when enabled" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  Path.mkdir_p (Path.relative Path.root "_build");
  let observe config =
    let result = ref None in
    go ~config (fun () ->
      let+ observation =
        Dune_engine.Build_system.run (fun () ->
          Memo.of_non_reproducible_fiber (Memo.Job_priority.For_tests.current_root ()))
      in
      result := Some observation);
    match Option.value_exn !result with
    | Error `Already_reported -> Code_error.raise "Build_system.run failed" []
    | Ok observation -> observation
  in
  let to_string = function
    | None -> "none"
    | Some (Demand_class.Bulk, _) -> "bulk"
    | Some (Normal, _) -> "normal"
    | Some (Direct, _) -> "direct"
  in
  Printf.printf "enabled: %s\n" (observe priority_config |> to_string);
  Printf.printf "disabled: %s\n" (observe default |> to_string);
  [%expect
    {|
    enabled: normal
    disabled: none |}]
;;

let%expect_test "root finalization clears memberships and removal is idempotent" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  let leaf = Memo.Lazy.create ~name:"finalized-root" (fun () -> Memo.return ()) in
  let remove_leaf =
    Memo.Lazy.create ~name:"removed-root" (fun () ->
      Memo.of_non_reproducible_fiber
        (let* priority = Memo.Job_priority.current () in
         let* () = print_registry_stats "before repeated removal" in
         let* () = Memo.Job_priority.For_tests.remove_current_root () in
         let* () = Memo.Job_priority.For_tests.remove_current_root () in
         Printf.printf "had queue handle: %b\n" (Option.is_some priority);
         print_registry_stats "removed twice"))
  in
  go ~config:priority_config (fun () ->
    let finalized =
      Memo.with_job_demand Demand_class.Direct (fun () ->
        let open Memo.O in
        let* () = Memo.Lazy.force leaf in
        Memo.of_non_reproducible_fiber (print_registry_stats "active"))
    in
    let* () = Memo.run finalized in
    let* () = print_registry_stats "finalized" in
    let* () = run_with_demand Demand_class.Direct (Memo.Lazy.force remove_leaf) in
    print_registry_stats "second finalizer");
  let roots, memberships = Memo.Job_priority.For_tests.global_registry_stats () in
  Printf.printf "after factory: %d roots, %d memberships\n" roots memberships;
  [%expect
    {|
    active: 1 roots, 1 memberships
    finalized: 0 roots, 0 memberships
    before repeated removal: 1 roots, 1 memberships
    had queue handle: true
    removed twice: 0 roots, 0 memberships
    second finalizer: 0 roots, 0 memberships
    after factory: 0 roots, 0 memberships |}]
;;

let%expect_test "removing direct demand reveals bulk and demotes queued work" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  let order = ref [] in
  let shared_ready = Fiber.Ivar.create () in
  let normal_ready = Fiber.Ivar.create () in
  let direct_removed = Fiber.Ivar.create () in
  let shared =
    Memo.Lazy.create ~name:"demoted-shared" (fun () ->
      Memo.of_reproducible_fiber
        (let* () = Fiber.Ivar.fill shared_ready () in
         Scheduler.with_job_slot (fun () ->
           let+ roots = Memo.Job_priority.For_tests.current_node_roots () in
           let only_bulk =
             match roots with
             | [ (Demand_class.Bulk, _) ] -> true
             | [] | [ ((Normal | Direct), _) ] | _ :: _ :: _ -> false
           in
           Printf.printf "only bulk remains: %b\n" only_bulk;
           order := "shared" :: !order)))
  in
  let normal =
    Memo.Lazy.create ~name:"demotion-normal" (fun () ->
      Memo.of_reproducible_fiber
        (let* () = Fiber.Ivar.fill normal_ready () in
         Scheduler.with_job_slot (fun () ->
           order := "normal" :: !order;
           Fiber.return ())))
  in
  let direct =
    Memo.with_job_demand Demand_class.Direct (fun () ->
      Memo.of_non_reproducible_fiber
        (Fiber.fork_and_join_unit
           (fun () -> Memo.Lazy.force shared |> Memo.run)
           (fun () ->
              let* () = Fiber.Ivar.read shared_ready in
              let* () = Memo.Job_priority.For_tests.remove_current_root () in
              Fiber.Ivar.fill direct_removed ())))
  in
  let blocker_started = Fiber.Ivar.create () in
  let release_blocker = Fiber.Ivar.create () in
  go ~config:priority_config (fun () ->
    Fiber.fork_and_join_unit
      (fun () ->
         Scheduler.with_job_slot (fun () ->
           let* () = Fiber.Ivar.fill blocker_started () in
           Fiber.Ivar.read release_blocker))
      (fun () ->
         let* () = Fiber.Ivar.read blocker_started in
         Fiber.parallel_iter
           [ (fun () -> run_with_demand Demand_class.Bulk (Memo.Lazy.force shared))
           ; (fun () -> Memo.run direct)
           ; (fun () -> run_with_demand Demand_class.Normal (Memo.Lazy.force normal))
           ; (fun () ->
               let* () = Fiber.Ivar.read direct_removed in
               let* () = Fiber.Ivar.read normal_ready in
               Fiber.Ivar.fill release_blocker ())
           ]
           ~f:(fun f -> f ())));
  List.rev !order |> List.iter ~f:print_endline;
  [%expect
    {|
    only bulk remains: true
    normal
    shared |}]
;;

let%expect_test "build cancellation demotes queued demand before branch finalizers" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  let module Process = Dune_engine.Process in
  let cancellation = Fiber.Cancel.create () in
  let build =
    Process.Build.create
      ~action_runner:None
      ~run_id:Dune_engine.Run_id.Batch
      ~cancellation
  in
  let order = ref [] in
  let direct_ready = Fiber.Ivar.create () in
  let normal_ready = Fiber.Ivar.create () in
  let blocker_started = Fiber.Ivar.create () in
  let release_blocker = Fiber.Ivar.create () in
  let direct =
    Memo.Lazy.create ~name:"cancelled-direct" (fun () ->
      Memo.of_reproducible_fiber
        (Fiber.finalize
           (fun () ->
              let* () = Fiber.Ivar.fill direct_ready () in
              Scheduler.with_job_slot (fun () ->
                order := "direct" :: !order;
                Fiber.return ()))
           ~finally:(fun () -> print_registry_stats "branch finalizer")))
  in
  go ~config:priority_config (fun () ->
    Process.Build.with_ build (fun () ->
      let* normal_priority = Scheduler.create_job_priority ~priority:1 () in
      Fiber.parallel_iter
        [ (fun () ->
            Scheduler.with_job_slot (fun () ->
              let* () = Fiber.Ivar.fill blocker_started () in
              Fiber.Ivar.read release_blocker))
        ; (fun () ->
            let* () = Fiber.Ivar.read blocker_started in
            let+ (), canceled =
              Fiber.Cancel.with_handler
                cancellation
                (fun () -> run_with_demand Demand_class.Direct (Memo.Lazy.force direct))
                ~on_cancel:(fun () -> Fiber.return ())
            in
            Printf.printf
              "branch canceled: %b\n"
              (match canceled with
               | Fiber.Cancel.Cancelled () -> true
               | Not_cancelled -> false))
        ; (fun () ->
            let* () = Fiber.Ivar.read blocker_started in
            let* () = Fiber.Ivar.fill normal_ready () in
            Scheduler.with_job_slot ~priority:normal_priority (fun () ->
              order := "normal" :: !order;
              Fiber.return ()))
        ; (fun () ->
            let* () = Fiber.Ivar.read direct_ready in
            let* () = Fiber.Ivar.read normal_ready in
            let* () = Process.Build.cancel_current () in
            Fiber.Ivar.fill release_blocker ())
        ]
        ~f:(fun f -> f ())));
  List.rev !order |> List.iter ~f:print_endline;
  [%expect
    {|
    branch finalizer: 0 roots, 0 memberships
    branch canceled: true
    normal
    direct |}]
;;

let%expect_test "invalidation is terminal until the Memo generation changes" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  let order = ref [] in
  let seed =
    Memo.Lazy.create ~name:"invalidation-seed" (fun () ->
      Memo.of_reproducible_fiber (Scheduler.with_job_slot Fiber.return))
  in
  let undemanded_ready = Fiber.Ivar.create () in
  let undemanded =
    Memo.Lazy.create ~name:"same-run-after-invalidation" (fun () ->
      Memo.of_non_reproducible_fiber
        (let* root = Memo.Job_priority.For_tests.current_root () in
         let* () = print_registry_stats "inside later scope" in
         Printf.printf "later ambient root: %b\n" (Option.is_some root);
         let* () = Fiber.Ivar.fill undemanded_ready () in
         Scheduler.with_job_slot (fun () ->
           order := "undemanded" :: !order;
           Fiber.return ())))
  in
  let normal_ready = Fiber.Ivar.create () in
  go ~config:priority_config (fun () ->
    let* () = run_with_demand Demand_class.Direct (Memo.Lazy.force seed) in
    Memo.Job_priority.invalidate_current_registry ();
    let* normal_priority = Scheduler.create_job_priority ~priority:1 () in
    let blocker_started = Fiber.Ivar.create () in
    let release_blocker = Fiber.Ivar.create () in
    let* () =
      Fiber.fork_and_join_unit
        (fun () ->
           Scheduler.with_job_slot (fun () ->
             let* () = Fiber.Ivar.fill blocker_started () in
             Fiber.Ivar.read release_blocker))
        (fun () ->
           let* () = Fiber.Ivar.read blocker_started in
           Fiber.parallel_iter
             [ (fun () ->
                 run_with_demand Demand_class.Direct (Memo.Lazy.force undemanded))
             ; (fun () ->
                 let* () = Fiber.Ivar.fill normal_ready () in
                 Scheduler.with_job_slot ~priority:normal_priority (fun () ->
                   order := "normal" :: !order;
                   Fiber.return ()))
             ; (fun () ->
                 let* () = Fiber.Ivar.read undemanded_ready in
                 let* () = Fiber.Ivar.read normal_ready in
                 Fiber.Ivar.fill release_blocker ())
             ]
             ~f:(fun f -> f ()))
    in
    print_registry_stats "after later scope");
  List.rev !order |> List.iter ~f:print_endline;
  [%expect
    {|
    inside later scope: 0 roots, 0 memberships
    later ambient root: false
    after later scope: 0 roots, 0 memberships
    normal
    undemanded |}]
;;

let%expect_test "Memo.reset starts an empty registry for the same scheduler factory" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  let order = ref [] in
  let evaluation = ref 0 in
  let reused_ready = Fiber.Ivar.create () in
  let reused_node, reused =
    Memo.Lazy.Expert.create ~name:"reused-priority" ~cutoff:Unit.equal (fun () ->
      incr evaluation;
      Memo.of_reproducible_fiber
        (let* () =
           if !evaluation = 1 then Fiber.return () else Fiber.Ivar.fill reused_ready ()
         in
         Scheduler.with_job_slot (fun () ->
           if !evaluation > 1 then order := "reused" :: !order;
           Fiber.return ())))
  in
  let normal_ready = Fiber.Ivar.create () in
  let normal =
    Memo.Lazy.create ~name:"new-run-normal" (fun () ->
      Memo.of_reproducible_fiber
        (let* () = Fiber.Ivar.fill normal_ready () in
         Scheduler.with_job_slot (fun () ->
           order := "normal" :: !order;
           Fiber.return ())))
  in
  go ~config:priority_config (fun () ->
    let* () = run_with_demand Demand_class.Direct (Memo.Lazy.force reused) in
    Memo.reset (Memo.Node.invalidate ~reason:Memo.Invalidation.Reason.Test reused_node);
    let* () = print_registry_stats "new run before demand" in
    let blocker_started = Fiber.Ivar.create () in
    let release_blocker = Fiber.Ivar.create () in
    Fiber.fork_and_join_unit
      (fun () ->
         Scheduler.with_job_slot (fun () ->
           let* () = Fiber.Ivar.fill blocker_started () in
           Fiber.Ivar.read release_blocker))
      (fun () ->
         let* () = Fiber.Ivar.read blocker_started in
         Fiber.parallel_iter
           [ (fun () -> run_with_demand Demand_class.Bulk (Memo.Lazy.force reused))
           ; (fun () -> run_with_demand Demand_class.Normal (Memo.Lazy.force normal))
           ; (fun () ->
               let* () = Fiber.Ivar.read reused_ready in
               let* () = Fiber.Ivar.read normal_ready in
               Fiber.Ivar.fill release_blocker ())
           ]
           ~f:(fun f -> f ())));
  List.rev !order |> List.iter ~f:print_endline;
  [%expect
    {|
    new run before demand: 0 roots, 0 memberships
    normal
    reused |}]
;;

let%expect_test "the same root observes an active nested dependency once" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  let inner_started = Fiber.Ivar.create () in
  let inspect_roots = Fiber.Ivar.create () in
  let inner =
    Memo.Lazy.create ~name:"same-root-inner" (fun () ->
      Memo.of_reproducible_fiber
        (let* () = Fiber.Ivar.fill inner_started () in
         let* () = Fiber.Ivar.read inspect_roots in
         let+ roots = Memo.Job_priority.For_tests.current_node_roots () in
         let root_id =
           match roots with
           | [ (Demand_class.Bulk, root_id) ] -> Some root_id
           | [] | [ ((Normal | Direct), _) ] | _ :: _ :: _ -> None
         in
         Printf.printf "one bulk root ID: %b\n" (Option.is_some root_id)))
  in
  let outer =
    Memo.Lazy.create ~name:"same-root-outer" (fun () -> Memo.Lazy.force inner)
  in
  let force_twice =
    Memo.with_job_demand Demand_class.Bulk (fun () ->
      Memo.parallel_map [ outer; outer ] ~f:Memo.Lazy.force)
  in
  go ~config:priority_config (fun () ->
    Fiber.fork_and_join_unit
      (fun () -> Memo.run force_twice >>| ignore)
      (fun () ->
         let* () = Fiber.Ivar.read inner_started in
         Fiber.Ivar.fill inspect_roots ()));
  [%expect {| one bulk root ID: true |}]
;;

let%expect_test "equal demand leaves a dependency chain behind FIFO work" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  let order = ref [] in
  let flat_ready = Fiber.Ivar.create () in
  let chain_ready = Fiber.Ivar.create () in
  let job name ready =
    Memo.Lazy.create ~name:("same-root-" ^ name) (fun () ->
      Memo.of_reproducible_fiber
        (let* () = Fiber.Ivar.fill ready () in
         Scheduler.with_job_slot (fun () ->
           order := name :: !order;
           Fiber.return ())))
  in
  let flat = job "flat" flat_ready in
  let chain =
    List.init 3 ~f:Fun.id
    |> List.fold_left ~init:(job "chain-0" chain_ready) ~f:(fun dependency n ->
      let name = sprintf "chain-%d" (n + 1) in
      Memo.Lazy.create ~name:("same-root-" ^ name) (fun () ->
        let open Memo.O in
        let* () = Memo.Lazy.force dependency in
        Memo.of_reproducible_fiber
          (Scheduler.with_job_slot (fun () ->
             order := name :: !order;
             Fiber.return ()))))
  in
  let blocker_started = Fiber.Ivar.create () in
  let release_blocker = Fiber.Ivar.create () in
  go ~config:priority_config (fun () ->
    Fiber.fork_and_join_unit
      (fun () ->
         Scheduler.with_job_slot (fun () ->
           let* () = Fiber.Ivar.fill blocker_started () in
           Fiber.Ivar.read release_blocker))
      (fun () ->
         let* () = Fiber.Ivar.read blocker_started in
         Fiber.fork_and_join_unit
           (fun () ->
              run_with_demand
                Demand_class.Bulk
                (Memo.parallel_map [ flat; chain ] ~f:Memo.Lazy.force
                 |> Memo.map ~f:ignore))
           (fun () ->
              let* () = Fiber.Ivar.read flat_ready in
              let* () = Fiber.Ivar.read chain_ready in
              Fiber.Ivar.fill release_blocker ())));
  List.rev !order |> List.iter ~f:print_endline;
  [%expect
    {|
    flat
    chain-0
    chain-1
    chain-2
    chain-3 |}]
;;

let%expect_test "distinct same-class roots reach an active nested dependency" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  let inner_started = Fiber.Ivar.create () in
  let inspect_roots = Fiber.Ivar.create () in
  let inner =
    Memo.Lazy.create ~name:"distinct-roots-inner" (fun () ->
      Memo.of_reproducible_fiber
        (let* () = Fiber.Ivar.fill inner_started () in
         let* () = Fiber.Ivar.read inspect_roots in
         let+ roots = Memo.Job_priority.For_tests.current_node_roots () in
         let classes =
           List.map roots ~f:(fun (demand_class, _) ->
             match demand_class with
             | Demand_class.Bulk -> "bulk"
             | Normal -> "normal"
             | Direct -> "direct")
           |> List.sort ~compare:String.compare
         in
         Printf.printf "roots: %s\n" (String.concat ~sep:"," classes)))
  in
  let outer =
    Memo.Lazy.create ~name:"distinct-roots-outer" (fun () -> Memo.Lazy.force inner)
  in
  go ~config:priority_config (fun () ->
    Fiber.fork_and_join_unit
      (fun () -> run_with_demand Demand_class.Bulk (Memo.Lazy.force outer))
      (fun () ->
         let* () = Fiber.Ivar.read inner_started in
         Fiber.fork_and_join_unit
           (fun () -> run_with_demand Demand_class.Bulk (Memo.Lazy.force outer))
           (fun () -> Fiber.Ivar.fill inspect_roots ())));
  [%expect {| roots: bulk,bulk |}]
;;

let%expect_test "late direct demand promotes nested work without leaking upward" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  let order = ref [] in
  let record name = order := name :: !order in
  let inner_ready = Fiber.Ivar.create () in
  let inner =
    Memo.Lazy.create ~name:"shared-inner" (fun () ->
      Memo.of_reproducible_fiber
        (let* () = Fiber.Ivar.fill inner_ready () in
         Scheduler.with_job_slot (fun () ->
           let+ roots = Memo.Job_priority.For_tests.current_node_roots () in
           let classes =
             List.map roots ~f:(fun (demand_class, _) ->
               match demand_class with
               | Demand_class.Bulk -> "bulk"
               | Normal -> "normal"
               | Direct -> "direct")
             |> List.sort ~compare:String.compare
           in
           Printf.printf "shared roots: %s\n" (String.concat ~sep:"," classes);
           record "shared")))
  in
  let outer = Memo.Lazy.create ~name:"shared-outer" (fun () -> Memo.Lazy.force inner) in
  let bulk_caller =
    Memo.Lazy.create ~name:"bulk-caller" (fun () ->
      let open Memo.O in
      let* () = Memo.Lazy.force outer in
      Memo.of_reproducible_fiber
        (Scheduler.with_job_slot (fun () ->
           record "bulk-continuation";
           Fiber.return ())))
  in
  let normal =
    Memo.Lazy.create ~name:"normal" (fun () ->
      Memo.of_reproducible_fiber
        (Scheduler.with_job_slot (fun () ->
           record "normal";
           Fiber.return ())))
  in
  let blocker_started = Fiber.Ivar.create () in
  let release_blocker = Fiber.Ivar.create () in
  go ~config:priority_config (fun () ->
    Fiber.parallel_iter
      [ (fun () ->
          Scheduler.with_job_slot (fun () ->
            let* () = Fiber.Ivar.fill blocker_started () in
            Fiber.Ivar.read release_blocker))
      ; (fun () ->
          let* () = Fiber.Ivar.read blocker_started in
          run_with_demand Demand_class.Bulk (Memo.Lazy.force bulk_caller))
      ; (fun () ->
          let* () = Fiber.Ivar.read inner_ready in
          Fiber.fork_and_join_unit
            (fun () -> run_with_demand Demand_class.Normal (Memo.Lazy.force normal))
            (fun () ->
               Fiber.fork_and_join_unit
                 (fun () -> run_with_demand Demand_class.Direct (Memo.Lazy.force outer))
                 (fun () -> Fiber.Ivar.fill release_blocker ())))
      ]
      ~f:(fun f -> f ()));
  List.rev !order |> List.iter ~f:print_endline;
  [%expect
    {|
    shared roots: bulk,direct
    shared
    normal
    bulk-continuation |}]
;;

let%expect_test "demand classes propagate while restoring Memo dependencies" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  let order = ref [] in
  let job name =
    let inner_node, inner =
      Memo.Lazy.Expert.create
        ~name:("restore-inner-" ^ name)
        ~cutoff:Unit.equal
        (fun () ->
           Memo.of_reproducible_fiber
             (Scheduler.with_job_slot (fun () ->
                order := name :: !order;
                Fiber.return ())))
    in
    let outer =
      Memo.Lazy.create ~name:("restore-outer-" ^ name) (fun () -> Memo.Lazy.force inner)
    in
    inner_node, outer
  in
  let bulk_node, bulk = job "bulk" in
  let direct_node, direct = job "direct" in
  go ~config:priority_config (fun () ->
    Memo.parallel_map [ bulk; direct ] ~f:Memo.Lazy.force |> Memo.run >>| ignore);
  order := [];
  Memo.reset
    (Memo.Invalidation.combine
       (Memo.Node.invalidate ~reason:Memo.Invalidation.Reason.Test bulk_node)
       (Memo.Node.invalidate ~reason:Memo.Invalidation.Reason.Test direct_node));
  let blocker_started = Fiber.Ivar.create () in
  let release_blocker = Fiber.Ivar.create () in
  go ~config:priority_config (fun () ->
    Fiber.fork_and_join_unit
      (fun () ->
         Scheduler.with_job_slot (fun () ->
           let* () = Fiber.Ivar.fill blocker_started () in
           Fiber.Ivar.read release_blocker))
      (fun () ->
         let* () = Fiber.Ivar.read blocker_started in
         Fiber.parallel_iter
           [ (fun () -> run_with_demand Demand_class.Bulk (Memo.Lazy.force bulk))
           ; (fun () -> run_with_demand Demand_class.Direct (Memo.Lazy.force direct))
           ; (fun () -> Fiber.Ivar.fill release_blocker ())
           ]
           ~f:(fun f -> f ())));
  List.rev !order |> List.iter ~f:print_endline;
  [%expect
    {|
    direct
    bulk |}]
;;

let%expect_test "a priority reservation survives asynchronous bookkeeping" =
  let order = ref [] in
  let record name = order := name :: !order in
  let low =
    Memo.Lazy.create ~name:"low" (fun () ->
      Memo.of_reproducible_fiber
        (Scheduler.with_job_slot (fun () ->
           record "low";
           Fiber.return ())))
  in
  let high =
    Memo.Lazy.create ~name:"high" (fun () ->
      Memo.of_reproducible_fiber
        (let* () =
           Scheduler.with_job_slot (fun () ->
             record "high-1";
             Fiber.return ())
         in
         let* () = Scheduler.async_exn (fun () -> Thread.delay 0.02) in
         let* result =
           Scheduler.async (fun () ->
             Thread.delay 0.02;
             raise Exit)
         in
         (match result with
          | Error _ -> ()
          | Ok () -> Code_error.raise "background failure was not reported" []);
         Scheduler.with_job_slot (fun () ->
           record "high-2";
           Fiber.return ())))
  in
  let module Demand_class = Memo.Job_priority.Demand_class in
  let blocker_started = Fiber.Ivar.create () in
  let release_blocker = Fiber.Ivar.create () in
  go ~config:priority_config (fun () ->
    Fiber.fork_and_join_unit
      (fun () ->
         Scheduler.with_job_slot (fun () ->
           let* () = Fiber.Ivar.fill blocker_started () in
           Fiber.Ivar.read release_blocker))
      (fun () ->
         let* () = Fiber.Ivar.read blocker_started in
         Fiber.parallel_iter
           [ (fun () -> run_with_demand Demand_class.Bulk (Memo.Lazy.force low))
           ; (fun () -> run_with_demand Demand_class.Direct (Memo.Lazy.force high))
           ; (fun () -> Fiber.Ivar.fill release_blocker ())
           ]
           ~f:(fun f -> f ())));
  List.rev !order |> List.iter ~f:print_endline;
  [%expect
    {|
    high-1
    high-2
    low |}]
;;

let%expect_test "priority scheduling is disabled by default" =
  let order = ref [] in
  let blocker_started = Fiber.Ivar.create () in
  let release_blocker = Fiber.Ivar.create () in
  go (fun () ->
    let* low = Scheduler.create_job_priority () in
    let* high = Scheduler.create_job_priority ~priority:2 () in
    let run name priority =
      Scheduler.with_job_slot ~priority (fun () ->
        order := name :: !order;
        Fiber.return ())
    in
    Fiber.fork_and_join_unit
      (fun () ->
         Scheduler.with_job_slot (fun () ->
           let* () = Fiber.Ivar.fill blocker_started () in
           Fiber.Ivar.read release_blocker))
      (fun () ->
         let* () = Fiber.Ivar.read blocker_started in
         Fiber.parallel_iter
           [ (fun () -> run "low" low)
           ; (fun () -> run "high" high)
           ; (fun () -> Fiber.Ivar.fill release_blocker ())
           ]
           ~f:(fun f -> f ())));
  List.rev !order |> List.iter ~f:print_endline;
  [%expect
    {|
    low
    high |}]
;;

let%expect_test "a deferred priority restart cannot strand waiters" =
  let blocker_started = Fiber.Ivar.create () in
  let release_high = Fiber.Ivar.create () in
  let release_blocker = Fiber.Ivar.create () in
  go
    ~timeout:(Time.Span.of_secs 1.0)
    ~config:{ priority_config with concurrency = 2 }
    (fun () ->
       let* high = Scheduler.create_job_priority ~priority:2 () in
       let* low = Scheduler.create_job_priority () in
       Fiber.parallel_iter
         [ (fun () ->
             Scheduler.with_job_slot ~priority:high (fun () ->
               Fiber.Ivar.read release_high))
         ; (fun () ->
             Scheduler.with_job_slot (fun () ->
               let* () = Fiber.Ivar.fill blocker_started () in
               Fiber.Ivar.read release_blocker))
         ; (fun () ->
             let* () = Fiber.Ivar.read blocker_started in
             Fiber.fork_and_join_unit
               (fun () ->
                  Scheduler.with_job_slot ~priority:low (fun () ->
                    print_endline "low resumed";
                    Fiber.Ivar.fill release_blocker ()))
               (fun () -> Fiber.Ivar.fill release_high ()))
         ]
         ~f:(fun f -> f ()));
  [%expect {| low resumed |}]
;;

let%expect_test "Memo priorities propagate through dependency chains" =
  let order = ref [] in
  let job name =
    Memo.Lazy.create ~name:("job-" ^ name) (fun () ->
      Memo.of_reproducible_fiber
        (Scheduler.with_job_slot (fun () ->
           order := name :: !order;
           Fiber.return ())))
  in
  let low = job "low" in
  let chain =
    List.init 3 ~f:Fun.id
    |> List.fold_left ~init:(job "chain-0") ~f:(fun dependency n ->
      let name = sprintf "chain-%d" (n + 1) in
      Memo.Lazy.create ~name:(name ^ "-consumer") (fun () ->
        let open Memo.O in
        let* () = Memo.Lazy.force dependency in
        Memo.of_reproducible_fiber
          (Scheduler.with_job_slot (fun () ->
             order := name :: !order;
             Fiber.return ()))))
  in
  let module Demand_class = Memo.Job_priority.Demand_class in
  let blocker_started = Fiber.Ivar.create () in
  let release_blocker = Fiber.Ivar.create () in
  go ~config:priority_config (fun () ->
    Fiber.fork_and_join_unit
      (fun () ->
         Scheduler.with_job_slot (fun () ->
           let* () = Fiber.Ivar.fill blocker_started () in
           Fiber.Ivar.read release_blocker))
      (fun () ->
         let* () = Fiber.Ivar.read blocker_started in
         Fiber.parallel_iter
           [ (fun () -> run_with_demand Demand_class.Bulk (Memo.Lazy.force low))
           ; (fun () -> run_with_demand Demand_class.Direct (Memo.Lazy.force chain))
           ; (fun () -> Fiber.Ivar.fill release_blocker ())
           ]
           ~f:(fun f -> f ())));
  List.rev !order |> List.iter ~f:print_endline;
  [%expect
    {|
    chain-0
    chain-1
    chain-2
    chain-3
    low |}]
;;

let%expect_test "raise inside Scheduler.Run.go" =
  (try
     (go
      @@ fun () ->
      Fiber.fork_and_join_unit
        (fun () ->
           print_endline "t1";
           Fiber.return ())
        (fun () -> raise Exit));
     assert false
   with
   | Dune_util.Report_error.Already_reported -> print_endline "--> exception observed");
  [%expect
    {|
    t1
    Error: exception Stdlib.Exit

    I must not crash.  Uncertainty is the mind-killer. Exceptions are the
    little-death that brings total obliteration.  I will fully express my cases.
    Execution will pass over me and through me.  And when it has gone past, I
    will unwind the stack along its path.  Where the cases are handled there will
    be nothing.  Only I will remain.
    --> exception observed |}]
;;

let canonical_signal_number signal =
  let signal = Signal.to_int signal in
  let previous_mask = Unix.sigprocmask SIG_UNBLOCK [ signal ] in
  Exn.protect
    ~finally:(fun () -> ignore (Unix.sigprocmask SIG_SETMASK previous_mask : int list))
    ~f:(fun () ->
      let mask_without_signal = Unix.sigprocmask SIG_BLOCK [] in
      ignore (Unix.sigprocmask SIG_BLOCK [ signal ] : int list);
      let mask_with_signal = Unix.sigprocmask SIG_BLOCK [] in
      match
        List.filter mask_with_signal ~f:(fun signal ->
          not (List.mem mask_without_signal signal ~equal:Int.equal))
      with
      | [ signal ] -> signal
      | mask ->
        Code_error.raise
          "could not determine the signal's mask representation"
          [ "mask", Dyn.list Dyn.int mask ])
;;

let terminal_signals_are_blocked terminal_signals =
  let mask = Unix.sigprocmask SIG_BLOCK [] in
  List.for_all terminal_signals ~f:(fun signal -> List.mem mask signal ~equal:Int.equal)
;;

let%expect_test "threaded console handles terminal signals in the console thread" =
  let terminal_signals = List.map Terminal_signals.signals ~f:Signal.to_int in
  let terminal_signals_in_mask =
    List.map Terminal_signals.signals ~f:canonical_signal_number
  in
  let console_thread_blocked = ref None in
  let observation_mutex = Mutex.create () in
  let observed = Condition.create () in
  let observe_from_console_thread () =
    Mutex.protect observation_mutex (fun () ->
      if Option.is_none !console_thread_blocked
      then (
        console_thread_blocked
        := Some (terminal_signals_are_blocked terminal_signals_in_mask);
        Condition.broadcast observed))
  in
  let wait_for_console_thread_observation () =
    Mutex.protect observation_mutex (fun () ->
      while Option.is_none !console_thread_blocked do
        Condition.wait observed observation_mutex
      done;
      Option.value_exn !console_thread_blocked)
  in
  let previous_mask = Unix.sigprocmask SIG_BLOCK [] in
  Exn.protect
    ~finally:(fun () ->
      Console.Backend.set Console.Backend.dumb;
      ignore (Unix.sigprocmask SIG_SETMASK previous_mask : int list))
    ~f:(fun () ->
      ignore (Unix.sigprocmask SIG_UNBLOCK terminal_signals : int list);
      Printf.printf
        "main before start blocked: %b\n"
        (terminal_signals_are_blocked terminal_signals_in_mask);
      let module Test_console = struct
        let start () = ()
        let render (_ : Dune_threaded_console.state) = ()

        let handle_user_events ~now ~time_budget:_ (_ : Mutex.t) _ =
          observe_from_console_thread ();
          Unix.sleepf 0.01;
          now
        ;;

        let reset () = ()
        let reset_flush_history () = ()
        let finish () = ()
      end
      in
      Console.Backend.set
        (Dune_threaded_console.make ~frames_per_second:60 (module Test_console));
      Printf.printf
        "main after start blocked: %b\n"
        (terminal_signals_are_blocked terminal_signals_in_mask);
      Printf.printf
        "console thread blocked: %b\n"
        (wait_for_console_thread_observation ()));
  [%expect
    {|
    main before start blocked: false
    main after start blocked: true
    console thread blocked: false
    |}]
;;
