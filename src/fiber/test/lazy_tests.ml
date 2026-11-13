open Stdune
open Fiber.O
open Dyn
open Common

let%expect_test "Lazy.force_all_unit handles empty and completed inputs" =
  Common.test unit (Fiber.Lazy.force_all_unit []);
  Common.test unit (Fiber.Lazy.force_all_unit [ Fiber.Lazy.unit; Fiber.Lazy.of_value () ]);
  [%expect
    {|
    ()
    () |}]
;;

let%expect_test "Lazy.force_all_unit starts every pending computation" =
  let gate = Fiber.Ivar.create () in
  let started = ref 0 in
  let make () =
    Fiber.Lazy.create (fun () ->
      incr started;
      Fiber.Ivar.read gate)
  in
  let a = make () in
  let b = make () in
  let force () =
    let+ () = Fiber.Lazy.force_all_unit [ a; b ] in
    print_endline "all forced"
  in
  let release () =
    let* () = Scheduler.yield () in
    printfn "started before release: %d" !started;
    Fiber.Ivar.fill gate ()
  in
  Scheduler.run (Fiber.fork_and_join_unit force release);
  printfn "values: %b %b" (Fiber.Lazy.is_value a) (Fiber.Lazy.is_value b);
  [%expect
    {|
    started before release: 2
    all forced
    values: true true |}]
;;

let%expect_test "Lazy.force_all_unit joins a computation already being forced" =
  let runs = ref 0 in
  let lazy_fiber =
    Fiber.Lazy.create (fun () ->
      incr runs;
      Scheduler.yield ())
  in
  Scheduler.run
    (Fiber.fork_and_join_unit
       (fun () -> Fiber.Lazy.force lazy_fiber)
       (fun () -> Fiber.Lazy.force_all_unit [ lazy_fiber ]));
  printfn "runs: %d" !runs;
  printfn "is value: %b" (Fiber.Lazy.is_value lazy_fiber);
  [%expect
    {|
    runs: 1
    is value: true |}]
;;

let%expect_test "Lazy.force_all_unit collects all failures" =
  let make name =
    Fiber.Lazy.create (fun () ->
      let* () = Scheduler.yield () in
      failwith name)
  in
  let a = make "a" in
  let b = make "b" in
  let run () =
    match
      Scheduler.run (Fiber.collect_errors (fun () -> Fiber.Lazy.force_all_unit [ a; b ]))
    with
    | Ok () -> print_endline "unexpected success"
    | Error errors ->
      let errors =
        List.map errors ~f:(fun { Exn_with_backtrace.exn; _ } ->
          match exn with
          | Failure message -> message
          | exn -> Printexc.to_string exn)
        |> List.sort ~compare:String.compare
      in
      printfn "errors: %s" (String.concat ~sep:"," errors)
  in
  run ();
  run ();
  printfn "values: %b %b" (Fiber.Lazy.is_value a) (Fiber.Lazy.is_value b);
  [%expect
    {|
    errors: a,b
    errors: a,b
    values: false false |}]
;;
