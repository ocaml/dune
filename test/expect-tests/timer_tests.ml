open Stdune
open Fiber.O
module Scheduler = Dune_engine.Scheduler

let config =
  { Scheduler.Config.concurrency = 1
  ; display = { verbosity = Short; status_line = false }
  ; stats = None
  ; insignificant_changes = `React
  ; signal_watcher = `No
  }

let%expect_test "create and wait for timer" =
  Scheduler.Run.go
    ~on_event:(fun _ _ -> ())
    config
    (fun () ->
      let now () = Unix.gettimeofday () in
      let start = now () in
      let duration = 0.2 in
      let+ () = Scheduler.sleep duration in
      assert (now () -. start >= duration);
      print_endline "timer finished successfully");
  [%expect {| timer finished successfully |}]

let%expect_test "multiple timers" =
  Scheduler.Run.go
    ~on_event:(fun _ _ -> ())
    config
    (fun () ->
      [ 0.3; 0.2; 0.1 ]
      |> Fiber.parallel_iter ~f:(fun duration ->
             let+ () = Scheduler.sleep duration in
             printfn "finished %0.2f" duration));
  [%expect {|
    finished 0.10
    finished 0.20
    finished 0.30 |}]

let%expect_test "run process with timeout" =
  Scheduler.Run.go
    ~on_event:(fun _ _ -> ())
    config
    (fun () ->
      let pid =
        let prog =
          let path =
            Env.get Env.initial "PATH" |> Option.value_exn |> Bin.parse_path
          in
          Bin.which ~path "sleep" |> Option.value_exn |> Path.to_string
        in
        Spawn.spawn ~prog ~argv:[ prog; "100000" ] () |> Pid.of_int
      in
      let+ _ = Scheduler.wait_for_process ~timeout:0.1 pid in
      print_endline "sleep timed out");
  [%expect {|
    sleep timed out |}]
