open Stdune
open Printf
open Lev
module List = ListLabels

let%expect_test "version" =
  let major, minor = ev_version () in
  printf "version (%d, %d)\n" major minor;
  [%expect {| version (4, 33) |}]
;;

let%expect_test "default" =
  ignore (Loop.default ());
  [%expect {||}]
;;

let span_of_secs = Time.Span.of_secs
let immediately = span_of_secs 0.00001

let%expect_test "now" =
  let loop = Loop.default () in
  let (_ : Time.t) = Loop.now loop in
  [%expect {||}]
;;

let%expect_test "sleep" =
  Lev.sleep (span_of_secs 0.1);
  [%expect {||}]
;;

let%expect_test "suspend/resume" =
  let loop = Loop.create () in
  Loop.suspend loop;
  Loop.resume loop;
  [%expect {||}]
;;

let%expect_test "create and run" =
  let ev = Loop.create () in
  (match Loop.run ev Nowait with
   | `No_more_active_watchers -> ()
   | `Otherwise -> assert false);
  [%expect {||}]
;;

let%expect_test "supported backends include the active backend" =
  let loop = Loop.create () in
  let backend = Loop.backend loop in
  printf "supported = %b\n" (Backend.Set.mem (Backend.supported ()) backend);
  [%expect {| supported = true |}]
;;

let%expect_test "timer" =
  let loop = Loop.create () in
  let timer =
    Timer.create ~after:(span_of_secs 0.001) (fun timer ->
      print_endline "fired timer";
      Timer.stop timer loop)
  in
  Timer.start timer loop;
  ignore (Lev.Loop.run loop Once);
  [%expect
    {|
    fired timer |}]
;;

let%expect_test "timer remaining is boxed correctly" =
  let loop = Loop.create () in
  let timer = Timer.create ~after:(span_of_secs 1.0) (fun _ -> ()) in
  Timer.start timer loop;
  let remaining = Time.Span.to_secs (Timer.remaining timer loop) in
  printf "remaining positive = %b\n" (remaining > 0.);
  Timer.stop timer loop;
  [%expect {| remaining positive = true |}]
;;

let%expect_test "timer - not repeats" =
  let loop = Loop.create () in
  let timer = Timer.create ~after:immediately (fun _ -> print_endline "fired timer") in
  Timer.start timer loop;
  ignore (Lev.Loop.run loop Once);
  ignore (Lev.Loop.run loop Once);
  Timer.stop timer loop;
  [%expect
    {|
    fired timer |}]
;;

let%expect_test "timer - cancellation with stop" =
  let loop = Loop.create () in
  let timer =
    Timer.create ~after:(span_of_secs 6.5) (fun _ -> print_endline "fired timer")
  in
  Timer.start timer loop;
  ignore (Lev.Loop.run loop Nowait);
  Timer.stop timer loop;
  ignore (Lev.Loop.run loop Once);
  [%expect {| |}]
;;

let%expect_test "periodic timer" =
  let loop = Loop.create () in
  let timer =
    let count = ref 3 in
    Timer.create ~after:Time.Span.zero ~repeat:(span_of_secs 0.02) (fun timer ->
      if !count = 0
      then (
        let () = print_endline "stopping timer" in
        Timer.stop timer loop)
      else (
        decr count;
        print_endline "fired timer"))
  in
  Timer.start timer loop;
  ignore (Lev.Loop.run_until_done loop);
  [%expect
    {|
    fired timer
    fired timer
    fired timer
    stopping timer |}]
;;

let%expect_test "cleanup callbacks" =
  let loop = Loop.create () in
  let cleanup = Cleanup.create (fun _ -> print_endline "cleanup") in
  Cleanup.start cleanup loop;
  ignore (Loop.run loop Nowait);
  Loop.destroy loop;
  [%expect {| cleanup |}]
;;

let%expect_test "periodic - regular" =
  let loop = Loop.create () in
  let periodic =
    Periodic.create
      (fun p ->
         print_endline "periodic fired";
         Periodic.stop p loop)
      (Regular { offset = Time.add (Loop.now loop) (span_of_secs 0.1); interval = None })
  in
  Periodic.start periodic loop;
  Loop.run_until_done loop;
  [%expect {| periodic fired |}]
;;

let%expect_test "periodic - custom" =
  let loop = Loop.create () in
  let periodic =
    Periodic.create
      (fun p ->
         print_endline "periodic fired";
         Periodic.stop p loop)
      (Custom (fun _ ~now -> Time.add now (span_of_secs 0.2)))
  in
  Periodic.start periodic loop;
  Loop.run_until_done loop;
  [%expect {| periodic fired |}]
;;

let%expect_test "check/idle/prepare" =
  let loop = Loop.create () in
  let check = Check.create (fun _ -> print_endline "check") in
  let idle = Idle.create (fun _ -> print_endline "idle") in
  let prepare = Prepare.create (fun _ -> print_endline "prepare") in
  [ Check.start check; Idle.start idle; Prepare.start prepare ]
  |> List.iter ~f:(fun f -> f loop);
  ignore (Loop.run loop Once);
  [%expect
    {|
    prepare
    check
    idle |}]
;;

let%expect_test "async" =
  let loop = Loop.create () in
  let async = Async.create (fun _ -> print_endline "async fired") in
  let prepare =
    Prepare.create (fun _ ->
      print_endline "firing async";
      Async.send async loop)
  in
  Prepare.start prepare loop;
  Async.start async loop;
  ignore (Loop.run loop Nowait);
  ignore (Loop.run loop Nowait);
  [%expect
    {|
    firing async
    async fired
    firing async
    async fired |}]
;;

let%expect_test "is_pending/is_active" =
  let loop = Loop.create () in
  let idle = Idle.create (fun _ -> print_endline "idle") in
  let print_status () =
    printf "pending = %b; active = %b\n" (Idle.is_pending idle) (Idle.is_active idle)
  in
  print_status ();
  Idle.start idle loop;
  print_status ();
  ignore (Loop.run loop Once);
  print_status ();
  Idle.stop idle loop;
  print_status ();
  [%expect
    {|
    pending = false; active = false
    pending = false; active = true
    idle
    pending = false; active = true
    pending = false; active = false |}]
;;

let%expect_test "destroy" =
  let loop = Loop.create () in
  let idle =
    Idle.create (fun idle ->
      print_endline "idle";
      Idle.stop idle loop)
  in
  Idle.start idle loop;
  ignore (Loop.run loop Once);
  assert (not (Idle.is_active idle));
  assert (not (Idle.is_pending idle));
  print_endline "destroying watcher";
  Idle.destroy idle;
  [%expect
    {|
    idle
    destroying watcher |}]
;;

let%expect_test "timer - stops automatically" =
  let loop = Loop.create () in
  let another = ref true in
  let timer =
    Timer.create ~after:immediately (fun t ->
      print_endline "timer fired";
      if !another
      then (
        another := false;
        Timer.start t loop))
  in
  Timer.start timer loop;
  Loop.run_until_done loop;
  [%expect
    {|
    timer fired
    timer fired |}]
;;

let%expect_test "timer/again cancels start" =
  let loop = Loop.create () in
  let count = ref 0 in
  let timer =
    Timer.create ~after:immediately (fun _ ->
      incr count;
      printf "timer fired %d\n" !count)
  in
  Timer.start timer loop;
  ignore (Loop.run loop Once);
  [%expect {| timer fired 1 |}];
  Timer.start timer loop;
  ignore (Loop.run loop Once);
  [%expect {| timer fired 2 |}];
  (* again does nothing because timer isn't periodic *)
  Timer.again timer loop;
  ignore (Loop.run loop Once);
  [%expect {| |}]
;;

let%expect_test "timer/consecutive again" =
  let loop = Loop.create () in
  let now = Unix.time () in
  let timer =
    Timer.create ~after:(span_of_secs 1.0) (fun t ->
      printf "timer fired after %f\n" (Unix.time () -. now);
      Timer.stop t loop)
  in
  let control =
    let count = ref 3 in
    Timer.create ~after:immediately ~repeat:(span_of_secs 0.2) (fun t ->
      if !count = 0
      then Timer.stop t loop
      else (
        decr count;
        print_endline "resetting timer";
        Timer.again timer loop))
  in
  Timer.start timer loop;
  Timer.start control loop;
  Loop.run_until_done loop;
  [%expect
    {|
    resetting timer
    resetting timer
    resetting timer |}]
;;

exception Idle

let%expect_test "callback exception" =
  let loop = Loop.create () in
  let idle = Idle.create (fun _ -> raise Idle) in
  Idle.start idle loop;
  let with_idle () =
    try ignore (Loop.run loop Once) with
    | Idle -> print_endline "caught idle!"
  in
  with_idle ();
  with_idle ();
  [%expect
    {|
    caught idle!
    caught idle! |}]
;;

let%expect_test "unref" =
  let loop = Loop.create () in
  let check = Check.create (fun _ -> printf "only one iteration\n") in
  Check.start check loop;
  Loop.unref loop;
  ignore (Loop.run_until_done loop);
  [%expect
    {|
    only one iteration
  |}]
;;

let%expect_test "is_default" =
  printf "is_default(default) = %b\n" (Loop.is_default (Loop.default ()));
  printf "is_default(create) = %b\n" (Loop.is_default (Loop.create ()));
  [%expect
    {|
    is_default(default) = true
    is_default(create) = false
  |}]
;;
