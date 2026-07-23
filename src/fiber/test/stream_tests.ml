open Stdune
open Fiber.O
open Dyn
open Common

let stream a b =
  let n = ref a in
  Fiber.Stream.In.create (fun () ->
    if !n > b
    then Fiber.return None
    else (
      let x = !n in
      n := x + 1;
      Fiber.return (Some x)))
;;

let%expect_test "Stream.parallel_iter is indeed parallel" =
  let test ~iter_function =
    Scheduler.run
      (iter_function (stream 1 3) ~f:(fun n ->
         Printf.printf "%d: enter\n" n;
         let* () = long_running_fiber () in
         Printf.printf "%d: leave\n" n;
         Fiber.return ()))
  in
  (* The [enter] amd [leave] messages must be interleaved to indicate that the
     calls to [f] are executed in parallel: *)
  test ~iter_function:Fiber.Stream.In.parallel_iter;
  [%expect
    {|
    1: enter
    2: enter
    3: enter
    1: leave
    2: leave
    3: leave |}];
  (* With [sequential_iter] however, The [enter] amd [leave] messages must be
     paired in sequence: *)
  test ~iter_function:Fiber.Stream.In.sequential_iter;
  [%expect
    {|
    1: enter
    1: leave
    2: enter
    2: leave
    3: enter
    3: leave |}]
;;

let%expect_test "Stream.*_iter can be finalized" =
  let test ~iter_function =
    Scheduler.run
      (Fiber.finalize
         ~finally:(fun () ->
           Printf.printf "finalized";
           Fiber.return ())
         (fun () -> iter_function (stream 1 3) ~f:(fun _ -> Fiber.return ())))
  in
  test ~iter_function:Fiber.Stream.In.sequential_iter;
  [%expect {| finalized |}];
  test ~iter_function:Fiber.Stream.In.parallel_iter;
  [%expect {| finalized |}]
;;

let rec naive_stream_parallel_iter (t : _ Fiber.Stream.In.t) ~f =
  Fiber.Stream.In.read t
  >>= function
  | None -> Fiber.return ()
  | Some x ->
    Fiber.fork_and_join_unit
      (fun () ->
         (* without this [yield], our attempt to leak memory is defeated by an
           optimization in [fork_and_join_unit]*)
         Scheduler.yield () >>= fun () -> f x)
      (fun () -> naive_stream_parallel_iter t ~f)
;;

let%expect_test "Stream.parallel_iter doesn't leak" =
  (* Check that a naive [parallel_iter] functions on streams is leaking memory,
     while [Fiber.Stream.parallel_iter] does not. To do that, we construct a
     long stream and iterate over it. At each iteration, we do a full major GC
     and count the number of live words. With the naive implementation, we check
     that this number increases while with the right one we check that this
     number is constant.

     This test is carefully crafted to avoid creating new live words as we
     iterate through the stream. As a result, the only new live words that can
     appear are because of the iteration function. *)
  let test n ~iter_function =
    let finish_stream = Fiber.Ivar.create () in
    let stream =
      let count = ref n in
      Fiber.Stream.In.create (fun () ->
        if !count > 0
        then (
          decr count;
          Fiber.return (Some ()))
        else
          let* () = Fiber.Ivar.read finish_stream in
          Fiber.return None)
    in
    let awaiting = ref n in
    let iter_await = Fiber.Ivar.create () in
    let record () =
      Gc.full_major ();
      let curr = (Gc.stat ()).live_words in
      curr
    in
    let f () =
      decr awaiting;
      if !awaiting = 0 then Fiber.Ivar.fill iter_await () else Fiber.return ()
    in
    Scheduler.run
      (let+ prev, curr =
         Fiber.fork_and_join
           (fun () ->
              let prev = record () in
              let+ () = iter_function stream ~f in
              prev)
           (fun () ->
              let* () = Fiber.Ivar.read iter_await in
              let curr = record () in
              let+ () = Fiber.Ivar.fill finish_stream () in
              curr)
       in
       curr - prev)
  in
  let data_points = [ 1; 10; 100; 1000 ] in
  let rec pair_wise_check ~f = function
    | [] | [ _ ] -> true
    | x :: y :: xs -> f x y && pair_wise_check ~f (y :: xs)
  in
  let test ~pred ~iter_function =
    let results = List.map data_points ~f:(test ~iter_function) in
    if pair_wise_check results ~f:pred
    then print_endline "[PASS] memory usage is as expected"
    else (
      print_endline "[FAIL] memory usage is not as expected";
      Dyn.(list int) results |> print_dyn)
  in
  (* Check that the number of live words keeps on increasing because we are
     leaking memory: *)
  test ~pred:( < ) ~iter_function:naive_stream_parallel_iter;
  [%expect {| [PASS] memory usage is as expected |}];
  test ~pred:( = ) ~iter_function:Fiber.Stream.In.parallel_iter;
  [%expect {| [PASS] memory usage is as expected |}]
;;

let%expect_test "Stream: multiple readers is an error" =
  (* [stream] is so that the first element takes longer to be produced. An
     implementation supporting multiple readers should still yield the first
     element before the second. *)
  let stream =
    let n = ref 0 in
    Fiber.Stream.In.create (fun () ->
      let x = !n in
      n := x + 1;
      let+ () =
        if x = 0
        then
          let* () = long_running_fiber () in
          long_running_fiber ()
        else Fiber.return ()
      in
      Some ())
  in
  Scheduler.run
    (Fiber.fork_and_join_unit
       (fun () ->
          printf "Reader 1 reading\n";
          let+ _x = Fiber.Stream.In.read stream in
          printf "Reader 1 done\n")
       (fun () ->
          let* () = long_running_fiber () in
          printf "Reader 2 reading\n";
          let+ _x = Fiber.Stream.In.read stream in
          printf "Reader 2 done\n"))
[@@expect.uncaught_exn
  {|
  ("(\"Fiber.Stream.In: already reading\", {})")
  Trailing output
  ---------------
  Reader 1 reading
  Reader 2 reading |}]
;;

let%expect_test "Stream: multiple writers is an error" =
  (* [stream] is so that the first element takes longer to be consumed. An
     implementation supporting multiple writers should still yield the first
     element before the second. *)
  let stream =
    Fiber.Stream.Out.create (function
      | Some 1 ->
        let* () = long_running_fiber () in
        long_running_fiber ()
      | _ -> Fiber.return ())
  in
  Scheduler.run
    (Fiber.fork_and_join_unit
       (fun () ->
          printf "Writer 1 writing\n";
          let+ _x = Fiber.Stream.Out.write stream (Some 1) in
          printf "Writer 1 done\n")
       (fun () ->
          let* () = long_running_fiber () in
          printf "Writer 2 writing\n";
          let+ _x = Fiber.Stream.Out.write stream (Some 2) in
          printf "Writer 2 done\n"))
[@@expect.uncaught_exn
  {|
  ("(\"Fiber.Stream.Out: already writing\", {})")
  Trailing output
  ---------------
  Writer 1 writing
  Writer 2 writing |}]
;;

let%expect_test "Stream: writing on a closed stream is an error" =
  Scheduler.run
    (let out =
       Fiber.Stream.Out.create (fun x ->
         print_dyn ((option unit) x);
         Fiber.return ())
     in
     let* () = Fiber.Stream.Out.write out None in
     Fiber.Stream.Out.write out (Some ()))
[@@expect.uncaught_exn
  {|
  ("(\"Fiber.Stream.Out: stream output closed\", {})")
  Trailing output
  ---------------
  None |}]
;;
