open Stdune
include Dune_scheduler
open Dune_tests_common
open Fiber.O
module Thread_safe_channel = Dune_scheduler.For_tests.Thread_safe_channel

let () = init ()

let next_fills event_queue =
  match Event.Queue.next event_queue with
  | Fiber_fill_ivar fill -> [ fill ]
  | Shutdown _ | Job_complete_ready -> assert false
;;

let create_channel () =
  let event_queue = Event.Queue.create () in
  let channel = Thread_safe_channel.create event_queue in
  event_queue, channel
;;

let run event_queue fiber = Fiber.run fiber ~iter:(fun () -> next_fills event_queue)

let run_with_first_iter_action event_queue fiber ~f =
  let did_run = ref false in
  let iter () =
    if not !did_run
    then (
      did_run := true;
      f ());
    next_fills event_queue
  in
  Fiber.run fiber ~iter
;;

let rec read_all channel acc =
  Thread_safe_channel.read channel
  >>= function
  | None -> Fiber.return (List.rev acc)
  | Some value -> read_all channel (value :: acc)
;;

let require_ok = function
  | `Ok -> ()
  | `Closed -> Code_error.raise "channel unexpectedly closed" []
;;

let write_values channel values =
  List.iter values ~f:(fun value -> require_ok (Thread_safe_channel.write channel value))
;;

let write_fill channel ivar =
  Thread_safe_channel.write_fill channel (Fiber.Fill (ivar, ()))
;;

let write_fill_ok channel ivar = require_ok (write_fill channel ivar)

let print_write_status label = function
  | `Ok -> printfn "%s: Ok" label
  | `Closed -> printfn "%s: Closed" label
;;

let print_read_result label = function
  | None -> printfn "%s: closed" label
  | Some value -> printfn "%s: %d" label value
;;

let read_and_print channel label =
  Thread_safe_channel.read channel >>| print_read_result label
;;

let close_after_fill channel ivar =
  let+ () = Fiber.Ivar.read ivar in
  print_endline "fill";
  Thread_safe_channel.close channel
;;

let%expect_test "reads queued values before reporting closed" =
  let event_queue, channel = create_channel () in
  write_values channel [ 1; 2; 3 ];
  Thread_safe_channel.close channel;
  run event_queue (read_all channel []) |> Dyn.list Dyn.int |> print_dyn;
  [%expect {| [ 1; 2; 3 ] |}]
;;

let%expect_test "write wakes waiting reader" =
  let event_queue, channel = create_channel () in
  run_with_first_iter_action event_queue (read_and_print channel "reader") ~f:(fun () ->
    Thread_safe_channel.write channel 42 |> print_write_status "write");
  [%expect
    {|
    write: Ok
    reader: 42 |}]
;;

let%expect_test "close wakes waiting readers" =
  let event_queue, channel = create_channel () in
  run_with_first_iter_action
    event_queue
    (Fiber.fork_and_join_unit
       (fun () -> read_and_print channel "first")
       (fun () -> read_and_print channel "second"))
    ~f:(fun () ->
      print_endline "close";
      Thread_safe_channel.close channel);
  [%expect
    {|
    close
    first: closed
    second: closed |}]
;;

let%expect_test "close is idempotent and writes after close fail" =
  let event_queue, channel = create_channel () in
  Thread_safe_channel.close channel;
  Thread_safe_channel.close channel;
  print_write_status "write" (Thread_safe_channel.write channel 1);
  print_write_status "write_fill" (write_fill channel (Fiber.Ivar.create ()));
  run event_queue (Thread_safe_channel.read channel) |> Dyn.option Dyn.int |> print_dyn;
  [%expect
    {|
    write: Closed
    write_fill: Closed
    None |}]
;;

let%expect_test "write_fill is ordered after queued values" =
  let event_queue, channel = create_channel () in
  write_values channel [ 1; 2 ];
  let flushed = Fiber.Ivar.create () in
  write_fill_ok channel flushed;
  let reader () =
    let rec loop () =
      let* value = Thread_safe_channel.read channel in
      match value with
      | None ->
        print_endline "closed";
        Fiber.return ()
      | Some value ->
        printfn "value: %d" value;
        loop ()
    in
    loop ()
  in
  run
    event_queue
    (Fiber.fork_and_join_unit reader (fun () -> close_after_fill channel flushed));
  [%expect
    {|
    value: 1
    value: 2
    fill
    closed |}]
;;

let%expect_test "write_fill wakes scheduler when readers are waiting" =
  let event_queue, channel = create_channel () in
  let flushed = Fiber.Ivar.create () in
  run_with_first_iter_action
    event_queue
    (Fiber.fork_and_join_unit
       (fun () -> read_and_print channel "reader")
       (fun () -> close_after_fill channel flushed))
    ~f:(fun () -> write_fill channel flushed |> print_write_status "write_fill");
  [%expect
    {|
    write_fill: Ok
    fill
    reader: closed |}]
;;

let%expect_test "wakes fibers from side threads" =
  let event_queue, channel = create_channel () in
  ignore
    (Thread.create
       (fun () ->
          for i = 1 to 3 do
            match Thread_safe_channel.write channel i with
            | `Ok -> ()
            | `Closed -> print_endline "channel closed"
          done;
          Thread_safe_channel.close channel)
       ());
  run event_queue (read_all channel []) |> Dyn.list Dyn.int |> print_dyn;
  [%expect {| [ 1; 2; 3 ] |}]
;;
