open Stdune
open Fiber.O
module Scheduler = Dune_engine.Scheduler
open Dune_async_io

let config =
  { Scheduler.Config.concurrency = 1
  ; stats = None
  ; insignificant_changes = `Ignore
  ; signal_watcher = `No
  ; watch_exclusions = []
  }

let%expect_test "read readiness" =
  ( Scheduler.Run.go config ~on_event:(fun _ _ -> ()) @@ fun () ->
    let r, w = Unix.pipe ~cloexec:true () in
    if not Sys.win32 then Unix.set_nonblock r;
    let* task = Async_io.ready r `Read ~f:ignore in
    assert (Unix.write w (Bytes.of_string "0") 0 1 = 1);
    Async_io.Task.await task >>= function
    | Error _ -> assert false
    | Ok () ->
      let bytes = Bytes.of_string "1" in
      assert (Unix.read r bytes 0 1 = 1);
      assert (Bytes.to_string bytes = "0");
      Unix.close w;
      let+ () = Async_io.close r in
      print_endline "succesful read" );
  [%expect {| succesful read |}]

let%expect_test "write readiness" =
  ( Scheduler.Run.go config ~on_event:(fun _ _ -> ()) @@ fun () ->
    let r, w = Unix.pipe ~cloexec:true () in
    if not Sys.win32 then Unix.set_nonblock w;
    let* task = Async_io.ready w `Write ~f:ignore in
    Async_io.Task.await task >>= function
    | Error _ -> assert false
    | Ok () ->
      assert (Unix.write w (Bytes.of_string "0") 0 1 = 1);
      Unix.close r;
      let+ () = Async_io.close w in
      print_endline "succesful write" );
  [%expect {| succesful write |}]

let%expect_test "cancel task" =
  ( Scheduler.Run.go config ~on_event:(fun _ _ -> ()) @@ fun () ->
    let r, w = Unix.pipe ~cloexec:true () in
    if not Sys.win32 then Unix.set_nonblock r;
    let* task = Async_io.ready r `Read ~f:ignore in
    Fiber.fork_and_join_unit
      (fun () ->
        Async_io.Task.await task >>= function
        | Ok () | Error (`Exn _) -> assert false
        | Error `Cancelled ->
          Unix.close w;
          let+ () = Async_io.close r in
          print_endline "successfully cancelled")
      (fun () -> Async_io.Task.cancel task) );
  [%expect {| successfully cancelled |}]
