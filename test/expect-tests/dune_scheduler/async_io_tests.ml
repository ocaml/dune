open Stdune
open Fiber.O
open Dune_scheduler

let config =
  { Scheduler.Config.concurrency = 1
  ; print_ctrl_c_warning = false
  ; watch_exclusions = []
  }
;;

let pipe () =
  let r, w = Unix.pipe ~cloexec:true () in
  Fd.unsafe_of_unix_file_descr r, Fd.unsafe_of_unix_file_descr w
;;

let set_nonblock fd = Unix.set_nonblock (Fd.unsafe_to_unix_file_descr fd)

let write_one fd c =
  let bytes = Bytes.make 1 c in
  assert (Unix.write (Fd.unsafe_to_unix_file_descr fd) bytes 0 1 = 1)
;;

let read_one fd =
  let bytes = Bytes.make 1 '0' in
  assert (Unix.read (Fd.unsafe_to_unix_file_descr fd) bytes 0 1 = 1);
  Bytes.to_string bytes
;;

let%expect_test "read readiness" =
  (Scheduler.Run.go config ~on_event:(fun _ -> ())
   @@ fun () ->
   let r, w = pipe () in
   if not Sys.win32 then set_nonblock r;
   let* task = Async_io.ready r `Read ~f:ignore in
   write_one w '0';
   Async_io.Task.await task
   >>= function
   | Error _ -> assert false
   | Ok () ->
     assert (read_one r = "0");
     Fd.close w;
     let+ () = Async_io.close r in
     print_endline "successful read");
  [%expect {| successful read |}]
;;

let%expect_test "write readiness" =
  (Scheduler.Run.go config ~on_event:(fun _ -> ())
   @@ fun () ->
   let r, w = pipe () in
   if not Sys.win32 then set_nonblock w;
   let* task = Async_io.ready w `Write ~f:ignore in
   Async_io.Task.await task
   >>= function
   | Error _ -> assert false
   | Ok () ->
     write_one w '0';
     Fd.close r;
     let+ () = Async_io.close w in
     print_endline "successful write");
  [%expect {| successful write |}]
;;

let%expect_test "first ready" =
  (Scheduler.Run.go config ~on_event:(fun _ -> ())
   @@ fun () ->
   let r1, w1 = pipe () in
   let r2, w2 = pipe () in
   if not Sys.win32
   then (
     set_nonblock w1;
     set_nonblock w2);
   let* task =
     Async_io.ready_one [ (), w1; (), w2 ] `Write ~f:(fun () fd -> write_one fd '0')
   in
   Async_io.Task.await task
   >>= function
   | Error _ -> assert false
   | Ok () ->
     Fd.close r1;
     Fd.close r2;
     let* () = Async_io.close w1 in
     let+ () = Async_io.close w2 in
     print_endline "successful write");
  [%expect {| successful write |}]
;;

let%expect_test "cancel task" =
  (Scheduler.Run.go config ~on_event:(fun _ -> ())
   @@ fun () ->
   let r, w = pipe () in
   if not Sys.win32 then set_nonblock r;
   let* task = Async_io.ready r `Read ~f:ignore in
   Fiber.fork_and_join_unit
     (fun () ->
        Async_io.Task.await task
        >>= function
        | Ok () | Error (`Exn _) -> assert false
        | Error `Cancelled ->
          Fd.close w;
          let+ () = Async_io.close r in
          print_endline "successfully cancelled")
     (fun () -> Async_io.Task.cancel task));
  [%expect {| successfully cancelled |}]
;;
