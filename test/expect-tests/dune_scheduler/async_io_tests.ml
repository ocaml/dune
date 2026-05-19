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

let connected_sockets () =
  if not Sys.win32
  then (
    let client, server = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Fd.unsafe_of_unix_file_descr client, Fd.unsafe_of_unix_file_descr server)
  else (
    let listener = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt listener Unix.SO_REUSEADDR true;
    Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    Unix.listen listener 1;
    let addr = Unix.getsockname listener in
    let client = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.connect client addr;
    let server, _ = Unix.accept ~cloexec:true listener in
    Unix.close listener;
    Fd.unsafe_of_unix_file_descr client, Fd.unsafe_of_unix_file_descr server)
;;

let set_nonblock fd = Unix.set_nonblock (Fd.unsafe_to_unix_file_descr fd)

let socketpair () =
  let a, b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fd.unsafe_of_unix_file_descr a, Fd.unsafe_of_unix_file_descr b
;;

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
   let task = Async_io.ready r `Read ~f:ignore in
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
   let task = Async_io.ready w `Write ~f:ignore in
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
   let task =
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
   let task = Async_io.ready r `Read ~f:ignore in
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

let%expect_test "close cancels pending readiness" =
  (Scheduler.Run.go config ~on_event:(fun _ -> ())
   @@ fun () ->
   let r, w = pipe () in
   if not Sys.win32 then set_nonblock r;
   let task = Async_io.ready r `Read ~f:ignore in
   let* res, () =
     Fiber.fork_and_join (fun () -> Async_io.Task.await task) (fun () -> Async_io.close r)
   in
   Fd.close w;
   match res with
   | Error `Cancelled ->
     print_endline "close cancelled waiter";
     Fiber.return ()
   | Ok () | Error (`Exn _) -> assert false);
  [%expect {| close cancelled waiter |}]
;;

let%expect_test "close cancels unawaited task" =
  (Scheduler.Run.go config ~on_event:(fun _ -> ())
   @@ fun () ->
   let r, w = pipe () in
   if not Sys.win32 then set_nonblock r;
   let _task = Async_io.ready r `Read ~f:ignore in
   Fd.close w;
   let+ () = Async_io.close r in
   print_endline "closed cleanly");
  [%expect {| closed cleanly |}]
;;

let%expect_test "close waits for the loop thread to close the fd" =
  (Scheduler.Run.go config ~on_event:(fun _ -> ())
   @@ fun () ->
   let is_closed fd =
     let buf = Bytes.make 1 '0' in
     match Unix.read (Fd.unsafe_to_unix_file_descr fd) buf 0 1 with
     | exception Unix.Unix_error (EBADF, _, _) -> true
     | _ -> false
   in
   let r, w = pipe () in
   if not Sys.win32 then set_nonblock r;
   let _task_to_close = Async_io.ready r `Read ~f:ignore in
   Fd.close w;
   let+ () = Async_io.close r in
   assert (is_closed r);
   print_endline "close waited for loop thread");
  [%expect {| close waited for loop thread |}]
;;

let%expect_test "re-arm when watched mask changes" =
  (Scheduler.Run.go config ~on_event:(fun _ -> ())
   @@ fun () ->
   let a, b = socketpair () in
   set_nonblock a;
   set_nonblock b;
   let read_task = Async_io.ready a `Read ~f:(fun () -> read_one a) in
   write_one b 'a';
   let* () =
     Async_io.Task.await read_task
     >>= function
     | Error _ -> assert false
     | Ok s ->
       print_endline ("read " ^ s);
       Fiber.return ()
   in
   let write_task = Async_io.ready a `Write ~f:(fun () -> write_one a 'b') in
   let* () =
     Async_io.Task.await write_task
     >>= function
     | Error _ -> assert false
     | Ok () ->
       print_endline "wrote b";
       Fiber.return ()
   in
   assert (read_one b = "b");
   let* () = Async_io.close a in
   let+ () = Async_io.close b in
   print_endline "switched mask successfully");
  [%expect
    {|
    read a
    wrote b
    switched mask successfully |}]
;;

let%expect_test "mask switch on same fd" =
  (Scheduler.Run.go config ~on_event:(fun _ -> ())
   @@ fun () ->
   let client, server = connected_sockets () in
   set_nonblock client;
   set_nonblock server;
   let* writable = Async_io.Task.await (Async_io.ready client `Write ~f:ignore) in
   (match writable with
    | Error _ -> assert false
    | Ok () -> ());
   let task = Async_io.ready client `Read ~f:(fun () -> read_one client) in
   write_one server '1';
   Async_io.Task.await task
   >>= function
   | Error _ -> assert false
   | Ok "1" ->
     Fd.close server;
     let+ () = Async_io.close client in
     print_endline "successful mask switch"
   | Ok _ -> assert false);
  [%expect {| successful mask switch |}]
;;

let%expect_test "close cancels ready_one task" =
  (Scheduler.Run.go config ~on_event:(fun _ -> ())
   @@ fun () ->
   let r1, w1 = pipe () in
   let r2, w2 = pipe () in
   if not Sys.win32
   then (
     set_nonblock r1;
     set_nonblock r2);
   let task =
     Async_io.ready_one [ `First, r1; `Second, r2 ] `Read ~f:(fun which _fd -> which)
   in
   let* () = Async_io.close r1 in
   let* res = Async_io.Task.await task in
   Fd.close w1;
   Fd.close w2;
   let* () = Async_io.close r2 in
   match res with
   | Error `Cancelled ->
     print_endline "ready_one cancelled";
     Fiber.return ()
   | Ok _ | Error (`Exn _) -> assert false);
  [%expect {| ready_one cancelled |}]
;;

let%expect_test "sleep after shutdown is rejected" =
  let events = Dune_scheduler__Event.Queue.create () in
  let (t : Dune_scheduler__Types.Async_io.t) = Async_io.create events in
  Async_io.shutdown t;
  (try
     ignore (Async_io.sleep t Time.Span.zero);
     print_endline "FAIL: sleep succeeded"
   with
   | Code_error.E _ ->
     (match t.thread with
      | None -> print_endline "sleep rejected without restart"
      | Some _ -> print_endline "FAIL: sleep restarted the loop"));
  [%expect {| sleep rejected without restart |}]
;;
