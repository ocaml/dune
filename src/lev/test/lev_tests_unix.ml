open Stdune
open Printf
open Lev

let%expect_test "child" =
  let loop = Loop.default () in
  let stdin, stdin_w = Unix.pipe ~cloexec:true () in
  let stdout_r, stdout = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr = Unix.pipe ~cloexec:true () in
  Unix.close stdin_w;
  Unix.close stdout_r;
  Unix.close stderr_r;
  let pid = Unix.create_process "sh" [| "sh"; "-c"; "exit 42" |] stdin stdout stderr in
  let child =
    match Child.create with
    | Error `Unimplemented -> assert false
    | Ok create ->
      create
        (fun t ~pid:pid' status ->
           Child.stop t loop;
           (match status with
            | Unix.WEXITED i -> printf "exited with status %d\n" i
            | _ -> assert false);
           assert (pid = pid'))
        (Pid pid)
        Terminate
  in
  Child.start child loop;
  Loop.run_until_done loop;
  [%expect {| exited with status 42 |}]
;;

let%expect_test "read from pipe" =
  let r, w = Unix.pipe () in
  Unix.set_nonblock r;
  Unix.set_nonblock w;
  let loop = Loop.create () in
  let io_r =
    Io.create
      (fun io fd events ->
         let b = Bytes.make 1 '0' in
         match Unix.read fd b 0 1 with
         | exception Unix.Unix_error (EAGAIN, _, _) -> ()
         | s ->
           assert (Io.Event.Set.mem events Read);
           assert (s = 1);
           printf "read char %s\n" (Bytes.to_string b);
           Unix.close r;
           Io.stop io loop)
      r
      (Io.Event.Set.create ~read:true ())
  in
  let io_w =
    Io.create
      (fun io fd events ->
         assert (Io.Event.Set.mem events Write);
         ignore (Unix.write fd (Bytes.make 1 'c') 0 1);
         print_endline "written to pipe";
         Unix.close w;
         Io.stop io loop)
      w
      (Io.Event.Set.create ~write:true ())
  in
  Io.start io_r loop;
  Io.start io_w loop;
  ignore (Loop.run_until_done loop);
  [%expect
    {|
    written to pipe
    read char c |}]
;;

let%expect_test "read when write end closed" =
  let r, w = Unix.pipe ~cloexec:true () in
  Unix.close w;
  Unix.set_nonblock r;
  let loop = Loop.create ~flags:(Loop.Flag.Set.singleton (Backend Select)) () in
  let io_r =
    Io.create
      (fun io fd _events ->
         let b = Bytes.make 1 '0' in
         match Unix.read fd b 0 1 with
         | exception Unix.Unix_error (EAGAIN, _, _) -> assert false
         | 0 ->
           Lev.Io.stop io loop;
           print_endline "read 0 bytes"
         | _ -> assert false)
      r
      (Io.Event.Set.create ~read:true ())
  in
  Io.start io_r loop;
  Loop.run_until_done loop;
  [%expect
    {|
    read 0 bytes
  |}]
;;

let%expect_test "watch a closed fd" =
  let r, w = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock r;
  let loop = Loop.create ~flags:(Loop.Flag.Set.singleton (Backend Select)) () in
  let io_r =
    Io.create
      (fun io fd _events ->
         let b = Bytes.make 1 '0' in
         match Unix.read fd b 0 1 with
         | exception Unix.Unix_error (EBADF, _, _) ->
           print_endline "received EBADF";
           Io.stop io loop
         | _ -> assert false)
      r
      (Io.Event.Set.create ~read:true ())
  in
  Io.start io_r loop;
  let check =
    Check.create (fun check ->
      printf "closing after start\n";
      Unix.close w;
      Unix.close r;
      Check.stop check loop)
  in
  Check.start check loop;
  (* XXX why doesn't [run_until_done] work here? *)
  ignore (Loop.run loop Nowait);
  ignore (Loop.run loop Nowait);
  Loop.run_until_done loop;
  [%expect
    {|
    closing after start
    received EBADF
  |}]
;;

let%expect_test "write to closed reader" =
  let r, w = Unix.pipe ~cloexec:true () in
  let loop = Loop.create ~flags:(Loop.Flag.Set.singleton (Backend Select)) () in
  let old_sigpipe = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  Unix.close r;
  let io =
    Io.create
      (fun io fd _ ->
         let bytes = Bytes.of_string "foobar" in
         match Unix.single_write fd bytes 0 (Bytes.length bytes) with
         | exception Unix.Unix_error (Unix.EPIPE, _, _) ->
           print_endline "received epipe";
           Io.stop io loop
         | _ -> assert false)
      w
      (Io.Event.Set.create ~write:true ())
  in
  Io.start io loop;
  Loop.run_until_done loop;
  Sys.set_signal Sys.sigpipe old_sigpipe;
  [%expect {| received epipe |}]
;;

let%expect_test "modify io watcher and preserve watched fd" =
  let left, right = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.set_nonblock left;
  Unix.set_nonblock right;
  let loop = Loop.create () in
  let io =
    Io.create
      (fun io fd events ->
         assert (fd = left);
         assert (Io.fd io = fd);
         assert (Io.Event.Set.mem events Write);
         print_endline "write";
         Io.stop io loop;
         Unix.close left;
         Unix.close right;
         Io.destroy io)
      left
      (Io.Event.Set.create ~read:true ())
  in
  let prepare =
    Prepare.create (fun prepare ->
      Io.modify io (Io.Event.Set.create ~write:true ());
      Prepare.stop prepare loop;
      Prepare.destroy prepare)
  in
  Prepare.start prepare loop;
  Io.start io loop;
  Loop.run_until_done loop;
  [%expect {| write |}]
;;

let%expect_test "stat watcher reports updated file metadata" =
  let path = Stdlib.Filename.temp_file "lev-stat" ".txt" in
  let oc = open_out_bin path in
  output_string oc "a";
  close_out oc;
  let loop = Loop.create () in
  let timer =
    Timer.create ~after:(Time.Span.of_secs 0.01) (fun timer ->
      let oc = open_out_gen [ Open_wronly; Open_append ] 0o666 path in
      output_string oc "bc";
      close_out oc;
      Timer.stop timer loop;
      Timer.destroy timer)
  in
  let watcher =
    match Stat.create with
    | Error `Unimplemented -> assert false
    | Ok create ->
      create ~path (fun watcher ->
        let stat = Stat.stat watcher in
        printf "size = %d\n" stat.Unix.st_size;
        Stat.stop watcher loop;
        Stat.destroy watcher)
  in
  Gc.minor ();
  Gc.full_major ();
  Stat.start watcher loop;
  Timer.start timer loop;
  Loop.run_until_done loop;
  Sys.remove path;
  [%expect {| size = 3 |}]
;;
