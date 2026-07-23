let write_pid path =
  let oc = open_out path in
  Printf.fprintf oc "%d" (Unix.getpid ());
  close_out oc
;;

let sleep_forever () =
  while true do
    Unix.sleep max_int
  done
;;

let signal_ready write_fd =
  let ready = Bytes.of_string "x" in
  ignore (Unix.write write_fd ready 0 1 : int);
  Unix.close write_fd
;;

let wait_ready read_fd =
  let ready = Bytes.create 1 in
  ignore (Unix.read read_fd ready 0 1 : int);
  Unix.close read_fd
;;

let install_term_writer path =
  Sys.set_signal
    Sys.sigterm
    (Sys.Signal_handle
       (fun _ ->
         write_pid path;
         exit 0))
;;

let () =
  match Sys.argv with
  | [| _; "setsid"; pid_file |] ->
    let read_fd, write_fd = Unix.pipe () in
    (match Unix.fork () with
     | 0 ->
       Unix.close read_fd;
       ignore (Unix.setsid () : int);
       write_pid pid_file;
       signal_ready write_fd;
       sleep_forever ()
     | _ ->
       Unix.close write_fd;
       wait_ready read_fd)
  | [| _; "term-child"; parent_pid_file; child_cleanup_file |] ->
    let parent_read_fd, parent_write_fd = Unix.pipe () in
    (match Unix.fork () with
     | 0 ->
       Unix.close parent_read_fd;
       ignore (Unix.setsid () : int);
       let child_read_fd, child_write_fd = Unix.pipe () in
       (match Unix.fork () with
        | 0 ->
          Unix.close parent_write_fd;
          Unix.close child_read_fd;
          install_term_writer child_cleanup_file;
          signal_ready child_write_fd;
          sleep_forever ()
        | _ ->
          Unix.close child_write_fd;
          wait_ready child_read_fd;
          Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> exit 0));
          write_pid parent_pid_file;
          signal_ready parent_write_fd;
          sleep_forever ())
     | _ ->
       Unix.close parent_write_fd;
       wait_ready parent_read_fd)
  | _ -> assert false
;;
