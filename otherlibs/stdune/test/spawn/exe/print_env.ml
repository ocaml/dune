let write_file fn s =
  let oc = open_out fn in
  output_string oc s;
  close_out oc
;;

let pdeathsig_child ready_file marker_file =
  Sys.set_signal
    Sys.sigusr1
    (Sys.Signal_handle
       (fun _ ->
         write_file marker_file "got signal\n";
         exit 0));
  write_file ready_file "ready\n";
  while true do
    ignore (Unix.pause ())
  done
;;

let pdeathsig_default_child ready_file marker_file =
  Sys.set_signal Sys.sigterm Sys.Signal_ignore;
  let parent = Unix.getppid () in
  write_file ready_file "ready\n";
  while Unix.getppid () = parent do
    Unix.sleepf 0.01
  done;
  write_file marker_file "survived\n"
;;

let () =
  match Array.to_list Sys.argv with
  | _ :: "pdeathsig-child" :: ready_file :: marker_file :: _ ->
    pdeathsig_child ready_file marker_file
  | _ :: "pdeathsig-default-child" :: ready_file :: marker_file :: _ ->
    pdeathsig_default_child ready_file marker_file
  | _ ->
    (match Sys.getenv "FOO" with
     | exception _ -> print_endline "None"
     | str -> Printf.printf "Some %S\n" str)
;;
