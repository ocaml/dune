open Dune_action_plugin.V1

let touch path =
  let oc = open_out path in
  close_out oc
;;

let rec wait_for_file path =
  if not (Sys.file_exists path)
  then (
    ignore (Unix.select [] [] [] 0.01);
    wait_for_file path)
;;

let noop () = run (return ())

let read_file path =
  let open Dune_action_plugin.V1.O in
  let+ _contents = read_file ~path:(Path.of_string path) in
  ()
;;

let hold started release =
  touch started;
  wait_for_file release;
  run (return ())
;;

let touch_and_respond path =
  (try touch path with
   | Sys_error _ -> ());
  run (return ())
;;

let () =
  match Array.to_list Sys.argv with
  | [ _; "noop" ] -> noop ()
  | [ _; "read-file"; path ] -> run (read_file path)
  | [ _; "hold"; started; release ] -> hold started release
  | [ _; "touch"; path ] -> touch_and_respond path
  | _ ->
    prerr_endline
      "Usage: action_plugin_helper (noop | read-file <path> | hold <started> <release> | touch <path>)";
    exit 1
;;
