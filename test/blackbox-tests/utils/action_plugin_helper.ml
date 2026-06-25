open Dune_rpc_lwt.V1.Action_plugin

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

let noop () = run (fun _ -> Lwt.return_unit)

let hold started release =
  touch started;
  wait_for_file release;
  run (fun _ -> Lwt.return_unit)
;;

let touch_and_respond path =
  (try touch path with
   | Sys_error _ -> ());
  run (fun _ -> Lwt.return_unit)
;;

let () =
  match Array.to_list Sys.argv with
  | [ _; "noop" ] -> noop ()
  | [ _; "hold"; started; release ] -> hold started release
  | [ _; "touch"; path ] -> touch_and_respond path
  | _ ->
    prerr_endline
      "Usage: action_plugin_helper (noop | hold <started> <release> | touch <path>)";
    exit 1
;;
