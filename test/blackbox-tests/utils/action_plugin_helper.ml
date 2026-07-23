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

let hold started release =
  touch started;
  wait_for_file release;
  run (return ())
;;

let () =
  match Array.to_list Sys.argv with
  | [ _; "noop" ] -> noop ()
  | [ _; "hold"; started; release ] -> hold started release
  | _ ->
    prerr_endline "Usage: action_plugin_helper (noop | hold <started> <release>)";
    exit 1
;;
