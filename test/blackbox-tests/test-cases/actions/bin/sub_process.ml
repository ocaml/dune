let fn = Sys.getenv "BEACON_FILE"

let handle_sigterm who ~cleanup =
  Sys.set_signal Sys.sigterm
    (Signal_handle
       (fun _ ->
         close_out (open_out (fn ^ "." ^ who));
         cleanup ();
         exit 1))

let sub () =
  handle_sigterm "child" ~cleanup:ignore;
  let oc = open_out fn in
  Printf.fprintf oc "%d" (Unix.getpid ());
  close_out oc;
  Unix.sleep max_int

let () =
  match Unix.fork () with
  | 0 -> sub ()
  | pid ->
    handle_sigterm "parent" ~cleanup:(fun () -> Unix.kill pid Sys.sigterm);
    ignore (Unix.waitpid [] pid : _ * _)
