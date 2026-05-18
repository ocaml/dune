open Stdune

let warning =
  {|

**************************************************************
* Press Control+C again quickly to perform an emergency exit *
**************************************************************

|}
;;

let warning_bytes = Bytes.of_string warning
let signals = if Sys.win32 then [ Signal.Int ] else Thread0.interrupt_signals
let signal_numbers = lazy (List.map signals ~f:Signal.to_int)

external sys_exit : int -> _ = "caml_sys_exit"
external install : int list -> unit = "dune_signal_watcher_install"
external uninstall : unit -> unit = "dune_signal_watcher_uninstall"

type action =
  | Continue
  | Reap_processes
  | Shutdown of Shutdown.Reason.t

let shutdown_reason = function
  | (Signal.Int | Quit | Term) as signal -> Some (Shutdown.Reason.Signal signal)
  | _ -> None
;;

let is_exit_signal signal = Option.is_some (shutdown_reason signal)

let print_ctrl_c_warning () =
  try
    ignore
      (Unix.single_write Unix.stderr warning_bytes 0 (Bytes.length warning_bytes) : int)
  with
  | Unix.Unix_error _ -> ()
;;

let emergency_exit () = sys_exit 1

let with_signals_blocked f =
  if Sys.win32
  then f ()
  else (
    let previous_mask = Unix.sigprocmask SIG_BLOCK (Lazy.force signal_numbers) in
    Exn.protect ~f ~finally:(fun () ->
      ignore (Unix.sigprocmask SIG_SETMASK previous_mask : int list)))
;;

let uninstall_and_stop q =
  Exn.protect ~f:uninstall ~finally:(fun () -> Event.Queue.stop_using_signal_wakeup q)
;;

let init q =
  let install_with_cleanup () =
    Event.Queue.use_for_signal_wakeup q;
    match install (Lazy.force signal_numbers) with
    | () -> ()
    | exception exn ->
      (match uninstall_and_stop q with
       | () -> ()
       | exception _ -> ());
      Exn.raise exn
  in
  with_signals_blocked install_with_cleanup
;;

let cleanup q = with_signals_blocked (fun () -> uninstall_and_stop q)

let with_ q ~f =
  init q;
  Exn.protect ~finally:(fun () -> cleanup q) ~f
;;

let handle signal =
  Dune_trace.emit Process (fun () -> Dune_trace.Event.signal_received signal);
  if Signal.equal signal Thread0.debug_signal
  then (
    Dune_trace.emit Diagnostics (fun () ->
      let dump = Debug.dump () in
      Dune_trace.Event.debug dump);
    Continue)
  else (
    match signal with
    | Chld -> Reap_processes
    | signal ->
      (match shutdown_reason signal with
       | Some reason -> Shutdown reason
       | None -> Continue))
;;
