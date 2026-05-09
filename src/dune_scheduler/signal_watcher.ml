open Stdune

let warning =
  {|

**************************************************************
* Press Control+C again quickly to perform an emergency exit *
**************************************************************

|}
;;

let warning_bytes = Bytes.of_string warning
let one_second = 1.0

external sys_exit : int -> _ = "caml_sys_exit"
external install : int list -> unit = "dune_signal_watcher_install"

type action =
  | Continue
  | Reap_processes
  | Shutdown of Shutdown.Reason.t

let is_exit_signal = function
  | Signal.Int | Quit | Term -> true
  | _ -> false
;;

let write_warning () =
  try
    ignore
      (Unix.single_write Unix.stderr warning_bytes 0 (Bytes.length warning_bytes) : int)
  with
  | Unix.Unix_error _ -> ()
;;

let exit_signals = Queue.create ()

let record_exit_signal ~print_ctrl_c_warning =
  let now = Unix.gettimeofday () in
  Queue.push exit_signals now;
  while
    Queue.length exit_signals > 0 && now -. Queue.peek_exn exit_signals > one_second
  do
    ignore (Queue.pop_exn exit_signals : float)
  done;
  let count = Queue.length exit_signals in
  if count = 2 && print_ctrl_c_warning then write_warning ();
  if count >= 3 then sys_exit 1
;;

let should_print_ctrl_c_warning = ref true

let init ~print_ctrl_c_warning q =
  Queue.clear exit_signals;
  should_print_ctrl_c_warning := print_ctrl_c_warning;
  let signals = if Sys.win32 then [ Signal.Int ] else Thread0.interrupt_signals in
  let signal_numbers = List.map signals ~f:Signal.to_int in
  if Sys.win32
  then (
    Event.Queue.use_for_signal_wakeup q;
    install signal_numbers)
  else (
    ignore (Unix.sigprocmask SIG_BLOCK signal_numbers : int list);
    Exn.protect
      ~f:(fun () ->
        Event.Queue.use_for_signal_wakeup q;
        install signal_numbers)
      ~finally:(fun () -> ignore (Unix.sigprocmask SIG_UNBLOCK signal_numbers : int list)))
;;

let cleanup q =
  Queue.clear exit_signals;
  if Sys.win32
  then Event.Queue.stop_using_signal_wakeup q
  else (
    let signal_numbers = List.map Thread0.interrupt_signals ~f:Signal.to_int in
    ignore (Unix.sigprocmask SIG_BLOCK signal_numbers : int list);
    Exn.protect
      ~f:(fun () -> Event.Queue.stop_using_signal_wakeup q)
      ~finally:(fun () -> ignore (Unix.sigprocmask SIG_UNBLOCK signal_numbers : int list)))
;;

let handle signal =
  if is_exit_signal signal
  then record_exit_signal ~print_ctrl_c_warning:!should_print_ctrl_c_warning;
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
    | Int | Quit | Term -> Shutdown (Signal signal)
    | _ -> Continue)
;;
