open Stdune

type t = Thread.t

let join = Thread.join
let delay = Thread.delay
let debug_signal : Signal.t = Usr2

(* These are the signals that will make the scheduler attempt to terminate dune
   or signal to dune to reap a process *)
let interrupt_signals : Signal.t list = [ Int; Quit; Term; debug_signal; Chld ]

(* In addition, the scheduler also blocks some other signals so that only
   designated threads can handle them by unblocking *)
let blocked_signals : Signal.t list = Terminal_signals.signals @ interrupt_signals
let blocked_signos = lazy (List.map blocked_signals ~f:Signal.to_int)

let create =
  if Sys.win32
  then Thread.create
  else
    fun f x ->
      (* New threads inherit their creator's signal mask. Block scheduler
         signals around [Thread.create] so the child starts with those signals
         blocked, then restore the creator's mask so the main thread remains
         able to handle them via the scheduler's C signal handlers. *)
      let previous_mask = Unix.sigprocmask SIG_BLOCK (Lazy.force blocked_signos) in
      Exn.protect
        ~f:(fun () -> Thread.create f x)
        ~finally:(fun () ->
          ignore (Unix.sigprocmask SIG_SETMASK previous_mask : int list))
;;

let spawn ~name f =
  Dune_trace.emit Thread (fun () -> Dune_trace.Event.spawn_thread ~name);
  let f () =
    try f () with
    | exn ->
      let exn = Exn_with_backtrace.capture exn in
      Dune_util.Report_error.report exn
  in
  create f ()
;;
