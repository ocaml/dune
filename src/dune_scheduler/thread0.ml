open Stdune

type t = Thread.t

let join = Thread.join
let delay = Thread.delay
let wait_signal = Thread.wait_signal
let signal_watcher_interrupt : Signal.t = Usr1

(* These are the signals that will make the scheduler attempt to terminate dune
   or signal to dune to reap a process *)
let interrupt_signals : Signal.t list =
  [ signal_watcher_interrupt; Chld; Int; Quit; Term ]
;;

(* In addition, the scheduler also blocks some other signals so that only
   designated threads can handle them by unblocking *)
let blocked_signals : Signal.t list = Terminal_signals.signals @ interrupt_signals

let block_signals =
  lazy
    (let () =
       Sys.set_signal
         (Signal.to_int signal_watcher_interrupt)
         (Signal_handle (fun _ -> ()))
     in
     let signos = List.map blocked_signals ~f:Signal.to_int in
     ignore (Unix.sigprocmask SIG_BLOCK signos : int list))
;;

let create =
  if Sys.win32
  then Thread.create
  else
    (* On unix, we make sure to block signals globally before starting a
         thread so that only the signal watcher thread can receive signals. *)
    fun f x ->
      Lazy.force block_signals;
      Thread.create f x
;;

let spawn f =
  let f () =
    try f () with
    | exn ->
      let exn = Exn_with_backtrace.capture exn in
      Dune_util.Report_error.report exn
  in
  create f ()
;;
