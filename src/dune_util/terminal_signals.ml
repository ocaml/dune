open Stdune

let signals : Signal.t list =
  [ Cont (* restore the terminal after process is resumed *)
  ; Tstp (* to restore the terminal after it's stopped (C-z) *)
  ; Winch (* Re-render thet terminal when the terminal is resized *)
  ]
;;

let unblock () =
  if not Sys.win32
  then
    ignore (Unix.sigprocmask SIG_UNBLOCK (List.map ~f:Signal.to_int signals) : int list)
;;
