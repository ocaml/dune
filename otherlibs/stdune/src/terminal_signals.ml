let signals : Signal.t list =
  [ Cont (* restore the terminal after process is resumed *)
  ; Tstp (* to restore the terminal after it's stopped (C-z) *)
  ; Winch (* Re-render thet terminal when the terminal is resized *)
  ]
;;

let signal_numbers = lazy (List.map ~f:Signal.to_int signals)

type mask = int list option

let block () =
  if Sys.win32
  then None
  else Some (Unix.sigprocmask SIG_BLOCK (Lazy.force signal_numbers))
;;

let restore = function
  | None -> ()
  | Some mask -> ignore (Unix.sigprocmask SIG_SETMASK mask : int list)
;;

let unblock () =
  if not Sys.win32
  then ignore (Unix.sigprocmask SIG_UNBLOCK (Lazy.force signal_numbers) : int list)
;;
