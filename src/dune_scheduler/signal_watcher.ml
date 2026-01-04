open Stdune

let signos = List.map Thread0.interrupt_signals ~f:Signal.to_int

let warning =
  {|

**************************************************************
* Press Control+C again quickly to perform an emergency exit *
**************************************************************

|}
;;

external sys_exit : int -> _ = "caml_sys_exit"

let signal_waiter () =
  if Sys.win32
  then (
    let r, w = Unix.pipe ~cloexec:true () in
    let buf = Bytes.create 1 in
    Sys.set_signal Sys.sigint (Signal_handle (fun _ -> assert (Unix.write w buf 0 1 = 1)));
    Staged.stage (fun () ->
      assert (Unix.read r buf 0 1 = 1);
      Signal.Int))
  else Staged.stage (fun () -> Thread.wait_signal signos |> Signal.of_int)
;;

let run ~print_ctrl_c_warning q =
  let last_exit_signals = Queue.create () in
  let one_second = Time.Span.of_secs 1. in
  let wait_signal = Staged.unstage (signal_waiter ()) in
  while true do
    let signal = wait_signal () in
    Dune_trace.emit Process (fun () -> Dune_trace.Event.signal_received signal);
    Event.Queue.send_shutdown q (Signal signal);
    match signal with
    | Int | Quit | Term ->
      let now = Time.now () in
      Queue.push last_exit_signals now;
      (* Discard old signals *)
      while
        Queue.length last_exit_signals >= 0
        && Poly.(Time.diff now (Queue.peek_exn last_exit_signals) > one_second)
      do
        ignore (Queue.pop_exn last_exit_signals : Time.t)
      done;
      let n = Queue.length last_exit_signals in
      if n = 2 && print_ctrl_c_warning then prerr_endline warning;
      if n = 3 then sys_exit 1
    | _ -> (* we only blocked the signals above *) assert false
  done
;;

let init ~print_ctrl_c_warning q = Thread0.spawn (fun () -> run ~print_ctrl_c_warning q)
