open Stdune
open Notty

(* The text user interface is designed as a small application inside the Dune
   application. It's structure is similar to the main Dune structure:

   - one thread listen to input events continously

   - one thread receives and process all event synchronously. This thread also
   performs the rendering.

   [SIGWINCH] unix signals that indicate that the terminal has been resized are
   sent from the main signal watcher thread in the main Dune scheduler. *)

module Event = struct
  type t =
    | Input of Unescape.event
    | Resize of int * int
end

type t =
  { buffer : Buffer.t
  ; mutable cursor : (int * int) option
  ; tmachine : Tmachine.t
  ; mutable size : int * int
  ; mutex : Mutex.t
  ; cond : Condition.t
  ; input_pending : Event.t Queue.t
  ; mutable winch : bool
  }

let size t = t.size

external get_term_size : Unix.file_descr -> int * int
  = "dune_tui_get_size_from_fd"

let available { input_pending; winch; _ } =
  (not (Queue.is_empty input_pending)) || winch

let next ({ mutex; cond; winch; input_pending; _ } as t) =
  Mutex.lock mutex;
  while not (available t) do
    Condition.wait cond mutex
  done;
  let ev =
    if winch then (
      t.winch <- false;
      let rows, cols = get_term_size Unix.stdin in
      t.size <- (rows, cols);
      Event.Resize (rows, cols)
    ) else
      Queue.pop_exn input_pending
  in
  Mutex.unlock mutex;
  ev

let send_input t x =
  Mutex.lock t.mutex;
  let avail = available t in
  Queue.push t.input_pending (Input x);
  if not avail then Condition.signal t.cond;
  Mutex.unlock t.mutex

let send_winch t =
  Mutex.lock t.mutex;
  let avail = available t in
  t.winch <- true;
  if not avail then Condition.signal t.cond;
  Mutex.unlock t.mutex

(* Cleanup when we stop the text user interface *)
module Cleanup : sig
  val add : (unit -> unit) -> unit
end = struct
  let add = at_exit
end

module Input : sig
  (* Start the thread watching user input and forwarding it to the [Event]
     module *)
  val start : t -> unit
end = struct
  let read_forever t =
    let buf_len = 64 * 1024 (* buffer size forthe Unix module *) in
    let buf = Bytes.create buf_len in
    let unescape = Unescape.create () in
    let rec loop () =
      match Unescape.next unescape with
      | `End -> ()
      | `Await ->
        let n = Unix.read Unix.stdin buf 0 buf_len in
        Unescape.input unescape buf 0 n;
        loop ()
      | #Unescape.event as ev -> send_input t ev
    in
    loop ()

  let start t = ignore (Thread.create read_forever t : Thread.t)
end

let update t ~screen ~cursor =
  Buffer.clear t.buffer;
  Tmachine.image t.tmachine screen;
  if cursor <> t.cursor then (
    Tmachine.cursor t.tmachine cursor;
    t.cursor <- cursor
  );
  Tmachine.output t.tmachine t.buffer;
  Buffer.output_buffer stdout t.buffer;
  flush stdout

let start ~main =
  let attr = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin TCSANOW
    { attr with c_icanon = false; c_echo = false };
  Cleanup.add (fun () ->
      try Unix.tcsetattr Unix.stdin TCSANOW attr with _ -> ());
  let t =
    { buffer = Buffer.create 65536
    ; cursor = None
    ; tmachine = Tmachine.create ~mouse:false ~bpaste:true Cap.ansi
    ; size = get_term_size Unix.stdin
    ; input_pending = Queue.create ()
    ; mutex = Mutex.create ()
    ; cond = Condition.create ()
    ; winch = false
    }
  in
  Input.start t;
  ignore (Thread.create main t : Thread.t)
