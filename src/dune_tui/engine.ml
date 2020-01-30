open Stdune
open Notty

(* The text user interface is designed as a small application inside the Dune
   application. It's structure is similar to the main Dune structure:

   - one thread listen to input events continously

   - one thread receives and process all event synchronously. This thread also
   performs the rendering.

   [SIGWINCH] unix signals that indicate that the terminal has been resized are
   sent from the main signal watcher thread in the main Dune scheduler. *)

type t =
  { buffer : Buffer.t
  ; mutable cursor : (int * int) option
  ; tmachine : Tmachine.t
  ; mutable size : int * int
  }

let size t = t.size

external get_term_size : Unix.file_descr -> int * int
  = "dune_tui_get_size_from_fd"

module Event : sig
  type engine = t

  type t =
    | Input of Unescape.event
    | Resize of int * int

  val next : engine -> t

  val send_input : Unescape.event -> unit

  val send_winch : unit -> unit
end
with type engine := t = struct
  type t =
    | Input of Unescape.event
    | Resize of int * int

  let mutex = Mutex.create ()

  let cond = Condition.create ()

  let input_pending = Queue.create ()

  let winch = ref false

  let available () = (not (Queue.is_empty input_pending)) || !winch

  let next engine =
    Mutex.lock mutex;
    while not (available ()) do
      Condition.wait cond mutex
    done;
    let ev =
      if !winch then (
        winch := false;
        let rows, cols = get_term_size Unix.stdin in
        engine.size <- (rows, cols);
        Resize (rows, cols)
      ) else
        Queue.pop_exn input_pending
    in
    Mutex.unlock mutex;
    ev

  let send_input x =
    Mutex.lock mutex;
    let avail = available () in
    Queue.push input_pending (Input x);
    if not avail then Condition.signal cond;
    Mutex.unlock mutex

  let send_winch () =
    Mutex.lock mutex;
    let avail = available () in
    winch := true;
    if not avail then Condition.signal cond;
    Mutex.unlock mutex
end

(* Cleanup when we stop the text user interface *)
module Cleanup : sig
  val add : (unit -> unit) -> unit
end = struct
  let add = at_exit
end

module Input : sig
  (* Start the thread watching user input and forwarding it to the [Event]
     module *)
  val start : unit -> unit
end = struct
  let read_forever () =
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
      | #Unescape.event as ev -> Event.send_input ev
    in
    loop ()

  let start () = ignore (Thread.create read_forever () : Thread.t)
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
  Input.start ();
  let t =
    { buffer = Buffer.create 65536
    ; cursor = None
    ; tmachine = Tmachine.create ~mouse:false ~bpaste:true Cap.ansi
    ; size = get_term_size Unix.stdin
    }
  in
  ignore (Thread.create main t : Thread.t)
