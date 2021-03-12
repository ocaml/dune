open Stdune
module Json = Chrome_trace.Json
module Event = Chrome_trace.Event

type dst =
  | Out of out_channel
  | Custom of
      { write : string -> unit
      ; close : unit -> unit
      }

type t =
  { print : string -> unit
  ; close : unit -> unit
  ; buffer : Buffer.t
  ; mutable after_first_event : bool
  }
(* all fields of record used *)

let close { print; close; _ } =
  print "]\n";
  close ()

let create dst =
  let print =
    match dst with
    | Out out -> Stdlib.output_string out
    | Custom c -> c.write
  in
  let close =
    match dst with
    | Out out -> fun () -> Stdlib.close_out out
    | Custom c -> c.close
  in
  let buffer = Buffer.create 1024 in
  { print; close; after_first_event = false; buffer }

let next_leading_char t =
  match t.after_first_event with
  | true -> ','
  | false ->
    t.after_first_event <- true;
    '['

let printf t format_string =
  let c = next_leading_char t in
  Printf.ksprintf t.print ("%c" ^^ format_string ^^ "\n") c

let emit t event = printf t "%s" (Json.to_string (Event.to_json event))

module Fd_count = struct
  type t =
    | Unknown
    | This of int

  let try_to_use_lsof () =
    (* note: we do not use the Process module here, because it would create a
       circular dependency *)
    let temp = Temp.create File ~prefix:"dune." ~suffix:".lsof" in
    let stdout =
      Unix.openfile
        (Path.to_absolute_filename temp)
        [ O_WRONLY; O_CREAT; O_TRUNC; O_SHARE_DELETE ]
        0o666
    in
    let prog = "/usr/sbin/lsof" in
    let argv = [ prog; "-w"; "-p"; string_of_int (Unix.getpid ()) ] in
    let pid = Spawn.spawn ~prog ~argv ~stdout () in
    Unix.close stdout;
    match Unix.waitpid [] (Pid.to_int pid) with
    | _, Unix.WEXITED 0 ->
      let num_lines = List.length (Io.input_lines (Io.open_in temp)) in
      This (num_lines - 1)
    (* the output contains a header line *)
    | _ -> Unknown

  let get () =
    match Sys.readdir "/proc/self/fd" with
    | exception _ -> (
      match try_to_use_lsof () with
      | exception _ -> Unknown
      | value -> value)
    | files -> This (Array.length files - 1 (* -1 for the dirfd *))
end

let record_gc_and_fd stats =
  let module Event = Chrome_trace.Event in
  let module Json = Chrome_trace.Json in
  let ts = Event.Timestamp.now () in
  let () =
    let common = Event.common ~name:"gc" ~ts () in
    let args =
      let stat = Gc.stat () in
      [ ("live_words", Json.Int stat.live_words)
      ; ("free_words", Int stat.free_words)
      ; ("stack_size", Int stat.stack_size)
      ; ("heap_words", Int stat.heap_words)
      ; ("top_heap_words", Int stat.top_heap_words)
      ; ("minor_words", Float stat.minor_words)
      ; ("major_words", Float stat.major_words)
      ; ("promoted_words", Float stat.promoted_words)
      ; ("compactions", Int stat.compactions)
      ; ("major_collections", Int stat.major_collections)
      ; ("minor_collections", Int stat.minor_collections)
      ]
    in
    let event = Event.counter common args in
    emit stats event
  in
  match Fd_count.get () with
  | Unknown -> ()
  | This fds ->
    let event =
      let args = [ ("value", Json.Int fds) ] in
      let common = Event.common ~name:"fds" ~ts () in
      Event.counter common args
    in
    emit stats event
