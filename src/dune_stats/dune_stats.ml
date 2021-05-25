open Stdune
module Event = Chrome_trace.Event

module Json = struct
  include Chrome_trace.Json

  let quote_string_to_buf s buf =
    (* TODO: escaping is wrong here, in particular for control characters *)
    Buffer.add_string buf (sprintf "%S" s)

  let rec to_buf t buf =
    match t with
    | `String s -> quote_string_to_buf s buf
    | `Int i -> Buffer.add_string buf (string_of_int i)
    | `Float f -> Buffer.add_string buf (Printf.sprintf "%.17g" f)
    | `Bool b -> Buffer.add_string buf (string_of_bool b)
    | `List l ->
      Buffer.add_char buf '[';
      array_body_to_buf l buf;
      Buffer.add_char buf ']'
    | `Assoc o ->
      Buffer.add_char buf '{';
      object_body_to_buf o buf;
      Buffer.add_char buf '}'

  and array_body_to_buf t buf =
    match t with
    | [] -> ()
    | [ x ] -> to_buf x buf
    | x :: xs ->
      to_buf x buf;
      Buffer.add_char buf ',';
      array_body_to_buf xs buf

  and object_body_to_buf t buf =
    match t with
    | [] -> ()
    | [ (x, y) ] ->
      quote_string_to_buf x buf;
      Buffer.add_char buf ':';
      to_buf y buf
    | (x, y) :: xs ->
      quote_string_to_buf x buf;
      Buffer.add_char buf ':';
      to_buf y buf;
      Buffer.add_char buf ',';
      object_body_to_buf xs buf

  let to_string t =
    let buf = Buffer.create 0 in
    to_buf t buf;
    Buffer.contents buf
end

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
    let temp = Temp.create File ~prefix:"dune" ~suffix:"lsof" in
    let stdout =
      Unix.openfile
        (Path.to_absolute_filename temp)
        [ O_WRONLY; O_CREAT; O_TRUNC; O_SHARE_DELETE ]
        0o666
    in
    let prog = "/usr/sbin/lsof" in
    let argv = [ prog; "-w"; "-p"; string_of_int (Unix.getpid ()) ] in
    let pid = Spawn.spawn ~prog ~argv ~stdout () |> Pid.of_int in
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
    let common = Event.common_fields ~name:"gc" ~ts () in
    let args =
      let stat = Gc.quick_stat () in
      [ ("stack_size", `Int stat.stack_size)
      ; ("heap_words", `Int stat.heap_words)
      ; ("top_heap_words", `Int stat.top_heap_words)
      ; ("minor_words", `Float stat.minor_words)
      ; ("major_words", `Float stat.major_words)
      ; ("promoted_words", `Float stat.promoted_words)
      ; ("compactions", `Int stat.compactions)
      ; ("major_collections", `Int stat.major_collections)
      ; ("minor_collections", `Int stat.minor_collections)
      ]
    in
    let event = Event.counter common args in
    emit stats event
  in
  match Fd_count.get () with
  | Unknown -> ()
  | This fds ->
    let event =
      let args = [ ("value", `Int fds) ] in
      let common = Event.common_fields ~name:"fds" ~ts () in
      Event.counter common args
    in
    emit stats event
