open Stdune
module Json = Chrome_trace.Json
module Event = Chrome_trace.Event
module Timestamp = Event.Timestamp

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

let evaluated_rules = ref 0

let id = ref 0

let new_evaluated_rule () = incr evaluated_rules

let () = Hooks.End_of_build.always (fun () -> evaluated_rules := 0)

let trace = ref None

let record () =
  Option.iter !trace ~f:(fun reporter ->
      let ts = Event.Timestamp.now () in
      let pid = 0 in
      let tid = 0 in
      let () =
        let common = Event.common ~name:"gc" ~ts ~pid ~tid () in
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
        Chrome_trace.emit reporter event
      in
      let () =
        let event =
          let args = [ ("value", Json.Int !evaluated_rules) ] in
          let common = Event.common ~name:"evaluated_rules" ~ts ~pid ~tid () in
          Event.counter common args
        in
        Chrome_trace.emit reporter event
      in
      match Fd_count.get () with
      | Unknown -> ()
      | This fds ->
        let event =
          let args = [ ("value", Json.Int fds) ] in
          let common = Event.common ~name:"fds" ~ts ~pid ~tid () in
          Event.counter common args
        in
        Chrome_trace.emit reporter event)

let enable path =
  let reporter = Chrome_trace.make path in
  trace := Some reporter;
  at_exit (fun () -> Chrome_trace.close reporter)

let with_process ~program ~args fiber =
  match !trace with
  | None -> fiber
  | Some reporter ->
    let open Fiber.O in
    incr id;
    let id = Event.Id.Int !id in
    let common =
      let name = Filename.basename program in
      let ts = Timestamp.now () in
      Event.common ~cat:[ "process" ] ~name ~pid:0 ~tid:0 ~ts ()
    in
    let () =
      let args =
        [ ( "process_args"
          , Json.Array (List.map args ~f:(fun arg -> Json.String arg)) )
        ]
      in
      let event = Event.async id ~args Start common in
      Chrome_trace.emit reporter event
    in
    let+ result = fiber in
    let common = Event.set_ts common (Timestamp.now ()) in
    let () =
      let event = Event.async id End common in
      Chrome_trace.emit reporter event
    in
    result
