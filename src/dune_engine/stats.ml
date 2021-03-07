open Stdune
module Json = Chrome_trace.Json
module Event = Chrome_trace.Event
module Timestamp = Event.Timestamp

let id = ref 0

let trace = ref None

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
