open Stdune
module Timestamp = Chrome_trace.Event.Timestamp

type dst =
  | Out of out_channel
  | Custom of
      { write : string -> unit
      ; close : unit -> unit
      ; flush : unit -> unit
      }

type t =
  { print : string -> unit
  ; close : unit -> unit
  ; flush : unit -> unit
  ; mutable after_first_event : bool
  ; cats : Category.Set.t
  }

(* all fields of record used *)

let close { print; close; _ } =
  print "]\n";
  close ()
;;

let create cats dst =
  let print =
    match dst with
    | Out out -> fun str -> Stdlib.output_string out str
    | Custom c -> c.write
  in
  let close =
    match dst with
    | Out out -> fun () -> Stdlib.close_out out
    | Custom c -> c.close
  in
  let flush =
    match dst with
    | Out out -> fun () -> flush out
    | Custom c -> c.flush
  in
  let cats = Category.Set.of_list cats in
  { print; close; after_first_event = false; flush; cats }
;;

let flush t = t.flush ()

let next_leading_char t =
  match t.after_first_event with
  | true -> ','
  | false ->
    t.after_first_event <- true;
    '['
;;

let printf t format_string =
  let c = next_leading_char t in
  Printf.ksprintf t.print ("%c" ^^ format_string ^^ "\n") c
;;

let emit t event = printf t "%s" (Json.to_string (Chrome_trace.Event.to_json event))

let start t k : Event.Async.t option =
  match t with
  | None -> None
  | Some _ ->
    let event_data = k () in
    let start = Time.now () in
    Some (Event.Async.create ~event_data ~start)
;;

let finish t event =
  match event with
  | None -> ()
  | Some { Event.Async.start; event_data = { args; cat; name } } ->
    let dur =
      let stop = Time.now () in
      Time.diff stop start |> Time.Span.to_secs |> Timestamp.of_float_seconds
    in
    let common =
      Chrome_trace.Event.common_fields
        ~cat:[ Category.to_string cat ]
        ~name
        ~ts:(Timestamp.of_float_seconds (Time.to_secs start))
        ()
    in
    let event = Chrome_trace.Event.complete ?args common ~dur in
    emit t event
;;
