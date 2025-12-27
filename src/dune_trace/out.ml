open Stdune

type t =
  { out : out_channel
  ; mutable after_first_event : bool
  ; cats : Category.Set.t
  }

(* all fields of record used *)

let close t =
  output_string t.out "]\n";
  close_out t.out
;;

let create cats out = { out; after_first_event = false; cats = Category.Set.of_list cats }
let flush t = flush t.out

let next_leading_char t =
  match t.after_first_event with
  | true -> ','
  | false ->
    t.after_first_event <- true;
    '['
;;

let printf t format_string =
  let c = next_leading_char t in
  Printf.ksprintf (output_string t.out) ("%c" ^^ format_string ^^ "\n") c
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
      Time.diff stop start
    in
    let event = Event.Event.complete ?args ~start ~dur cat ~name in
    emit t event
;;
