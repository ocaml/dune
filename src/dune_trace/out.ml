open Stdune

type t =
  { out : out_channel
  ; cats : Category.Set.t
  }

(* all fields of record used *)

let close t = close_out t.out
let create cats out = { out; cats = Category.Set.of_list cats }
let flush t = flush t.out
let emit t event = Csexp.to_channel t.out event

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
