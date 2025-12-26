open Stdune

type dst =
  | Out of out_channel
  | Custom of
      { write : string -> unit
      ; close : unit -> unit
      ; flush : unit -> unit
      }

type t =
  { print_sexp : Csexp.t -> unit
  ; close : unit -> unit
  ; flush : unit -> unit
  ; cats : Category.Set.t
  }

(* all fields of record used *)

let close { close; _ } = close ()

let create cats dst =
  let print_sexp =
    match dst with
    | Out out -> fun sexp -> Csexp.to_channel out sexp
    | Custom c -> fun sexp -> c.write (Csexp.to_string sexp)
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
  { print_sexp; close; flush; cats }
;;

let flush t = t.flush ()
let emit t event = t.print_sexp event

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
