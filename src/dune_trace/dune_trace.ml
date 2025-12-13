open Stdune
module Category = Category
module Timestamp = Chrome_trace.Event.Timestamp
module Event = Event

module Out = struct
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
      let start = Unix.gettimeofday () in
      Some (Event.Async.create ~event_data ~start)
  ;;

  let finish t event =
    match event with
    | None -> ()
    | Some { Event.Async.start; event_data = { args; cat; name } } ->
      let dur =
        let stop = Unix.gettimeofday () in
        Timestamp.of_float_seconds (stop -. start)
      in
      let common =
        Chrome_trace.Event.common_fields
          ?cat
          ~name
          ~ts:(Timestamp.of_float_seconds start)
          ()
      in
      let event = Chrome_trace.Event.complete ?args common ~dur in
      emit t event
  ;;
end

let global = ref None

let () =
  at_exit (fun () ->
    match !global with
    | None -> ()
    | Some t -> Out.close t)
;;

let set_global t =
  if Option.is_some !global then Code_error.raise "global stats have been set" [];
  global := Some t
;;

let global () = !global

let always_emit event =
  match global () with
  | None -> ()
  | Some out -> Out.emit out event
;;

let emit cat f =
  match global () with
  | None -> ()
  | Some out -> if Category.Set.mem out.cats cat then Out.emit out (f ())
;;

let emit_all cat f =
  match global () with
  | None -> ()
  | Some out -> if Category.Set.mem out.cats cat then List.iter (f ()) ~f:(Out.emit out)
;;

let flush () =
  match global () with
  | None -> ()
  | Some s -> Out.flush s
;;

module Private = struct
  module Fd_count = Fd_count
end
