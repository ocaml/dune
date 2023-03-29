open Stdune

module Map_queue = struct
  (** A queue storing a maximum number of elements which are automatically
      dropped as new elements are added. It provides random access via
      monotonically incrementing integer indices. *)
  type 'a t =
    { map : 'a Int.Map.t
    ; max_size : int
    }

  let create ~max_size = { map = Int.Map.empty; max_size }

  let next_index { map; _ } =
    match Int.Map.max_binding map with
    | None -> 0
    | Some (max, _) -> max + 1

  let min_stored_index { map; _ } = Option.map ~f:fst (Int.Map.min_binding map)

  let insert t x =
    let x_index = next_index t in
    let max_index_to_remove = x_index - t.max_size in
    let map_below_max_index =
      Int.Map.filteri t.map ~f:(fun index _ -> index > max_index_to_remove)
    in
    let map_with_x = Int.Map.add_exn map_below_max_index x_index x in
    ({ t with map = map_with_x }, x_index)

  let get t ~index =
    match Int.Map.find t.map index with
    | Some x -> `Ok x
    | None ->
      if index >= next_index t then `Index_too_high
      else (
        assert (Option.value_exn (min_stored_index t) > index);
        `Index_too_low)
end

module Event_callbacks = struct
  (** A queue of events and a collection of callback functions to call on events
      which haven't yet occurred *)
  type 'a t =
    { events : 'a Map_queue.t
    ; callbacks : ('a -> unit Fiber.t) list Int.Map.t
    }

  let create ~max_stored_events =
    { events = Map_queue.create ~max_size:max_stored_events
    ; callbacks = Int.Map.empty
    }

  let next_seqno t = Map_queue.next_index t.events

  let add_event t ~event =
    let open Fiber.O in
    let events, seqno = Map_queue.insert t.events event in
    let+ () =
      match Int.Map.find t.callbacks seqno with
      | None -> Fiber.return ()
      | Some callbacks ->
        List.map callbacks ~f:(fun f -> f event) |> Fiber.all_concurrently_unit
    in
    let callbacks = Int.Map.remove t.callbacks seqno in
    { events; callbacks }

  let add_callback t ~f ~seqno =
    let callbacks = Int.Map.add_multi t.callbacks seqno f in
    { t with callbacks }

  let get_event t ~seqno =
    match Map_queue.get t.events ~index:seqno with
    | `Ok event -> (t, `Event_stored event)
    | `Index_too_low -> (t, `Event_no_longer_stored)
    | `Index_too_high ->
      let ivar = Fiber.Ivar.create () in
      let t = add_callback t ~seqno ~f:(Fiber.Ivar.fill ivar) in
      (t, `Event_not_yet_received (Fiber.Ivar.read ivar))

  let event_fiber t ~seqno =
    let t, event = get_event t ~seqno in
    match event with
    | `Event_no_longer_stored -> (t, Error `Event_no_longer_stored)
    | `Event_stored event -> (t, Ok (Fiber.return event))
    | `Event_not_yet_received fiber -> (t, Ok fiber)
end

module Mutable_event_callbacks = struct
  (** A convenience wrapper of [Event_callbacks.t] that mutates a ref *)
  type 'a t = 'a Event_callbacks.t ref

  let create ~max_stored_events =
    ref (Event_callbacks.create ~max_stored_events)

  let next_seqno t = Event_callbacks.next_seqno !t

  let add_event t ~event =
    let open Fiber.O in
    let+ t_new = Event_callbacks.add_event !t ~event in
    t := t_new

  let event_fiber t ~seqno =
    let t_new, event = Event_callbacks.event_fiber !t ~seqno in
    t := t_new;
    event
end

include Mutable_event_callbacks
