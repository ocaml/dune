(** Stores an association between monotonically increasing integer sequence
    numbers and events. There is a limit to the number of events that can be
    stored, and once this limit is reached, adding a new event will cause the
    oldest event to be removed. *)
type 'a t

(** Create a new event queue *)
val create : max_stored_events:int -> 'a t

(** Returns the sequence number that will be assigned to the next event added to
    the queue *)
val next_seqno : 'a t -> int

(** Add an event to the queue. This function returns a fiber which becomes
    determined immediately. Binding/mapping on this fiber is necessary for the
    fibers returned by [event_fiber] to become determined (its behaviour is
    similar to that of [Fiber.Ivar.fill]. *)
val add_event : 'a t -> event:'a -> unit Fiber.t

(** Look up an event by its sequence number. If the event was once stored in the
    queue but has been dropped due to new events arriving after the maximum size
    is reached, this function returns [Error `Event_no_longer_stored]. Otherwise
    it returns a fiber which becomes determined to the event with the requested
    sequence number. If the event has not been added yet (ie.
    [seqno >= next_seqno t]) then the returned fiber will become determined when
    the requested event is added. *)
val event_fiber :
  'a t -> seqno:int -> ('a Fiber.t, [ `Event_no_longer_stored ]) result
