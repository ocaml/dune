open! Stdune
open Fiber.O

module Temp = Temp.Monad (struct
  type 'a t = 'a Fiber.t

  let protect ~f ~finally =
    Fiber.finalize f ~finally:(fun () -> finally () |> Fiber.return)
end)

module Cancellation = struct
  type 'a outcome =
    | Cancelled of 'a
    | Not_cancelled

  type handlers =
    | End_of_handlers
    | Handler of
        { ivar : unit outcome Fiber.Ivar.t
        ; mutable next : handlers
        ; mutable prev : handlers
        }

  module State = struct
    type t =
      | Cancelled
      | Not_cancelled of { mutable handlers : handlers }
  end

  type t = { mutable state : State.t }

  let create () = { state = Not_cancelled { handlers = End_of_handlers } }

  let rec invoke_handlers = function
    | Handler { ivar; next; prev = _ } ->
      let* () = Fiber.Ivar.fill ivar (Cancelled ()) in
      invoke_handlers next
    | End_of_handlers -> Fiber.return ()

  let fire t =
    Fiber.of_thunk (fun () ->
        match t.state with
        | Cancelled -> Fiber.return ()
        | Not_cancelled { handlers } ->
          t.state <- Cancelled;
          invoke_handlers handlers)

  let rec fills_of_handlers acc = function
    | Handler { ivar; next; prev = _ } ->
      fills_of_handlers (Fiber.Fill (ivar, Cancelled ()) :: acc) next
    | End_of_handlers -> List.rev acc

  let fire' t =
    match t.state with
    | Cancelled -> []
    | Not_cancelled { handlers } ->
      t.state <- Cancelled;
      fills_of_handlers [] handlers

  let fired t =
    match t.state with
    | Cancelled -> true
    | Not_cancelled _ -> false

  let with_handler t f ~on_cancellation =
    match t.state with
    | Cancelled ->
      let+ x, y = Fiber.fork_and_join f on_cancellation in
      (x, Cancelled y)
    | Not_cancelled h ->
      let ivar = Fiber.Ivar.create () in
      let node = Handler { ivar; next = h.handlers; prev = End_of_handlers } in
      (match h.handlers with
      | End_of_handlers -> ()
      | Handler first -> first.prev <- node);
      h.handlers <- node;
      Fiber.fork_and_join
        (fun () ->
          let* y = f () in
          match t.state with
          | Cancelled -> Fiber.return y
          | Not_cancelled h -> (
            match node with
            | End_of_handlers ->
              (* We could avoid this [assert false] with GADT sorcery given that
                 we created [node] just above and we know for sure it is the
                 [Handler _] case, but it's not worth the code complexity. *)
              assert false
            | Handler node ->
              (match node.prev with
              | End_of_handlers -> h.handlers <- node.next
              | Handler prev -> prev.next <- node.next);
              (match node.next with
              | End_of_handlers -> ()
              | Handler next -> next.prev <- node.prev);
              let+ () = Fiber.Ivar.fill ivar Not_cancelled in
              y))
        (fun () ->
          Fiber.Ivar.read ivar >>= function
          | Cancelled () ->
            let+ x = on_cancellation () in
            Cancelled x
          | Not_cancelled -> Fiber.return Not_cancelled)
end

module Observer_id = Id.Make ()

module State = struct
  type 'a state =
    | Active of 'a
    | Closed

  type 'a t = 'a state ref

  let create x = ref (Active x)
end

module Waiters = struct
  (* A set of all observers waiting for an ivar to be filled *)
  type t = unit Fiber.Ivar.t Observer_id.Map.t ref

  let create () = ref Observer_id.Map.empty

  let notify (t : t) =
    let to_notify = !t in
    t := Observer_id.Map.empty;
    Observer_id.Map.values to_notify
    |> Fiber.sequential_iter ~f:(fun ivar -> Fiber.Ivar.fill ivar ())
end

module Observable_types = struct
  type 'a subscription =
    { observers : 'a observer Observer_id.Map.t ref
    ; id : Observer_id.t
    ; waiters : Waiters.t
    }

  and 'a observer_desc =
    | Closed
    | Closed_undelivered of 'a
    (* When an observable closes, one can still observe the final value *)
    | Active of
        { sub : 'a subscription
        ; mutable state :
            [ `Idle (* the next await will go pending *)
            | `Pending_await of unit Fiber.Ivar.t
            | `Ready of 'a (* the next await is immediate *)
            ]
        }

  and 'a observer = 'a observer_desc ref
end

module Subscription = struct
  open Observable_types

  type 'a t = 'a subscription

  (* Represents the operations an observer needs from an observable. It's
     possible to do without this and make observable and observer mutually
     recursive, but it becomes quite ugly *)

  (* observer removes itself from the observable's data structure without
     knowing what's inside an observable *)
  let unsubscribe { observers; id; waiters } =
    observers := Observer_id.Map.remove !observers id;
    match Observer_id.Map.find !waiters id with
    | None -> Fiber.return ()
    | Some s -> Fiber.Ivar.fill s ()

  let register t ivar =
    t.waiters := Observer_id.Map.add_exn !(t.waiters) t.id ivar
end

module Observer = struct
  open Observable_types

  type 'a t = 'a observer

  let create (sub : 'a Subscription.t) (a : 'a) : 'a t =
    ref (Active { sub; state = `Ready a })

  let unsubscribe t =
    match !t with
    | Closed
    | Closed_undelivered _ ->
      Fiber.return (t := Closed)
    | Active { sub; state = _ } ->
      t := Closed;
      Subscription.unsubscribe sub

  let rec await t =
    match !t with
    | Closed -> Fiber.return None
    | Closed_undelivered a ->
      t := Closed;
      Fiber.return (Some a)
    | Active a -> (
      match a.state with
      | `Pending_await _ ->
        Code_error.raise "await cannot be called concurrently" []
      | `Ready v ->
        a.state <- `Idle;
        Fiber.return (Some v)
      | `Idle ->
        let ivar = Fiber.Ivar.create () in
        Subscription.register a.sub ivar;
        a.state <- `Pending_await ivar;
        let* () = Fiber.Ivar.read ivar in
        (* we need to retry from scratch because it's possible that another
           observer will get awaken before us and it might interact with the
           observer somehow to update our [t] *)
        await t)
end

module Observable = struct
  type 'a desc =
    { observers : 'a Observer.t Observer_id.Map.t ref
    ; mutable current : 'a
    ; combine : 'a -> 'a -> 'a
    ; waiters : Waiters.t
    }

  type 'a t = 'a desc State.t

  type 'a sink = 'a t

  let create ?(combine = fun _ x -> x) current =
    let t =
      State.create
        { current
        ; observers = ref Observer_id.Map.empty
        ; combine
        ; waiters = Waiters.create ()
        }
    in
    (t, t)

  let add_observer (type a) (t : a t) : a Observer.t =
    match !t with
    | Closed -> Code_error.raise "cannot add observer to closed observable" []
    | Active a ->
      let id = Observer_id.gen () in
      let sub : a Subscription.t =
        { id; observers = a.observers; waiters = a.waiters }
      in
      let obs = Observer.create sub a.current in
      a.observers := Observer_id.Map.add_exn !(a.observers) id obs;
      obs

  let close (obs : _ sink) =
    match !obs with
    | Closed -> Fiber.return ()
    | Active o ->
      obs := Closed;
      let to_close = !(o.observers) in
      o.observers := Observer_id.Map.empty;
      Observer_id.Map.iter to_close ~f:(fun obs ->
          match !obs with
          | Closed
          | Closed_undelivered _ ->
            assert false
          | Active active -> (
            match active.state with
            | `Pending_await _
            | `Idle ->
              obs := Closed
            | `Ready v -> obs := Closed_undelivered v));
      Waiters.notify o.waiters

  let update (type a) (sink : a sink) (a : a) =
    match !sink with
    | Closed -> Code_error.raise "unable to update when observable is closed" []
    | Active observable ->
      observable.current <- observable.combine observable.current a;
      Observer_id.Map.iter !(observable.observers)
        ~f:(fun (obs : a Observer.t) ->
          match !obs with
          | Closed -> assert false
          | Closed_undelivered _ -> assert false
          | Active active -> (
            match active.state with
            | `Pending_await _ -> active.state <- `Ready a
            | `Idle -> active.state <- `Ready a
            | `Ready a' -> active.state <- `Ready (observable.combine a' a)));
      Waiters.notify observable.waiters
end
