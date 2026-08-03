open Stdune
open Fiber.O

exception Poll_cancelled

module Poller = struct
  module Id = Stdune.Id.Make ()

  type t =
    { id : Id.t
    ; cancel : Fiber.Cancel.t
    }

  let create () = { id = Id.gen (); cancel = Fiber.Cancel.create () }
  let repr = Repr.view (Repr.abstract Id.to_dyn) ~to_:(fun { id; cancel = _ } -> id)
  let to_dyn = Repr.to_dyn repr
  let compare x y = Id.compare x.id y.id
  let cancel t = t.cancel
  let cancelled t = Fiber.Cancel.fired t.cancel
  let fire_cancel t = Fiber.Cancel.fire t.cancel
end

(* A source of state observed by long polling. [Svar] uses [Fiber.Svar.wait] to block until
   the value changes; [Computed] reads the value from a thunk and polls it periodically (for
   state that lives in a plain [ref] and cannot wake waiters itself). *)
module Source = struct
  type 'a t =
    | Svar of 'a Fiber.Svar.t
    | Computed of
        { get : unit -> 'a
        ; poll_every : Time.Span.t
        }

  let read = function
    | Svar svar -> Fiber.Svar.read svar
    | Computed { get; poll_every = _ } -> get ()
  ;;

  let wait t ~until =
    match t with
    | Svar svar -> Fiber.Svar.wait svar ~until
    | Computed { get; poll_every } ->
      let rec loop () =
        if until (get ())
        then Fiber.return ()
        else
          let* () = Dune_scheduler.Scheduler.sleep poll_every in
          loop ()
      in
      loop ()
  ;;

  let wake = function
    | Svar svar -> Fiber.Svar.write svar (Fiber.Svar.read svar)
    | Computed _ -> Fiber.return ()
  ;;
end

module Poll_comparable = Comparable.Make (Poller)
module Map = Poll_comparable.Map

type 'a initial =
  { source : 'a Source.t
  ; on_cancel : 'a -> unit Fiber.t
  }

type 'a active =
  { value : 'a
  ; initial : 'a initial
  }

module Status = struct
  type 'a t =
    | Initialized of 'a initial
    | Active of 'a active
    | Cancelled
end

(* The set of pollers belonging to a single long-poll implementation, tracking each
   poller's lifecycle. Cancelled pollers are kept around so that an in-flight poll can
   observe the cancellation; they are reclaimed by [find_and_remove_cancelled]. *)
module Active_set = struct
  type 'a t = 'a Status.t Map.t ref

  let create () = ref Map.empty

  let initialize (t : _ t) poller initial =
    match Map.find !t poller with
    | None | Some Cancelled -> t := Map.set !t poller (Status.Initialized initial)
    | Some (Active _ | Initialized _) -> Code_error.raise "poller already initialized" []
  ;;

  let cancel (t : _ t) poller =
    match Map.find !t poller with
    | None | Some Cancelled -> Fiber.return ()
    | Some (Initialized a) ->
      t := Map.set !t poller Status.Cancelled;
      a.on_cancel (Source.read a.source)
    | Some (Active a) ->
      t := Map.set !t poller Status.Cancelled;
      a.initial.on_cancel (Source.read a.initial.source)
  ;;

  let update (t : _ t) poller value =
    match Map.find !t poller with
    | Some (Active active) ->
      t := Map.set !t poller (Status.Active { active with value });
      `Updated
    | Some (Initialized initial) ->
      t := Map.set !t poller (Status.Active { initial; value });
      `Updated
    | Some Cancelled -> `Poller_was_cancelled
    | None -> Code_error.raise "Active_set.update on a non-existent poller" []
  ;;

  let find_and_remove_cancelled (t : _ t) poller =
    let res = Map.find !t poller in
    (match res with
     | Some Cancelled -> t := Map.remove !t poller
     | None | Some (Active _) | Some (Initialized _) -> ());
    res
  ;;
end

let make_on_poll active_set source ~equal ~diff _session poller =
  let cancel = Poller.cancel poller in
  let wait_for_change last =
    let wait () =
      match last with
      | None -> Fiber.return ()
      | Some last ->
        let until x = Fiber.Cancel.fired cancel || not (equal x last) in
        Source.wait source ~until
    in
    let+ (), _ =
      Fiber.Cancel.with_handler cancel wait ~on_cancel:(fun () -> Source.wake source)
    in
    ()
  in
  let send last =
    let* () = wait_for_change last in
    if Poller.cancelled poller
    then raise Poll_cancelled
    else (
      let now = Source.read source in
      match Active_set.update active_set poller now with
      | `Poller_was_cancelled -> raise Poll_cancelled
      | `Updated ->
        let to_send = diff ~last ~now in
        Fiber.return (Some to_send))
  in
  match Active_set.find_and_remove_cancelled active_set poller with
  | None ->
    let initial = { source; on_cancel = (fun _ -> Fiber.return ()) } in
    Active_set.initialize active_set poller initial;
    send None
  | Some (Initialized _) -> send None
  | Some (Active a) -> send (Some a.value)
  | Some Cancelled -> raise Poll_cancelled
;;
