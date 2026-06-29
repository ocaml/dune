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

module Status = struct
  type 'a t =
    | Idle of 'a
    | Waiting
end

let make_on_poll map source ~equal ~diff _session poller =
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
    map := Map.set !map poller Status.Waiting;
    let* () = wait_for_change last in
    if Poller.cancelled poller
    then (
      map := Map.remove !map poller;
      raise Poll_cancelled)
    else (
      match Map.find !map poller with
      | Some Waiting ->
        let now = Source.read source in
        map := Map.set !map poller (Status.Idle now);
        let to_send = diff ~last ~now in
        Fiber.return (Some to_send)
      | None | Some (Idle _) ->
        Code_error.raise "poller changed state while a poll was in flight" [])
  in
  match Map.find !map poller with
  | None -> send None
  | Some (Idle last) -> send (Some last)
  | Some Waiting -> Code_error.raise "poller already has an in-flight request" []
;;
