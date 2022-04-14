open Core
open Core.O

type 'a outcome =
  | Cancelled of 'a
  | Not_cancelled

type handlers =
  | End_of_handlers
  | Handler of
      { ivar : unit outcome Ivar.t
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
    let* () = Ivar.fill ivar (Cancelled ()) in
    invoke_handlers next
  | End_of_handlers -> return ()

let fire t =
  of_thunk (fun () ->
      match t.state with
      | Cancelled -> return ()
      | Not_cancelled { handlers } ->
        t.state <- Cancelled;
        invoke_handlers handlers)

let rec fills_of_handlers acc = function
  | Handler { ivar; next; prev = _ } ->
    fills_of_handlers (Scheduler.Fill (ivar, Cancelled ()) :: acc) next
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

let with_handler t f ~on_cancel =
  match t.state with
  | Cancelled ->
    let+ x, y = fork_and_join f on_cancel in
    (x, Cancelled y)
  | Not_cancelled h ->
    let ivar = Ivar.create () in
    let node = Handler { ivar; next = h.handlers; prev = End_of_handlers } in
    (match h.handlers with
    | End_of_handlers -> ()
    | Handler first -> first.prev <- node);
    h.handlers <- node;
    fork_and_join
      (fun () ->
        let* y = f () in
        match t.state with
        | Cancelled -> return y
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
            let+ () = Ivar.fill ivar Not_cancelled in
            y))
      (fun () ->
        Ivar.read ivar >>= function
        | Cancelled () ->
          let+ x = on_cancel () in
          Cancelled x
        | Not_cancelled -> return Not_cancelled)
