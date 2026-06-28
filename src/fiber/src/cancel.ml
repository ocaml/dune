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

type state =
  | Cancelled
  | Not_cancelled of
      { mutable handlers : handlers
      ; mutable children : t list
      ; parent : t option
      }

and t = { mutable state : state }

let create () =
  { state = Not_cancelled { handlers = End_of_handlers; children = []; parent = None } }
;;

let make_child parent =
  match parent.state with
  | Cancelled -> { state = Cancelled }
  | Not_cancelled p ->
    let child =
      { state =
          Not_cancelled
            { handlers = End_of_handlers; children = []; parent = Some parent }
      }
    in
    p.children <- child :: p.children;
    child
;;

let remove_me_from_parent t ~parent =
  match parent with
  | None -> ()
  | Some { state = Cancelled; _ } -> ()
  | Some { state = Not_cancelled p; _ } ->
    p.children <- List.filter (fun c -> not (c == t)) p.children
;;

let rec invoke_handlers = function
  | Handler { ivar; next; prev = _ } ->
    let* () = Ivar.fill ivar (Cancelled ()) in
    invoke_handlers next
  | End_of_handlers -> return ()
;;

let fire =
  let rec gather acc t =
    match t.state with
    | Cancelled -> acc
    | Not_cancelled { handlers; children; parent = _ } ->
      t.state <- Cancelled;
      List.fold_left gather (handlers :: acc) children
  in
  fun t ->
    of_thunk (fun () ->
      (match t.state with
       | Cancelled -> ()
       | Not_cancelled { parent; _ } -> remove_me_from_parent t ~parent);
      gather [] t |> List.rev |> parallel_iter ~f:invoke_handlers)
;;

let rec fills_of_handlers acc = function
  | Handler { ivar; next; prev = _ } ->
    fills_of_handlers (Scheduler.Fill (ivar, Cancelled ()) :: acc) next
  | End_of_handlers -> List.rev acc
;;

let rec fire' t =
  match t.state with
  | Cancelled -> []
  | Not_cancelled { handlers; children; parent } ->
    t.state <- Cancelled;
    remove_me_from_parent t ~parent;
    fills_of_handlers [] handlers @ List.concat_map fire' children
;;

let fired t =
  match t.state with
  | Cancelled -> true
  | Not_cancelled _ -> false
;;

let with_handler t f ~on_cancel =
  match t.state with
  | Cancelled ->
    let+ x, y = fork_and_join f on_cancel in
    x, (Cancelled y : _ outcome)
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
         | Not_cancelled h ->
           (match node with
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
         Ivar.read ivar
         >>= function
         | Cancelled () ->
           let+ x = on_cancel () in
           (Cancelled x : _ outcome)
         | Not_cancelled -> return (Not_cancelled : _ outcome))
;;
