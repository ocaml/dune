open! Stdune
open Dune_rpc_server
open Import
module Poll_comparable = Comparable.Make (Poller)
module Poll_map = Poll_comparable.Map

type ('a, 's) waiter =
  | No_active_request of 's
  | Waiting of 'a option Fiber.Ivar.t

module Instance = struct
  module type S = sig
    module Spec : Poll_spec.S

    val waiters : (Spec.response, Spec.state) waiter Poll_map.t ref
  end

  type ('r, 'u) t =
    (module S with type Spec.response = 'r and type Spec.update = 'u)

  let finish_waiter = function
    | No_active_request _ -> Fiber.return ()
    | Waiting a -> Fiber.Ivar.fill a None

  let client_cancel (type r u) (t : (r, u) t) poller =
    let module T = (val t) in
    match Poll_map.find !T.waiters poller with
    | None -> Fiber.return ()
    | Some w ->
      T.waiters := Poll_map.remove !T.waiters poller;
      finish_waiter w

  let poll (type r u) (t : (r, u) t) poller =
    let module T = (val t) in
    match Poll_map.find !T.waiters poller with
    | Some (Waiting _) ->
      (* TODO this means that the user polled again before waiting for the last
         poll to complete. we should signal an error here. *)
      assert false
    | None ->
      T.waiters :=
        Poll_map.add_exn !T.waiters poller
          (No_active_request (T.Spec.no_change ()));
      Fiber.return (Some (T.Spec.current ()))
    | Some (No_active_request s) -> (
      match T.Spec.on_rest_request poller s with
      | `Respond r ->
        T.waiters :=
          Poll_map.set !T.waiters poller
            (No_active_request (T.Spec.no_change ()));
        Fiber.return (Some r)
      | `Delay ->
        let ivar = Fiber.Ivar.create () in
        T.waiters := Poll_map.set !T.waiters poller (Waiting ivar);
        Fiber.Ivar.read ivar)

  let update (type u r) (t : (r, u) t) (u : u) =
    let module T = (val t) in
    let to_notify = ref [] in
    T.waiters :=
      Poll_map.filter_map !T.waiters ~f:(fun a ->
          match a with
          | No_active_request s ->
            T.Spec.on_update_inactive u s
            |> Option.map ~f:(fun s -> No_active_request s)
          | Waiting ivar ->
            to_notify := ivar :: !to_notify;
            Some (No_active_request (T.Spec.no_change ())));
    match !to_notify with
    | [] -> Fiber.return ()
    | to_notify ->
      let r = T.Spec.on_update_waiting u in
      Fiber.parallel_iter to_notify ~f:(fun ivar ->
          Fiber.Ivar.fill ivar (Some r))

  let disconnect_session (type u r) (t : (r, u) t) session =
    let module T = (val t) in
    let cancel =
      Poll_map.filteri !T.waiters ~f:(fun poller _ ->
          Session.has_poller session poller)
    in
    T.waiters :=
      if Poll_map.is_empty cancel then
        !T.waiters
      else
        Poll_map.merge !T.waiters cancel ~f:(fun _ x y ->
            match (x, y) with
            | Some _, Some _ -> None
            | Some _, _ -> y
            | None, Some _
            | None, None ->
              assert false);
    Poll_map.values cancel |> Fiber.parallel_iter ~f:finish_waiter
end

module Build_system = Dune_engine.Build_system
module Build_config = Dune_engine.Build_config

module Progress = struct
  module Progress = Dune_rpc.Progress

  type state =
    | Next_update of Progress.t
    | No_change

  type response = Progress.t

  type update = Progress.t

  let current () : Progress.t =
    match Build_system.last_event () with
    | Some ((Finish | Fail | Interrupt) as evt) -> progress_of_build_event evt
    | Some Start
    | None -> (
      let p = Build_system.get_current_progress () in
      match Build_system.Progress.is_determined p with
      | false -> Waiting
      | true ->
        let complete = Build_system.Progress.complete p in
        let remaining = Build_system.Progress.remaining p in
        In_progress { complete; remaining })

  let no_change () = No_change

  let on_rest_request _poller = function
    | No_change -> `Delay
    | Next_update last_event -> `Respond last_event

  let on_update_inactive evt _ = Some (Next_update evt)

  let on_update_waiting u = u
end

module Diagnostic = struct
  module Diagnostic = Dune_rpc.Diagnostic

  module Pending_diagnostics : sig
    type t

    val empty : t

    val add_error : t -> Build_config.Handler.error -> t

    val is_empty : t -> bool

    val to_diagnostic_list : t -> Diagnostic.Event.t list
  end = struct
    module Error = Build_config.Error
    module Map = Build_config.Error.Id.Map
    module Handler = Build_config.Handler

    type t = Handler.error Map.t

    let empty = Map.empty

    let is_empty t = Map.is_empty t

    let add_error t (m : Handler.error) =
      let id =
        match m with
        | Add e -> Error.id e
        | Remove e -> Error.id e
      in
      Map.set t id m

    let to_diagnostic_list t =
      Map.to_list_map t ~f:(fun _ e ->
          match (e : Handler.error) with
          | Remove x ->
            Diagnostic.Event.Remove (Diagnostics.diagnostic_of_error x)
          | Add x -> Add (Diagnostics.diagnostic_of_error x))
  end

  type update = Build_config.Handler.error list

  type response = Diagnostic.Event.t list

  type state = Pending_diagnostics.t

  let current () =
    Build_system.errors ()
    |> List.map ~f:(fun e ->
           Diagnostic.Event.Add (Diagnostics.diagnostic_of_error e))

  let no_change () = Pending_diagnostics.empty

  let on_rest_request _poller pd =
    if Pending_diagnostics.is_empty pd then
      `Delay
    else
      `Respond (Pending_diagnostics.to_diagnostic_list pd)

  let on_update_inactive errors init =
    Some
      (List.fold_left errors ~init ~f:(fun acc a ->
           Pending_diagnostics.add_error acc a))

  let on_update_waiting =
    List.map ~f:Diagnostics.diagnostic_event_of_error_event
end

type t =
  { diagnostic : (Diagnostic.response, Diagnostic.update) Instance.t
  ; progress : (Progress.response, Progress.update) Instance.t
  }

let progress t = t.progress

let diagnostic t = t.diagnostic

let create () : t =
  let module D = struct
    module Spec = Diagnostic

    let waiters = ref Poll_map.empty
  end in
  let module P = struct
    module Spec = Progress

    let waiters = ref Poll_map.empty
  end in
  { diagnostic = (module D); progress = (module P) }

let disconnect_session { diagnostic; progress } session =
  Fiber.fork_and_join_unit
    (fun () -> Instance.disconnect_session diagnostic session)
    (fun () -> Instance.disconnect_session progress session)
