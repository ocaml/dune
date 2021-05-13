open! Stdune
open Fiber.O
open Dune_rpc_server
open Dune_rpc_private
module Dep_conf = Dune_rules.Dep_conf
module Build_system = Dune_engine.Build_system

module Status = struct
  type t =
    | Accepted
    | Rejected

  let sexp = Conv.enum [ ("Accepted", Accepted); ("Rejected", Rejected) ]
end

type pending_build_action = Build of Dep_conf.t list * Status.t Fiber.Ivar.t

let diagnostic_of_error : Build_system.Error.t -> Dune_rpc_private.Diagnostic.t
    =
 fun m ->
  let message, dir = Build_system.Error.info m in
  let loc = message.loc in
  let message = Pp.map_tags (Pp.concat message.paragraphs) ~f:(fun _ -> ()) in
  { severity = None
  ; targets = []
  ; message
  ; loc
  ; promotion = []
  ; directory = Option.map ~f:Path.to_string dir
  }

(* TODO un-copy-paste from dune/bin/arg.ml *)
let dep_parser =
  let open Dune_engine in
  Dune_lang.Syntax.set Stanza.syntax (Active Stanza.latest_version)
    Dep_conf.decode

module Decl = struct
  module Decl = Decl

  let build = Decl.request ~method_:"build" Conv.(list string) Status.sexp

  let shutdown = Decl.notification ~method_:"shutdown" Conv.unit

  module Status = struct
    type t = { clients : Id.t list }

    let sexp =
      let open Conv in
      let to_ clients = { clients } in
      let from { clients } = clients in
      iso (list Id.sexp) to_ from
  end

  let status = Decl.request ~method_:"status" Conv.unit Status.sexp
end

module Client = struct
  type t = { mutable next_id : int }

  let create () = { next_id = 0 }

  let to_dyn { next_id = _ } =
    let open Dyn.Encoder in
    opaque ()
end

module Session_comparable = Comparable.Make (struct
  type t = Client.t Session.t

  let compare = Session.compare

  let to_dyn s = Session.to_dyn Client.to_dyn s
end)

module Session_set = Session_comparable.Set

module Subscribers = struct
  type t =
    { build_progress : Session_set.t
    ; diagnostics : Session_set.t
    }

  let empty =
    { build_progress = Session_set.empty; diagnostics = Session_set.empty }

  let get t (which : Subscribe.t) =
    match which with
    | Build_progress -> t.build_progress
    | Diagnostics -> t.diagnostics

  let modify t (which : Subscribe.t) ~f =
    match which with
    | Build_progress -> { t with build_progress = f t.build_progress }
    | Diagnostics -> { t with diagnostics = f t.diagnostics }

  let add t which session =
    modify t which ~f:(fun set -> Session_set.add set session)

  let remove t which session =
    modify t which ~f:(fun set -> Session_set.remove set session)

  let notify t which ~f =
    let set = get t which in
    Fiber.parallel_iter_set (module Session_set) set ~f
end

type t =
  { config : Run.Config.t
  ; pending_build_jobs : (Dep_conf.t list * Status.t Fiber.Ivar.t) Queue.t
  ; build_handler : Build_system.Handler.t
  ; pool : Fiber.Pool.t
  ; mutable subscribers : Subscribers.t
  ; mutable clients : Session_set.t
  }

let build_handler t = t.build_handler

let handler (t : t Fdecl.t) : 'a Dune_rpc_server.Handler.t =
  let on_init session (_ : Initialize.Request.t) =
    let t = Fdecl.get t in
    let client = Client.create () in
    t.clients <- Session_set.add t.clients session;
    Fiber.return client
  in
  let on_terminate session =
    let t = Fdecl.get t in
    t.subscribers <-
      [ Subscribe.Diagnostics; Build_progress ]
      |> List.fold_left ~init:t.subscribers ~f:(fun acc which ->
             Subscribers.remove acc which session);
    Fiber.return ()
  in
  let rpc =
    Handler.create ~on_terminate ~on_init
      ~version:Dune_rpc_private.Version.latest ()
  in
  let () =
    Handler.request rpc
      (Handler.callback (Handler.public ~since:(1, 0) ()) Fiber.return)
      Public.Request.ping
  in
  let () =
    let build targets =
      let ivar = Fiber.Ivar.create () in
      let targets =
        List.map targets ~f:(fun s ->
            Dune_lang.Decoder.parse dep_parser Univ_map.empty
              (Dune_lang.Parser.parse_string ~fname:"dune rpc"
                 ~mode:Dune_lang.Parser.Mode.Single s))
      in
      Queue.push (Fdecl.get t).pending_build_jobs (targets, ivar);
      Fiber.Ivar.read ivar
    in
    Handler.request rpc (Handler.callback Handler.private_ build) Decl.build
  in
  let () =
    let rec cancel_pending_jobs () =
      match Queue.pop (Fdecl.get t).pending_build_jobs with
      | None -> Fiber.return ()
      | Some (_, job) ->
        let* () = Fiber.Ivar.fill job Status.Rejected in
        cancel_pending_jobs ()
    in
    let shutdown () =
      let t = Fdecl.get t in
      let terminate_sessions () =
        Fiber.fork_and_join_unit cancel_pending_jobs (fun () ->
            Fiber.parallel_iter_set
              (module Session_set)
              t.clients ~f:Session.request_close)
      in
      let shutdown () =
        Fiber.fork_and_join_unit Dune_engine.Scheduler.shutdown Run.stop
      in
      Fiber.fork_and_join_unit terminate_sessions shutdown
    in
    Handler.notification rpc
      (Handler.callback Handler.private_ shutdown)
      Decl.shutdown
  in
  let () =
    let unsubscribe session (sub : Subscribe.t) =
      let t = Fdecl.get t in
      t.subscribers <- Subscribers.remove t.subscribers sub session;
      Fiber.return ()
    in
    let cb = Handler.callback' (Handler.public ~since:(1, 0) ()) unsubscribe in
    Handler.notification rpc cb Public.Notification.unsubscribe
  in
  let () =
    let subscribe session (sub : Subscribe.t) =
      let t = Fdecl.get t in
      t.subscribers <- Subscribers.add t.subscribers sub session;
      let* running = Fiber.Pool.running t.pool in
      match running with
      | false -> Fiber.return ()
      | true ->
        let errors = Build_system.errors () in
        Fiber.Pool.task t.pool ~f:(fun () ->
            let events =
              List.map errors ~f:(fun (e : Build_system.Error.t) ->
                  Diagnostic.Event.Add (diagnostic_of_error e))
            in
            Session.notification session Server_notifications.diagnostic events)
    in
    let cb = Handler.callback' (Handler.public ~since:(1, 0) ()) subscribe in
    Handler.notification rpc cb Public.Notification.subscribe
  in
  let () =
    let f () =
      let t = Fdecl.get t in
      let clients =
        Session_set.to_list_map t.clients ~f:(fun session ->
            Session.initialize session |> Initialize.Request.id)
      in
      Fiber.return { Decl.Status.clients }
    in
    let cb = Handler.callback Handler.private_ f in
    Handler.request rpc cb Decl.status
  in
  let () =
    let cb =
      let f () =
        Build_system.errors ()
        |> List.map ~f:diagnostic_of_error
        |> Fiber.return
      in
      Handler.callback (Handler.public ~since:(1, 0) ()) f
    in
    Handler.request rpc cb Public.Request.diagnostics
  in
  rpc

let task t f =
  let* running = Fiber.Pool.running t.pool in
  if running then
    Fiber.Pool.task t.pool ~f
  else
    Fiber.return ()

let error t errors =
  let t = Fdecl.get t in
  task t (fun () ->
      let events =
        lazy
          (List.map errors ~f:(fun (e : Build_system.Handler.error) ->
               match e with
               | Add x -> Diagnostic.Event.Add (diagnostic_of_error x)
               | Remove x -> Remove (diagnostic_of_error x)))
      in
      Subscribers.notify t.subscribers Diagnostics ~f:(fun session ->
          Session.notification session Server_notifications.diagnostic
            (Lazy.force events)))

let build_progress t ~complete ~remaining =
  let t = Fdecl.get t in
  let notification = { Progress.complete; remaining } in
  task t (fun () ->
      Subscribers.notify t.subscribers Build_progress ~f:(fun session ->
          Session.notification session Server_notifications.progress
            notification))

let build_event _ _ = Fiber.return ()

let create () =
  let t = Fdecl.create Dyn.Encoder.opaque in
  let pending_build_jobs = Queue.create () in
  let handler = Dune_rpc_server.make (handler t) in
  let pool = Fiber.Pool.create () in
  let config = Run.Config.Server { handler; backlog = 10; pool } in
  let build_handler =
    Build_system.Handler.create ~error:(error t)
      ~build_progress:(build_progress t) ~build_event:(build_event t)
  in
  let res =
    { config
    ; pending_build_jobs
    ; subscribers = Subscribers.empty
    ; clients = Session_set.empty
    ; build_handler
    ; pool
    }
  in
  Fdecl.set t res;
  res

let config t = t.config

let pending_build_action t =
  Queue.pop t.pending_build_jobs
  |> Option.map ~f:(fun (targets, ivar) -> Build (targets, ivar))
