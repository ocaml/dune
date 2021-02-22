open! Stdune
open Fiber.O
open Dune_rpc_server
open Dune_rpc_private
module Dep_conf = Dune_rules.Dep_conf

module Status = struct
  type t =
    | Accepted
    | Rejected

  let sexp = Conv.enum [ ("Accepted", Accepted); ("Rejected", Rejected) ]
end

type pending_build_action =
  | Shutdown
  | Build of Dep_conf.t list * Status.t Fiber.Ivar.t

(* TODO un-copy-paste from dune/bin/arg.ml *)
let dep_parser =
  let open Dune_engine in
  Dune_lang.Syntax.set Stanza.syntax (Active Stanza.latest_version)
    Dep_conf.decode

module Decl = struct
  module Decl = Decl

  let build = Decl.request ~method_:"build" Conv.(list string) Status.sexp

  let ping = Decl.request ~method_:"ping" Conv.unit Conv.unit

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

type t =
  { config : Dune_engine.Scheduler.Config.Rpc.t
  ; build_mutex : Fiber.Mutex.t
  ; pending_build_jobs : (Dep_conf.t list * Status.t Fiber.Ivar.t) Queue.t
  ; mutable promotion_subs : Session_set.t
  ; mutable error_subs : Session_set.t
  ; mutable pending_shutdown : bool
  ; mutable clients : Session_set.t
  }

let handler (t : t Fdecl.t) : 'a Dune_rpc_server.Handler.t =
  let on_init session (_ : Initialize.Request.t) =
    let t = Fdecl.get t in
    let client = Client.create () in
    t.clients <- Session_set.add t.clients session;
    Fiber.return client
  in
  let on_terminate session =
    let t = Fdecl.get t in
    t.clients <- Session_set.remove t.clients session;
    t.promotion_subs <- Session_set.remove t.promotion_subs session;
    t.error_subs <- Session_set.remove t.error_subs session;
    Fiber.return ()
  in
  let rpc = Handler.create ~on_terminate ~on_init ~version:(1, 0) () in
  let () =
    Handler.request rpc
      (Handler.callback (Handler.public ~since:(1, 0) ()) Fiber.return)
      Decl.ping
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
      (Fdecl.get t).pending_shutdown <- true;
      cancel_pending_jobs ()
    in
    Handler.notification rpc
      (Handler.callback Handler.private_ shutdown)
      Decl.shutdown
  in
  let () =
    let unsubscribe session (sub : Subscribe.t) =
      let t = Fdecl.get t in
      ( match sub with
      | Promotion ->
        t.promotion_subs <- Session_set.remove t.promotion_subs session
      | Error -> t.error_subs <- Session_set.remove t.error_subs session );
      Fiber.return ()
    in
    let cb = Handler.callback' (Handler.public ~since:(1, 0) ()) unsubscribe in
    Handler.notification rpc cb Public.Notification.unsubscribe
  in
  let () =
    let subscribe session (sub : Subscribe.t) =
      let t = Fdecl.get t in
      ( match sub with
      | Promotion ->
        t.promotion_subs <- Session_set.add t.promotion_subs session
      | Error -> t.error_subs <- Session_set.add t.error_subs session );
      Fiber.return ()
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
  rpc

let create () =
  let fdecl = Fdecl.create Dyn.Encoder.opaque in
  let pending_build_jobs = Queue.create () in
  let handler = Dune_rpc_server.make (handler fdecl) in
  let config =
    Dune_engine.Scheduler.Config.Rpc.Server { handler; backlog = 10 }
  in
  let build_mutex = Fiber.Mutex.create () in
  let t =
    { config
    ; build_mutex
    ; pending_build_jobs
    ; pending_shutdown = false
    ; promotion_subs = Session_set.empty
    ; error_subs = Session_set.empty
    ; clients = Session_set.empty
    }
  in
  Fdecl.set fdecl t;
  t

let build_mutex t = t.build_mutex

let config t = t.config

let pending_build_action t =
  if t.pending_shutdown then
    Some Shutdown
  else
    Queue.pop t.pending_build_jobs
    |> Option.map ~f:(fun (targets, ivar) -> Build (targets, ivar))
