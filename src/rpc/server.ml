open Stdune
open Dune_rpc.Private
open Fiber.O
module Session_id = Stdune.Id.Make ()
module User_message = Stdune.User_message

module Poller = struct
  module Id = Stdune.Id.Make ()

  type t =
    { id : Id.t
    ; name : Procedures.Poll.Name.t
    ; session_id : Session_id.t
    }

  let create session_id name = { id = Id.gen (); name; session_id }
  let to_dyn { id; name = _; session_id = _ } = Id.to_dyn id
  let compare x y = Id.compare x.id y.id
end

module V = Versioned.Make (struct
    include Fiber

    let collect_errors f =
      Fiber.collect_errors f
      >>| function
      | Ok _ as s -> s
      | Error exns ->
        Error (List.map exns ~f:(fun { Exn_with_backtrace.exn; backtrace = _ } -> exn))
    ;;

    let parallel_iter t ~f =
      let stream = Fiber.Stream.In.create t in
      Fiber.Stream.In.parallel_iter stream ~f
    ;;
  end)

module type Session = Server_intf.Session

module Session = struct
  module Id = Session_id

  module Close = struct
    type t =
      { ivar : unit Fiber.Ivar.t
      ; mutable state : [ `Open | `Closed | `Closing ]
      ; finalizer : unit -> unit Fiber.t
      }

    let create finalizer = { ivar = Fiber.Ivar.create (); state = `Open; finalizer }

    let close t =
      match t.state with
      | `Closing -> Fiber.Ivar.read t.ivar
      | `Closed -> Fiber.return ()
      | `Open ->
        t.state <- `Closing;
        Fiber.finalize t.finalizer ~finally:(fun () ->
          t.state <- `Closed;
          Fiber.Ivar.fill t.ivar ())
    ;;

    let to_dyn { state; ivar = _; finalizer = _ } =
      let name =
        match state with
        | `Open -> "Open"
        | `Closed -> "Closed"
        | `Closing -> "Closing"
      in
      Dyn.variant name []
    ;;
  end

  type 'a state =
    | Uninitialized
    | Initialized of
        { init : Initialize.Request.t
        ; state : 'a
        }

  module Pending_response = struct
    type t =
      | Response of Response.t
      | Closed
  end

  type 's chan' = (module Session with type t = 's) * 's
  type chan = Chan : 's chan' -> chan

  module Stage1 = struct
    type 'a t =
      { id : Id.t
      ; version : int * int
      ; close : Close.t
      ; chan : chan
      ; mutable menu : Menu.t option
      ; pool : Fiber.Pool.t
      ; mutable state : 'a state
      ; pending : (Dune_rpc.Private.Id.t, Pending_response.t Fiber.Ivar.t) Table.t
        (** Pending requests sent to the client. When a response is
          received, the ivar for the response will be filled. *)
      ; name : string
      }

    let set t state =
      match t.state with
      | Initialized s -> t.state <- Initialized { s with state }
      | Uninitialized -> Code_error.raise "set: state not available" []
    ;;

    let get t =
      match t.state with
      | Initialized s -> s.state
      | Uninitialized -> Code_error.raise "get: state not available" []
    ;;

    let initialize t =
      match t.state with
      | Initialized s -> s.init
      | Uninitialized -> Code_error.raise "initialize: request not available" []
    ;;

    let close_pending_requests pending =
      let pending_list = Table.to_list pending in
      Table.clear pending;
      Fiber.parallel_iter pending_list ~f:(fun (_, ivar) ->
        Fiber.Ivar.fill ivar Pending_response.Closed)
    ;;

    let create (type s) ~name ~version (chan, s) ~finalizer =
      let pending = Table.create (module Dune_rpc.Private.Id) 16 in
      let pool = Fiber.Pool.create () in
      let module Chan = (val chan : Session with type t = s) in
      { version
      ; chan = Chan (chan, s)
      ; menu = None
      ; close =
          Close.create (fun () ->
            let open Fiber.O in
            let+ () = Chan.close s
            and+ () = Fiber.of_thunk finalizer
            and+ () = Fiber.Pool.close pool
            and+ () = close_pending_requests pending in
            ())
      ; state = Uninitialized
      ; id = Id.gen ()
      ; pool
      ; pending
      ; name
      }
    ;;

    let menu t = t.menu
    let close t = Close.close t.close
    let id t = t.id

    let write t packet =
      let (Chan (chan, s)) = t.chan in
      let module Chan = (val chan) in
      let sexp = Conv.to_sexp Packet.sexp packet in
      Chan.write s [ sexp ]
      >>= function
      | Ok () -> Fiber.return `Ok
      | Error `Closed ->
        let+ () = Close.close t.close in
        `Closed
    ;;

    let read t =
      let (Chan (chan, s)) = t.chan in
      let module Chan = (val chan) in
      Chan.read s
      >>= function
      | None ->
        let+ () = Close.close t.close in
        None
      | Some sexp ->
        (match Conv.of_sexp Packet.sexp ~version:t.version sexp with
         | Ok message -> Fiber.return (Some message)
         | Error error ->
           Log.log (fun () ->
             Log.Message.create
               `Warn
               "invalid RPC packet from sesion"
               [ "error", Conv.dyn_of_error error ]);
           let+ () = Close.close t.close in
           None)
    ;;

    let request t ((id, call) as req) =
      match t.close.state with
      | `Closing | `Closed ->
        Fiber.return (Fiber.Ivar.create_full Pending_response.Closed)
      | `Open ->
        (match Table.find t.pending id with
         | Some _ ->
           Code_error.raise
             "request with this id is already pending"
             [ "id", Dune_rpc.Private.Id.to_dyn id
             ; "call", Dune_rpc.Private.Call.to_dyn call
             ]
         | None ->
           let ivar = Fiber.Ivar.create () in
           Table.add_exn t.pending id ivar;
           let+ () =
             Fiber.Pool.task t.pool ~f:(fun () ->
               let+ (_ : [> `Closed | `Ok ]) = write t (Request req) in
               ())
           in
           ivar)
    ;;

    let response t (id, response) =
      match Table.find t.pending id with
      | None ->
        (match t.close.state with
         | `Closing | `Closed -> Fiber.return ()
         | `Open ->
           Log.log (fun () ->
             Log.Message.create
               `Warn
               "unexpected response from rpc client"
               [ "response", Response.to_dyn response ]);
           Close.close t.close)
      | Some ivar ->
        Table.remove t.pending id;
        Fiber.Ivar.fill ivar (Pending_response.Response response)
    ;;

    let compare x y = Id.compare x.id y.id

    let dyn_of_state f =
      let open Dyn in
      function
      | Uninitialized -> variant "Uninitialized" []
      | Initialized { init; state } ->
        let record = record [ "init", opaque init; "state", f state ] in
        variant "Initialized" [ record ]
    ;;

    let to_dyn
          f
          { id; version = _; state; close; chan = _; pool = _; pending = _; menu; name }
      =
      let open Dyn in
      record
        [ "id", Id.to_dyn id
        ; "state", dyn_of_state f state
        ; "menu", Dyn.option Menu.to_dyn menu
        ; "name", Dyn.string name
        ; "close", Close.to_dyn close
        ]
    ;;

    let name t = t.name
  end

  type 'a t =
    { base : 'a Stage1.t
    ; handler : 'a t V.Handler.t
    ; mutable pollers : Poller.t Dune_rpc.Private.Id.Map.t
    }

  let get t = Stage1.get t.base
  let set t = Stage1.set t.base
  let closed t = Fiber.Ivar.read t.base.close.ivar
  let compare x y = Stage1.compare x.base y.base
  let id t = t.base.id

  let of_stage1 (base : _ Stage1.t) handler =
    { base; handler; pollers = Dune_rpc.Private.Id.Map.empty }
  ;;

  let prepare_notification t decl = V.Handler.prepare_notification t.handler decl

  let send_notification t { Versioned_intf.Staged.encode } n =
    let+ (_ : [> `Closed | `Ok ]) = Stage1.write t.base (Notification (encode n)) in
    ()
  ;;

  let raise_connection_dead id =
    let payload = Sexp.record [ "id", Dune_rpc.Private.Id.to_sexp id ] in
    let error =
      Response.Error.create
        ~kind:Connection_dead
        ~payload
        ~message:"Connection terminated. This request will never receive a response."
        ()
    in
    raise (Response.Error.E error)
  ;;

  let request t decl id req =
    let* () = Fiber.return () in
    match V.Handler.prepare_request t.handler decl with
    | Error error ->
      Code_error.raise
        "client doesn't support request"
        [ "id", Dune_rpc.Private.Id.to_dyn id
        ; "error", Dune_rpc.Private.Version_error.to_dyn error
        ]
    | Ok { Versioned_intf.Staged.encode_req; decode_resp } ->
      let req = encode_req req in
      let* ivar = Stage1.request t.base (id, req) in
      Fiber.Ivar.read ivar
      >>| (function
       | Pending_response.Closed -> raise_connection_dead id
       | Response response ->
         (match response with
          | Error error ->
            (* CR-soon rgrinberg: this is not a code error *)
            Code_error.raise
              "client and server do not agree on version"
              [ "error", Response.Error.to_dyn error ]
          | Ok resp ->
            (match decode_resp resp with
             | Ok s -> s
             | Error error ->
               (* CR-soon rgrinberg: this is not a code error *)
               Code_error.raise
                 "unexpected response"
                 [ "error", Response.Error.to_dyn error ])))
  ;;

  let to_dyn f t =
    Dyn.Record [ "handler", Dyn.String "<handler>"; "base", Stage1.to_dyn f t.base ]
  ;;

  let find_or_create_poller t (name : Procedures.Poll.Name.t) id =
    match Dune_rpc.Private.Id.Map.find t.pollers id with
    | Some poller -> poller
    | None ->
      let poller = Poller.create t.base.id name in
      t.pollers <- Dune_rpc.Private.Id.Map.add_exn t.pollers id poller;
      poller
  ;;

  let cancel_poller t id =
    match Dune_rpc.Private.Id.Map.find t.pollers id with
    | None -> None
    | Some poller ->
      t.pollers <- Dune_rpc.Private.Id.Map.remove t.pollers id;
      Some poller
  ;;

  let name t = t.base.name
end

type message_kind =
  | Request of Dune_rpc.Private.Id.t
  | Notification

module Event = struct
  type t =
    | Session of Dune_trace.Event.Rpc.stage
    | Message of
        { kind : message_kind
        ; meth_ : Method.Name.t
        ; stage : Dune_trace.Event.Rpc.stage
        }

  let emit t id =
    Dune_trace.emit Rpc (fun () ->
      let id = Session_id.to_int id in
      match t with
      | Session stage -> Dune_trace.Event.Rpc.session ~id stage
      | Message { kind; meth_; stage } ->
        let kind =
          match kind with
          | Request id -> `Request (Dune_rpc.Private.Id.to_sexp id)
          | Notification -> `Notification
        in
        Dune_trace.Event.Rpc.message kind ~meth_:(Method.Name.to_string meth_) ~id stage)
  ;;
end

module H = struct
  type 'a base =
    { on_init : 'a Session.Stage1.t -> Initialize.Request.t -> 'a Fiber.t
    ; on_terminate : 'a Session.Stage1.t -> unit Fiber.t
    ; on_upgrade : 'a Session.t -> Menu.t -> unit Fiber.t
    ; version : int * int
    }

  type 'a stage1 =
    { base : 'a base
    ; to_handler : Menu.t -> 'a Session.t V.Handler.t
    ; known_versions : Int.Set.t Method.Name.Map.t
    }

  type 'a t = { handler : 'a Session.t V.Handler.t }

  let abort ?payload (session : _ Session.Stage1.t) ~message =
    let open Fiber.O in
    let msg = { Message.message; payload } in
    let call =
      { Call.params = Message.to_sexp_unversioned msg
      ; method_ = Method.Name.of_string "notify/abort"
      }
    in
    let* (_ : [> `Closed | `Ok ]) = Session.Stage1.write session (Notification call) in
    Session.Stage1.close session
  ;;

  (* TODO catch and convert dispatch users *)

  let dispatch_notification (type a) (t : a t) (session : a Session.t) meth_ n =
    let kind = Notification in
    Event.emit (Message { kind; meth_; stage = `Start }) (Session.id session);
    let+ result = V.Handler.handle_notification t.handler session n in
    let () =
      match result with
      | Error e ->
        Code_error.raise
          "received badly-versioned notification"
          [ ( "notification"
            , Dyn.Record
                [ "method_", Method.Name.to_dyn n.method_
                ; "params", Sexp.to_dyn n.params
                ] )
          ; "description", Response.Error.to_dyn e
          ]
      | Ok r -> r
    in
    Event.emit (Message { kind; meth_; stage = `Stop }) (Session.id session)
  ;;

  let dispatch_request (type a) (t : a t) (session : a Session.t) meth_ r id =
    let kind = Request id in
    Event.emit (Message { kind; meth_; stage = `Start }) (Session.id session);
    let* response =
      let+ result =
        Fiber.collect_errors (fun () ->
          V.Handler.handle_request t.handler session (id, r))
      in
      match result with
      | Ok r -> r
      | Error [ { Exn_with_backtrace.exn = Response.Error.E e; backtrace = _ } ] ->
        Error e
      | Error xs ->
        let payload =
          Sexp.List (List.map xs ~f:(fun x -> Exn_with_backtrace.to_dyn x |> Sexp.of_dyn))
        in
        Error (Response.Error.create ~kind:Code_error ~message:"server error" ~payload ())
    in
    Event.emit (Message { kind; meth_; stage = `Stop }) (Session.id session);
    let+ (_ : [> `Closed | `Ok ]) =
      Session.Stage1.write session.base (Response (id, response))
    in
    ()
  ;;

  let run_session (type a) (t : a t) (session : a Session.t) =
    let open Fiber.O in
    let dispatch_in_pool f = Fiber.Pool.task session.base.pool ~f in
    let rec loop () =
      Session.Stage1.read session.base
      >>= function
      | None -> Fiber.return ()
      | Some (message : Packet.t) ->
        let* () =
          match message with
          | Response resp -> Session.Stage1.response session.base resp
          | Notification n ->
            dispatch_in_pool (fun () -> dispatch_notification t session n.method_ n)
          | Request (id, r) ->
            dispatch_in_pool (fun () -> dispatch_request t session r.method_ r id)
        in
        loop ()
    in
    loop ()
  ;;

  let negotiate_version (type a) (t : a stage1) (session : a Session.Stage1.t) =
    let open Fiber.O in
    Session.Stage1.read session
    >>= function
    | None -> Fiber.return ()
    | Some client_versions ->
      (match (client_versions : Packet.t) with
       | Response _ ->
         abort session ~message:"Response unexpected. No requests before negotiation"
       | Notification _ ->
         abort
           session
           ~message:
             "Notification unexpected. You must complete version negotiation first."
       | Request (id, call) ->
         (match Version_negotiation.Request.of_call ~version:t.base.version call with
          | Error e ->
            let+ (_ : [> `Closed | `Ok ]) =
              Session.Stage1.write session (Response (id, Error e))
            in
            ()
          | Ok (Menu client_versions) ->
            (match
               Menu.select_common
                 ~remote_versions:client_versions
                 ~local_versions:t.known_versions
             with
             | None ->
               abort
                 session
                 ~message:"Server and client have no method versions in common"
             | Some menu ->
               let response =
                 Version_negotiation.(
                   Conv.to_sexp Response.sexp (Response.create (Menu.to_list menu)))
               in
               Session.Stage1.write session (Response (id, Ok response))
               >>= (function
                | `Closed -> Fiber.return ()
                | `Ok ->
                  let handler = t.to_handler menu in
                  session.menu <- Some menu;
                  let session = Session.of_stage1 session handler in
                  let* () = t.base.on_upgrade session menu in
                  run_session { handler } session))))
  ;;

  let handle (type a) (t : a stage1) (session : a Session.Stage1.t) =
    let open Fiber.O in
    let* () = Fiber.return () in
    Session.Stage1.read session
    >>= function
    | None -> Fiber.return ()
    | Some init ->
      (match (init : Packet.t) with
       | Response _ ->
         abort session ~message:"Response unexpected. You must initialize first."
       | Notification _ ->
         abort session ~message:"Notification unexpected. You must initialize first."
       | Request (id, call) ->
         (match Initialize.Request.of_call ~version:t.base.version call with
          | Error e ->
            let+ (_ : [> `Closed | `Ok ]) =
              Session.Stage1.write session (Response (id, Error e))
            in
            ()
          | Ok init ->
            let protocol_ver = Initialize.Request.protocol_version init in
            if protocol_ver <> Protocol.latest_version
            then
              abort session ~message:"The server and client use incompatible protocols."
            else
              let* a = t.base.on_init session init in
              let () = session.state <- Initialized { init; state = a } in
              let response =
                Ok (Initialize.Response.to_response (Initialize.Response.create ()))
              in
              Session.Stage1.write session (Response (id, response))
              >>= (function
               | `Closed -> Fiber.return ()
               | `Ok -> negotiate_version t session)))
  ;;

  module Builder = struct
    type 's t =
      { builder : 's Session.t V.Builder.t
      ; on_terminate : 's Session.Stage1.t -> unit Fiber.t
      ; on_init : 's Session.Stage1.t -> Initialize.Request.t -> 's Fiber.t
      ; on_upgrade : 's Session.t -> Menu.t -> unit Fiber.t
      ; version : int * int
      }

    let to_handler { builder; on_terminate; on_init; version; on_upgrade } =
      let to_handler menu =
        V.Builder.to_handler builder ~menu ~session_version:(fun s ->
          (Session.Stage1.initialize s.base).dune_version)
      in
      let known_versions =
        V.Builder.registered_procedures builder
        |> Method.Name.Map.of_list_map_exn ~f:(fun (name, gens) ->
          name, Int.Set.of_list gens)
      in
      { to_handler
      ; base = { on_init; on_terminate; on_upgrade; version }
      ; known_versions
      }
    ;;

    let create
          ?(on_terminate = fun _ -> Fiber.return ())
          ~on_init
          ?(on_upgrade = fun _ _ -> Fiber.return ())
          ~version
          ()
      =
      { builder = V.Builder.create (); on_init; on_terminate; version; on_upgrade }
    ;;

    let implement_request (t : _ t) = V.Builder.implement_request t.builder
    let implement_notification (t : _ t) = V.Builder.implement_notification t.builder
    let declare_notification (t : _ t) = V.Builder.declare_notification t.builder
    let declare_request (t : _ t) = V.Builder.declare_request t.builder

    module Long_poll = struct
      let implement_poll (t : _ t) (sub : _ Procedures.Poll.t) ~on_poll ~on_cancel =
        let on_poll session id =
          let poller =
            Session.find_or_create_poller session (Procedures.Poll.name sub) id
          in
          let+ res = on_poll session poller in
          let () =
            match res with
            | Some _ -> ()
            | None ->
              let (_ : Poller.t option) = Session.cancel_poller session id in
              ()
          in
          res
        in
        let on_cancel session id =
          let poller = Session.cancel_poller session id in
          match poller with
          | None -> Fiber.return () (* XXX log *)
          | Some poller -> on_cancel session poller
        in
        implement_request t (Procedures.Poll.poll sub) on_poll;
        implement_notification t (Procedures.Poll.cancel sub) on_cancel
      ;;

      module Poll_comparable = Comparable.Make (Poller)
      module Map = Poll_comparable.Map

      module Status = struct
        type 'a t =
          | Active of 'a
          | Cancelled
      end

      let on_cancel map _session poller =
        let new_map =
          Map.update !map poller ~f:(function
            | None -> assert false
            | Some Status.Cancelled as s -> s
            | Some (Active _) -> Some Cancelled)
        in
        map := new_map;
        Fiber.return ()
      ;;

      let make_on_poll map svar ~equal ~diff _session poller =
        let send last =
          let* () =
            match last with
            | None -> Fiber.return ()
            | Some last ->
              let until x = not (equal x last) in
              Fiber.Svar.wait svar ~until
          in
          let now = Fiber.Svar.read svar in
          map := Map.set !map poller (Status.Active now);
          let to_send = diff ~last ~now in
          Fiber.return (Some to_send)
        in
        match Map.find !map poller with
        | None -> send None
        | Some (Active a) -> send (Some a)
        | Some Cancelled ->
          map := Map.remove !map poller;
          Fiber.never
      ;;

      let implement_long_poll (rpc : _ t) proc svar ~equal ~diff =
        let map = ref Map.empty in
        implement_poll
          rpc
          proc
          ~on_cancel:(on_cancel map)
          ~on_poll:(make_on_poll map svar ~equal ~diff)
      ;;
    end

    let implement_long_poll = Long_poll.implement_long_poll

    module For_tests = struct
      let implement_poll t poll ~on_poll ~on_cancel =
        let on_poll session _poller = on_poll session in
        let on_cancel session _poller = on_cancel session in
        Long_poll.implement_poll t poll ~on_poll ~on_cancel
      ;;
    end
  end
end

type t = Server : 'a H.stage1 -> t

let make (type a) (h : a H.Builder.t) : t = Server (H.Builder.to_handler h)

let new_session (Server handler) ~name chan =
  let session = Fdecl.create Dyn.opaque in
  Fdecl.set
    session
    (Session.Stage1.create ~name ~version:handler.base.version chan ~finalizer:(fun () ->
       let session : _ Session.Stage1.t = Fdecl.get session in
       handler.base.on_terminate session));
  let session = Fdecl.get session in
  object
    method id = session.id
    method close = Session.Stage1.close session

    method start =
      Fiber.fork_and_join_unit
        (fun () -> Fiber.Pool.run session.pool)
        (fun () ->
           let* () = H.handle handler session in
           Session.Stage1.close session)
  end
;;

module Make (S : Session) = struct
  open Fiber.O

  let serve sessions server =
    Fiber.Stream.In.parallel_iter sessions ~f:(fun session ->
      let session =
        let name = S.name session in
        new_session server ~name ((module S), session)
      in
      let id = session#id in
      Event.emit (Session `Start) id;
      let+ res =
        Fiber.map_reduce_errors
          (module Monoid.Unit)
          (fun () -> session#start)
          ~on_error:(fun exn ->
            (* TODO report errors in dune_stats as well *)
            (match exn.exn with
             | Dune_util.Report_error.Already_reported -> ()
             | _ ->
               Log.warn
                 "encountered error serving rpc client"
                 [ "id", Dyn.int (Session.Id.to_int id)
                 ; "error", Exn_with_backtrace.to_dyn exn
                 ];
               Dune_util.Report_error.report exn);
            session#close)
      in
      Event.emit (Session `Stop) id;
      match res with
      | Ok () -> ()
      | Error () ->
        (* already reported above *)
        ())
  ;;
end

module Handler = H.Builder

let serve =
  let module M = Make (struct
      include Csexp_rpc.Session

      let name _ = "dune"
    end)
  in
  M.serve
;;
