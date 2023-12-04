open Stdune
open Dune_rpc_private
open Fiber.O
module Session_id = Stdune.Id.Make ()
module User_message = Stdune.User_message

type error =
  { message : User_message.t
  ; details : (string * Dyn.t) list
  }

exception Invalid_session of error

let () =
  Printexc.register_printer (function
    | Invalid_session { message; details } ->
      let message =
        let paragraphs =
          message.paragraphs @ [ Pp.text "details:"; Dyn.pp (Dyn.record details) ]
        in
        { message with paragraphs }
      in
      Some (User_message.to_string message)
    | _ -> None)
;;

let raise_invalid_session message details =
  let message = User_message.make [ Pp.text message ] in
  raise (Invalid_session { message; details })
;;

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

    let parallel_iter t ~f =
      let stream = Fiber.Stream.In.create t in
      Fiber.Stream.In.parallel_iter stream ~f
    ;;
  end)

module Session = struct
  module Id = Session_id

  module Close = struct
    type t =
      { ivar : unit Fiber.Ivar.t
      ; mutable state : [ `Open | `Closed ]
      ; finalizer : unit -> unit Fiber.t
      }

    let create finalizer = { ivar = Fiber.Ivar.create (); state = `Open; finalizer }

    let to_dyn { state; ivar = _; finalizer = _ } =
      let name =
        match state with
        | `Open -> "Open"
        | `Closed -> "Closed"
      in
      Dyn.variant name []
    ;;

    let close t =
      match t.state with
      | `Closed -> Fiber.return ()
      | `Open ->
        t.state <- `Closed;
        Fiber.fork_and_join_unit t.finalizer (Fiber.Ivar.fill t.ivar)
    ;;
  end

  type 'a state =
    | Uninitialized
    | Initialized of
        { init : Initialize.Request.t
        ; state : 'a
        }

  module Stage1 = struct
    type 'a t =
      { queries : Packet.t Fiber.Stream.In.t
      ; id : Id.t
      ; close : Close.t
      ; mutable menu : Menu.t option
      ; send : Packet.t list option -> unit Fiber.t
      ; pool : Fiber.Pool.t
      ; mutable state : 'a state
      ; (* TODO these should be cancelled when the connection closes *)
        pending : (Dune_rpc_private.Id.t, Response.t Fiber.Ivar.t) Table.t
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

    let create ~name ~queries ~send ~finalizer =
      { queries
      ; send
      ; menu = None
      ; close =
          Close.create (fun () ->
            Fiber.fork_and_join_unit (fun () -> send None) finalizer)
      ; state = Uninitialized
      ; id = Id.gen ()
      ; pool = Fiber.Pool.create ()
      ; pending = Table.create (module Dune_rpc_private.Id) 16
      ; name
      }
    ;;

    let menu t = t.menu
    let close t = Close.close t.close
    let id t = t.id

    let request t ((id, call) as req) =
      match Table.find t.pending id with
      | Some _ ->
        Code_error.raise
          "request with this id is already pending"
          [ "id", Dune_rpc_private.Id.to_dyn id
          ; "call", Dune_rpc_private.Call.to_dyn call
          ]
      | None ->
        let ivar = Fiber.Ivar.create () in
        Table.add_exn t.pending id ivar;
        let+ () = Fiber.Pool.task t.pool ~f:(fun () -> t.send (Some [ Request req ])) in
        ivar
    ;;

    let response t (id, response) =
      match Table.find t.pending id with
      | None -> raise_invalid_session "unexpected response" []
      | Some ivar ->
        Table.remove t.pending id;
        Fiber.Ivar.fill ivar response
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
      { id; state; close; queries = _; send = _; pool = _; pending = _; menu; name }
      =
      let open Dyn in
      record
        [ "id", Id.to_dyn id
        ; "state", dyn_of_state f state
        ; "menu", Dyn.option Menu.to_dyn menu
        ; "close", Close.to_dyn close
        ; "name", Dyn.string name
        ]
    ;;

    let name t = t.name
  end

  type 'a t =
    { base : 'a Stage1.t
    ; handler : 'a t V.Handler.t
    ; mutable pollers : Poller.t Dune_rpc_private.Id.Map.t
    }

  let get t = Stage1.get t.base
  let set t = Stage1.set t.base
  let closed t = Fiber.Ivar.read t.base.close.ivar
  let compare x y = Stage1.compare x.base y.base
  let id t = t.base.id

  let of_stage1 (base : _ Stage1.t) handler =
    { base; handler; pollers = Dune_rpc_private.Id.Map.empty }
  ;;

  let prepare_notification t decl = V.Handler.prepare_notification t.handler decl

  let send_notification t { Versioned.Staged.encode } n =
    t.base.send (Some [ Notification (encode n) ])
  ;;

  let request t decl id req =
    let* () = Fiber.return () in
    match V.Handler.prepare_request t.handler decl with
    | Error error ->
      Code_error.raise
        "client doesn't support request"
        [ "id", Dune_rpc_private.Id.to_dyn id
        ; "error", Dune_rpc_private.Version_error.to_dyn error
        ]
    | Ok { Versioned.Staged.encode_req; decode_resp } ->
      let req = encode_req req in
      let* ivar = Stage1.request t.base (id, req) in
      let+ resp = Fiber.Ivar.read ivar in
      (match resp with
       | Error error ->
         Code_error.raise
           "client and server do not agree on version"
           [ "error", Response.Error.to_dyn error ]
       | Ok resp ->
         (match decode_resp resp with
          | Ok s -> s
          | Error error ->
            Code_error.raise
              "unexpected response"
              [ "error", Response.Error.to_dyn error ]))
  ;;

  let to_dyn f t =
    Dyn.Record [ "handler", Dyn.String "<handler>"; "base", Stage1.to_dyn f t.base ]
  ;;

  let find_or_create_poller t (name : Procedures.Poll.Name.t) id =
    match Dune_rpc_private.Id.Map.find t.pollers id with
    | Some poller -> poller
    | None ->
      let poller = Poller.create t.base.id name in
      t.pollers <- Dune_rpc_private.Id.Map.add_exn t.pollers id poller;
      poller
  ;;

  let cancel_poller t id =
    match Dune_rpc_private.Id.Map.find t.pollers id with
    | None -> None
    | Some poller ->
      t.pollers <- Dune_rpc_private.Id.Map.remove t.pollers id;
      Some poller
  ;;

  let name t = t.base.name
end

type message_kind =
  | Request of Dune_rpc_private.Id.t
  | Notification

type stage =
  | Start
  | Stop

module Event = struct
  module Event = Chrome_trace.Event

  let async_kind_of_stage = function
    | Start -> Event.Start
    | Stop -> Event.End
  ;;

  type t =
    | Session of stage
    | Message of
        { kind : message_kind
        ; meth_ : string
        ; stage : stage
        }

  let emit t stats id =
    Option.iter stats ~f:(fun stats ->
      let event =
        let kind, name, args =
          match t with
          | Session stage -> async_kind_of_stage stage, "rpc_session", None
          | Message { kind; meth_; stage } ->
            let args =
              match kind with
              | Notification -> None
              | Request id ->
                let id = Dune_rpc_private.Id.to_sexp id in
                let rec to_json : Sexp.t -> Chrome_trace.Json.t = function
                  | Atom s -> `String s
                  | List s -> `List (List.map s ~f:to_json)
                in
                Some [ "request_id", to_json id ]
            in
            async_kind_of_stage stage, meth_, args
        in
        let common =
          let ts = Event.Timestamp.of_float_seconds (Unix.gettimeofday ()) in
          Event.common_fields ~ts ~name ()
        in
        let id = Chrome_trace.Id.create (`Int (Session.Id.to_int id)) in
        Event.async ?args id kind common
      in
      Dune_stats.emit stats event)
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
    ; known_versions : Int.Set.t String.Map.t
    }

  type 'a t = { handler : 'a Session.t V.Handler.t }

  let abort ?payload (session : _ Session.Stage1.t) ~message =
    let open Fiber.O in
    let msg = { Message.message; payload } in
    let call =
      { Call.params = Message.to_sexp_unversioned msg; method_ = "notify/abort" }
    in
    let* () = session.send (Some [ Notification call ]) in
    session.send None
  ;;

  (* TODO catch and convert dispatch users *)

  let dispatch_notification (type a) (t : a t) stats (session : a Session.t) meth_ n =
    let kind = Notification in
    Event.emit (Message { kind; meth_; stage = Start }) stats (Session.id session);
    let+ result = V.Handler.handle_notification t.handler session n in
    let () =
      match result with
      | Error e ->
        Code_error.raise
          "received badly-versioned notification"
          [ ( "notification"
            , Dyn.Record
                [ "method_", Dyn.String n.method_; "params", Sexp.to_dyn n.params ] )
          ; "description", Response.Error.to_dyn e
          ]
      | Ok r -> r
    in
    Event.emit (Message { kind; meth_; stage = Stop }) stats (Session.id session)
  ;;

  let dispatch_request (type a) (t : a t) stats (session : a Session.t) meth_ r id =
    let kind = Request id in
    Event.emit (Message { kind; meth_; stage = Start }) stats (Session.id session);
    let* response =
      let+ result =
        (* TODO instead of waiting for all errors, wait for the first one.
           The fiber might never finish anyway. *)
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
    Event.emit (Message { kind; meth_; stage = Stop }) stats (Session.id session);
    match session.base.close.state with
    | `Closed -> Fiber.return ()
    | `Open -> session.base.send (Some [ Response (id, response) ])
  ;;

  let run_session (type a) (t : a t) stats (session : a Session.t) =
    let open Fiber.O in
    let* () =
      Fiber.Stream.In.parallel_iter session.base.queries ~f:(fun (message : Packet.t) ->
        match message with
        | Response resp -> Session.Stage1.response session.base resp
        | Notification n -> dispatch_notification t stats session n.method_ n
        | Request (id, r) -> dispatch_request t stats session r.method_ r id)
    in
    Session.Stage1.close session.base
  ;;

  let negotiate_version (type a) (t : a stage1) stats (session : a Session.Stage1.t) =
    let open Fiber.O in
    let* query = Fiber.Stream.In.read session.queries in
    match query with
    | None -> session.send None
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
          | Error e -> session.send (Some [ Response (id, Error e) ])
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
               let* () = session.send (Some [ Response (id, Ok response) ]) in
               let handler = t.to_handler menu in
               session.menu <- Some menu;
               let session = Session.of_stage1 session handler in
               let* () = t.base.on_upgrade session menu in
               run_session { handler } stats session)))
  ;;

  let handle (type a) (t : a stage1) stats (session : a Session.Stage1.t) =
    let open Fiber.O in
    let* () = Fiber.return () in
    let* query = Fiber.Stream.In.read session.queries in
    match query with
    | None -> session.send None
    | Some init ->
      (match (init : Packet.t) with
       | Response _ ->
         abort session ~message:"Response unexpected. You must initialize first."
       | Notification _ ->
         abort session ~message:"Notification unexpected. You must initialize first."
       | Request (id, call) ->
         (match Initialize.Request.of_call ~version:t.base.version call with
          | Error e -> session.send (Some [ Response (id, Error e) ])
          | Ok init ->
            let protocol_ver = Initialize.Request.protocol_version init in
            if protocol_ver <> Protocol.latest_version
            then
              abort session ~message:"The server and client use incompatible protocols."
            else
              let* a = t.base.on_init session init in
              let () = session.state <- Initialized { init; state = a } in
              let* () =
                let response =
                  Ok (Initialize.Response.to_response (Initialize.Response.create ()))
                in
                session.send (Some [ Response (id, response) ])
              in
              negotiate_version t stats session))
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
        |> String.Map.of_list_map_exn ~f:(fun (name, gens) -> name, Int.Set.of_list gens)
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
let version (Server h) = h.base.version

let new_session (Server handler) stats ~name ~queries ~send =
  let session = Fdecl.create Dyn.opaque in
  Fdecl.set
    session
    (Session.Stage1.create ~name ~queries ~send ~finalizer:(fun () ->
       let session : _ Session.Stage1.t = Fdecl.get session in
       Fiber.fork_and_join_unit
         (fun () -> Fiber.Pool.close session.pool)
         (fun () -> handler.base.on_terminate session)));
  let session = Fdecl.get session in
  object
    method id = session.id
    method close = Session.Stage1.close session

    method start =
      Fiber.fork_and_join_unit
        (fun () -> Fiber.Pool.run session.pool)
        (fun () ->
          let* () = H.handle handler stats session in
          Session.Stage1.close session)
  end
;;

let create_sequence f ~version conv =
  let read () =
    let+ read = f () in
    Option.map read ~f:(fun sexp ->
      match Conv.of_sexp conv ~version sexp with
      | Ok message -> message
      | Error error ->
        raise_invalid_session "unexpected csexp" [ "error", Conv.dyn_of_error error ])
  in
  Fiber.Stream.In.create read
;;

module Make (S : sig
    type t

    val close : t -> unit Fiber.t
    val write : t -> Sexp.t list -> (unit, [ `Closed ]) result Fiber.t
    val read : t -> Sexp.t option Fiber.t
    val name : t -> string
  end) =
struct
  open Fiber.O

  let serve sessions stats server =
    Fiber.Stream.In.parallel_iter sessions ~f:(fun session ->
      let session =
        let send = function
          | None -> S.close session
          | Some packets ->
            List.map packets ~f:(Conv.to_sexp Packet.sexp)
            |> S.write session
            >>| (function
             | Ok () -> ()
             | Error `Closed -> raise Dune_util.Report_error.Already_reported)
        in
        let queries =
          create_sequence (fun () -> S.read session) ~version:(version server) Packet.sexp
        in
        let name = S.name session in
        new_session server stats ~name ~queries ~send
      in
      let id = session#id in
      Event.emit (Session Start) stats id;
      let+ res =
        Fiber.map_reduce_errors
          (module Monoid.Unit)
          (fun () -> session#start)
          ~on_error:(fun exn ->
            (* TODO report errors in dune_stats as well *)
            (match exn.exn with
             | Dune_util.Report_error.Already_reported -> ()
             | _ ->
               Dune_util.Log.info
                 [ Pp.textf
                     "encountered error serving rpc client (id %d)"
                     (Session.Id.to_int id)
                 ; Exn_with_backtrace.pp exn
                 ]);
            Dune_util.Report_error.report exn;
            session#close)
      in
      Event.emit (Session Stop) stats id;
      match res with
      | Ok () -> ()
      | Error () ->
        (* already reported above *)
        ())
  ;;
end

module Handler = H.Builder
