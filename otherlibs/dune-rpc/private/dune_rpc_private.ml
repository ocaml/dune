open Import
module Conv = Conv
module Versioned = Versioned
module Menu = Menu
module Procedures = Procedures
module Where = Where
module Registry = Registry
include Types
include Exported_types
module Version_error = Versioned.Version_error
module Decl = Decl

module type Fiber = Fiber_intf.S

module Sub = struct
  type 'a t =
    { poll : (Id.t, 'a option) Decl.Request.witness
    ; cancel : Id.t Decl.Notification.witness
    ; id : Procedures.Poll.Name.t
    }

  let of_procedure p =
    let open Procedures.Poll in
    { poll = (poll p).decl; cancel = (cancel p).decl; id = name p }

  let poll t = t.poll

  let poll_cancel t = t.cancel

  module Id = Procedures.Poll.Name

  let id t = t.id
end

module Public = struct
  module Request = struct
    type ('a, 'b) t = ('a, 'b) Decl.Request.witness

    let ping = Procedures.Public.ping.decl

    let diagnostics = Procedures.Public.diagnostics.decl

    let format_dune_file = Procedures.Public.format_dune_file.decl

    let promote = Procedures.Public.promote.decl

    let build_dir = Procedures.Public.build_dir.decl
  end

  module Notification = struct
    type 'a t = 'a Decl.Notification.witness

    let shutdown = Procedures.Public.shutdown.decl
  end

  module Sub = struct
    type 'a t = 'a Sub.t

    let diagnostic = Sub.of_procedure Procedures.Poll.diagnostic

    let progress = Sub.of_procedure Procedures.Poll.progress
  end
end

module Server_notifications = struct
  let abort = Procedures.Server_side.abort.decl

  let log = Procedures.Server_side.log.decl
end

module Client = struct
  module type S = sig
    type t

    type 'a fiber

    type chan

    module Versioned : sig
      type ('a, 'b) request = ('a, 'b) Versioned.Staged.request

      type 'a notification = 'a Versioned.Staged.notification

      val prepare_request :
           t
        -> ('a, 'b) Decl.Request.witness
        -> (('a, 'b) request, Version_error.t) result fiber

      val prepare_notification :
           t
        -> 'a Decl.Notification.witness
        -> ('a notification, Version_error.t) result fiber
    end

    val request :
         ?id:Id.t
      -> t
      -> ('a, 'b) Versioned.request
      -> 'a
      -> ('b, Response.Error.t) result fiber

    val notification : t -> 'a Versioned.notification -> 'a -> unit fiber

    val disconnected : t -> unit fiber

    module Stream : sig
      type 'a t

      val cancel : _ t -> unit fiber

      val next : 'a t -> 'a option fiber
    end

    val poll :
      ?id:Id.t -> t -> 'a Sub.t -> ('a Stream.t, Version_error.t) result fiber

    module Batch : sig
      type t

      type client

      val create : client -> t

      val request :
           ?id:Id.t
        -> t
        -> ('a, 'b) Versioned.request
        -> 'a
        -> ('b, Response.Error.t) result fiber

      val notification : t -> 'a Versioned.notification -> 'a -> unit

      val submit : t -> unit fiber
    end
    with type client := t

    module Handler : sig
      type t

      val create :
           ?log:(Message.t -> unit fiber)
        -> ?abort:(Message.t -> unit fiber)
        -> unit
        -> t
    end

    type proc =
      | Request : ('a, 'b) Decl.request -> proc
      | Notification : 'a Decl.notification -> proc
      | Poll : 'a Procedures.Poll.t -> proc

    val connect_with_menu :
         ?handler:Handler.t
      -> private_menu:proc list
      -> chan
      -> Initialize.Request.t
      -> f:(t -> 'a fiber)
      -> 'a fiber

    val connect :
         ?handler:Handler.t
      -> chan
      -> Initialize.Request.t
      -> f:(t -> 'a fiber)
      -> 'a fiber
  end

  module Make (Fiber : sig
    type 'a t

    val return : 'a -> 'a t

    val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t

    val parallel_iter : (unit -> 'a option t) -> f:('a -> unit t) -> unit t

    val finalize : (unit -> 'a t) -> finally:(unit -> unit t) -> 'a t

    module O : sig
      val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

      val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    end

    module Ivar : sig
      type 'a fiber

      type 'a t

      val create : unit -> 'a t

      val read : 'a t -> 'a fiber

      val fill : 'a t -> 'a -> unit fiber
    end
    with type 'a fiber := 'a t
  end) (Chan : sig
    type t

    val write : t -> Sexp.t list option -> unit Fiber.t

    val read : t -> Sexp.t option Fiber.t
  end) =
  struct
    open Fiber.O
    module V = Versioned.Make (Fiber)

    module Chan = struct
      type t =
        { read : unit -> Sexp.t option Fiber.t
        ; write : Sexp.t list option -> unit Fiber.t
        ; closed_read : bool
        ; mutable closed_write : bool
        ; disconnected : unit Fiber.Ivar.t
        }

      let of_chan c =
        let disconnected = Fiber.Ivar.create () in
        let read () =
          let* result = Chan.read c in
          match result with
          | None ->
            let+ () = Fiber.Ivar.fill disconnected () in
            None
          | _ -> Fiber.return result
        in
        { read
        ; write = (fun s -> Chan.write c s)
        ; closed_read = false
        ; closed_write = false
        ; disconnected
        }

      let write t s =
        let* () = Fiber.return () in
        match s with
        | Some _ -> t.write s
        | None ->
          if t.closed_write then Fiber.return ()
          else (
            t.closed_write <- true;
            t.write None)

      let read t =
        let* () = Fiber.return () in
        if t.closed_read then Fiber.return None else t.read ()
    end

    type abort =
      | Invalid_session of Conv.error
      | Server_aborted of Message.t

    exception Abort of abort

    let () =
      Printexc.register_printer (function
        | Abort error ->
          let dyn =
            match error with
            | Invalid_session e ->
              Dyn.variant "Invalid_session" [ Conv.dyn_of_error e ]
            | Server_aborted e ->
              Dyn.variant "Server_aborted"
                [ Sexp.to_dyn (Message.to_sexp_unversioned e) ]
          in
          Some (Dyn.to_string dyn)
        | _ -> None)

    type t =
      { chan : Chan.t
      ; requests :
          ( Id.t
          , [ `Cancelled
            | `Pending of [ `Completed of Response.t | `Cancelled ] Fiber.Ivar.t
            ] )
          Table.t
      ; initialize : Initialize.Request.t
      ; mutable next_id : int
      ; mutable running : bool
      ; mutable handler_initialized : bool
      ; (* We need this field to be an Ivar to ensure that any typed
           communications are correctly versioned. The contract of the [Fiber]
           interface ensures that this will be filled before any user code is
           run. *)
        handler : unit V.Handler.t Fiber.t
      ; on_preemptive_abort : Message.t -> unit Fiber.t
      }

    (* When the client is terminated via this function, the session is
       considered to be dead without a way to recover. *)
    let terminate t =
      let* () = Fiber.return () in
      match t.running with
      | false -> Fiber.return ()
      | true ->
        t.running <- false;
        let ivars = ref [] in
        Table.filteri_inplace t.requests ~f:(fun ~key:id ~data:ivar ->
            ivars := (id, ivar) :: !ivars;
            false);
        let ivars () =
          Fiber.return
            (match !ivars with
            | [] -> None
            | x :: xs ->
              ivars := xs;
              Some x)
        in
        Fiber.fork_and_join_unit
          (fun () -> Chan.write t.chan None)
          (fun () ->
            Fiber.parallel_iter ivars ~f:(fun (id, status) ->
                match status with
                | `Cancelled -> Fiber.return ()
                | `Pending ivar ->
                  let error =
                    let payload = Sexp.record [ ("id", Id.to_sexp id) ] in
                    Response.Error.create ~kind:Code_error ~payload
                      ~message:
                        "connection terminated. this request will never \
                         receive a response"
                      ()
                  in
                  Fiber.Ivar.fill ivar (`Completed (Error error))))

    let terminate_with_error t message info =
      Fiber.fork_and_join_unit
        (fun () -> terminate t)
        (fun () -> Code_error.raise message info)

    let send conn (packet : Packet.Query.t list option) =
      let sexps =
        Option.map packet
          ~f:
            (List.map ~f:(function
              | Packet.Query.Notification p ->
                Conv.to_sexp (Conv.record Call.fields) p
              | Request (id, request) ->
                let conv =
                  Conv.record (Conv.both Id.required_field Call.fields)
                in
                Conv.to_sexp conv (id, request)))
      in
      Chan.write conn.chan sexps

    let create ~chan ~initialize ~handler ~on_preemptive_abort =
      let requests = Table.create (module Id) 16 in
      { chan
      ; requests
      ; next_id = 0
      ; initialize
      ; running = true
      ; handler_initialized = false
      ; handler
      ; on_preemptive_abort
      }

    let prepare_request' conn (id, req) =
      match conn.running with
      | false ->
        let err =
          let payload =
            Sexp.record
              [ ("id", Id.to_sexp id)
              ; ("req", Conv.to_sexp (Conv.record Call.fields) req)
              ]
          in
          Response.Error.create ~payload
            ~message:"request sent while connection is dead" ~kind:Code_error ()
        in
        Error err
      | true ->
        let ivar = Fiber.Ivar.create () in
        (match Table.add conn.requests id (`Pending ivar) with
        | Ok () -> ()
        | Error _ -> Code_error.raise "duplicate id" [ ("id", Id.to_dyn id) ]);
        Ok ivar

    let request_untyped conn (id, req) =
      let* () = Fiber.return () in
      match prepare_request' conn (id, req) with
      | Error e -> Fiber.return (`Completed (Error e))
      | Ok ivar ->
        let* () = send conn (Some [ Request (id, req) ]) in
        Fiber.Ivar.read ivar

    let parse_response t decode = function
      | Error e -> Fiber.return (Error e)
      | Ok res -> (
        match decode res with
        | Ok s -> Fiber.return (Ok s)
        | Error e ->
          terminate_with_error t "response not matched by decl"
            [ ("e", Response.Error.to_dyn e) ])

    let gen_id t = function
      | Some id -> id
      | None ->
        let id = Sexp.List [ Atom "auto"; Atom (Int.to_string t.next_id) ] in
        t.next_id <- t.next_id + 1;
        Id.make id

    module Versioned = struct
      type ('a, 'b) request = ('a, 'b) Versioned.Staged.request

      type 'a notification = 'a Versioned.Staged.notification

      let prepare_request t (decl : _ Decl.Request.witness) =
        let+ handler = t.handler in
        V.Handler.prepare_request handler decl

      let prepare_notification (type a) t (decl : a Decl.Notification.witness) =
        let+ handler = t.handler in
        V.Handler.prepare_notification handler decl
    end

    let request ?id t ({ encode_req; decode_resp } : _ Versioned.request) req =
      let id = gen_id t id in
      let req = encode_req req in
      let* res = request_untyped t (id, req) in
      match res with
      | `Cancelled -> Fiber.return `Cancelled
      | `Completed res ->
        let+ res = parse_response t decode_resp res in
        `Completed res

    let cancel t id =
      match Table.find t.requests id with
      | None | Some `Cancelled -> Fiber.return ()
      | Some (`Pending ivar) ->
        Table.remove t.requests id;
        Fiber.Ivar.fill ivar `Cancelled

    let make_notification (type a) t ({ encode } : a Versioned.notification)
        (n : a) (k : Call.t -> 'a) : 'a =
      let call = encode n in
      match t.running with
      | true -> k call
      | false ->
        let err =
          let payload =
            Sexp.record
              [ ("method", Atom call.method_); ("params", call.params) ]
          in
          Response.Error.create ~payload
            ~message:"notification sent while connection is dead"
            ~kind:Code_error ()
        in
        raise (Response.Error.E err)

    let notification (type a) t (stg : a Versioned.notification) (n : a) =
      let* () = Fiber.return () in
      make_notification t stg n (fun call ->
          send t (Some [ Notification call ]))

    let disconnected t = Fiber.Ivar.read t.chan.disconnected

    module Stream = struct
      type nonrec 'a t =
        { poll : (Id.t, 'a option) Versioned.request
        ; cancel : Id.t Versioned.notification
        ; client : t
        ; id : Id.t
        ; mutable pending_request_id : Id.t option
        ; counter : int
        ; mutable active : bool
        }

      let create sub client id =
        let+ handler = client.handler in
        let open Result.O in
        let+ poll = V.Handler.prepare_request handler (Sub.poll sub)
        and+ cancel =
          V.Handler.prepare_notification handler (Sub.poll_cancel sub)
        in
        { poll
        ; cancel
        ; client
        ; id
        ; pending_request_id = None
        ; counter = 0
        ; active = true
        }

      let check_active t =
        if not t.active then
          Code_error.raise "polling is inactive" [ ("id", Id.to_dyn t.id) ]

      let next t =
        let* () = Fiber.return () in
        check_active t;
        (match t.pending_request_id with
        | Some _ ->
          Code_error.raise "Poll.next: previous Poll.next did not terminate yet"
            []
        | None -> ());
        let id =
          Sexp.record
            [ ("poll", Id.to_sexp t.id)
            ; ("i", Sexp.Atom (string_of_int t.counter))
            ]
          |> Id.make
        in
        t.pending_request_id <- Some id;
        let+ res = request ~id t.client t.poll t.id in
        t.pending_request_id <- None;
        match res with
        | `Cancelled -> None
        | `Completed (Ok res) -> res
        | `Completed (Error e) ->
          (* cwong: Should this really be a raise? *)
          raise (Response.Error.E e)

      let cancel t =
        let* () = Fiber.return () in
        check_active t;
        t.active <- false;
        (* XXX should we add a pool to stop waiting for the notification to
           reach the server? *)
        let notify () = notification t.client t.cancel t.id in
        match t.pending_request_id with
        | None -> notify ()
        | Some id ->
          Fiber.fork_and_join_unit (fun () -> cancel t.client id) notify
    end

    let no_cancel = function
      | `Cancelled -> assert false
      | `Completed s -> s

    let request ?id t spec req =
      let+ res = request ?id t spec req in
      no_cancel res

    let poll ?id client sub =
      let* () = Fiber.return () in
      let id = gen_id client id in
      Stream.create sub client id

    module Batch = struct
      type nonrec t =
        { client : t
        ; mutable pending : Packet.Query.t list
        }

      let create client = { client; pending = [] }

      let notification t n a =
        make_notification t.client n a (fun call ->
            t.pending <- Notification call :: t.pending)

      let request (type a b) ?id t
          ({ encode_req; decode_resp } : (a, b) Versioned.request) (req : a) :
          (b, _) result Fiber.t =
        let* () = Fiber.return () in
        let id = gen_id t.client id in
        let call = encode_req req in
        let ivar = prepare_request' t.client (id, call) in
        match ivar with
        | Error e -> Fiber.return (Error e)
        | Ok ivar -> (
          t.pending <- Packet.Query.Request (id, call) :: t.pending;
          let* res = Fiber.Ivar.read ivar in
          match res with
          | `Cancelled ->
            (* currently impossible because there's no batching for polling and
               cancellation is only available for polled requests *)
            assert false
          | `Completed res -> parse_response t.client decode_resp res)

      let submit t =
        let* () = Fiber.return () in
        let pending = List.rev t.pending in
        t.pending <- [];
        send t.client (Some pending)
    end

    let read_packets t packets =
      let* () =
        Fiber.parallel_iter packets ~f:(function
          | Packet.Reply.Notification n -> (
            if
              String.equal n.method_ Procedures.Server_side.abort.decl.method_
              && not t.handler_initialized
            then
              match
                Conv.of_sexp ~version:t.initialize.dune_version Message.sexp
                  n.params
              with
              | Ok msg -> t.on_preemptive_abort msg
              | Error _ ->
                Code_error.raise
                  "fatal: server aborted connection, but couldn't parse reason"
                  [ ("reason", Sexp.to_dyn n.params) ]
            else
              let* handler = t.handler in
              let* result = V.Handler.handle_notification handler () n in
              match result with
              | Error e ->
                terminate_with_error t "received bad notification from server"
                  [ ("error", Response.Error.to_dyn e)
                  ; ("notification", Call.to_dyn n)
                  ]
              | Ok () -> Fiber.return ())
          | Response (id, response) -> (
            match Table.find t.requests id with
            | Some status -> (
              Table.remove t.requests id;
              match status with
              | `Pending ivar -> Fiber.Ivar.fill ivar (`Completed response)
              | `Cancelled -> Fiber.return ())
            | None ->
              terminate_with_error t "unexpected response"
                [ ("id", Id.to_dyn id); ("response", Response.to_dyn response) ]
            ))
      in
      terminate t

    module Handler = struct
      type nonrec t =
        { log : Message.t -> unit Fiber.t
        ; abort : Message.t -> unit Fiber.t
        }

      let log { Message.payload; message } =
        let+ () = Fiber.return () in
        match payload with
        | None -> Format.eprintf "%s@." message
        | Some payload ->
          Format.eprintf "%s: %s@." message (Sexp.to_string payload)

      let abort m = raise (Abort (Server_aborted m))

      let default = { log; abort }

      let create ?log ?abort () =
        let t =
          let t = default in
          match log with
          | None -> t
          | Some log -> { t with log }
        in
        let t =
          match abort with
          | None -> t
          | Some abort -> { t with abort }
        in
        t
    end

    type proc =
      | Request : ('a, 'b) Decl.request -> proc
      | Notification : 'a Decl.notification -> proc
      | Poll : 'a Procedures.Poll.t -> proc

    let setup_versioning ?(private_menu = []) ~(handler : Handler.t) () =
      let open V in
      let t : _ Builder.t = Builder.create () in
      (* CR-soon cwong: It is a *huge* footgun that you have to remember to
         declare a request here, or via [private_menu], and there is no
         mechanism to warn you if you forget. The closest thing is either seeing
         that [dune rpc status] does not report the new procedure, or need to
         deal with the [Notification_error.t], which contains some good context,
         but very little to indicate this specific problem. *)
      Builder.declare_request t Procedures.Public.ping;
      Builder.declare_request t Procedures.Public.diagnostics;
      Builder.declare_notification t Procedures.Public.shutdown;
      Builder.declare_request t Procedures.Public.format_dune_file;
      Builder.declare_request t Procedures.Public.promote;
      Builder.declare_request t Procedures.Public.build_dir;
      Builder.implement_notification t Procedures.Server_side.abort (fun () ->
          handler.abort);
      Builder.implement_notification t Procedures.Server_side.log (fun () ->
          handler.log);
      Builder.declare_request t Procedures.Poll.(poll diagnostic);
      Builder.declare_request t Procedures.Poll.(poll progress);
      Builder.declare_notification t Procedures.Poll.(cancel diagnostic);
      Builder.declare_notification t Procedures.Poll.(cancel progress);
      List.iter
        ~f:(function
          | Request r -> Builder.declare_request t r
          | Notification n -> Builder.declare_notification t n
          | Poll p ->
            Builder.declare_request t (Procedures.Poll.poll p);
            Builder.declare_notification t (Procedures.Poll.cancel p))
        private_menu;
      t

    let connect_raw chan (initialize : Initialize.Request.t)
        ~(private_menu : proc list) ~(handler : Handler.t) ~f =
      let packets () =
        let+ read = Chan.read chan in
        Option.map read ~f:(fun sexp ->
            match
              Conv.of_sexp Packet.Reply.sexp ~version:initialize.dune_version
                sexp
            with
            | Error e -> raise (Abort (Invalid_session e))
            | Ok message -> message)
      in
      let builder = setup_versioning ~handler ~private_menu () in
      let on_preemptive_abort = handler.abort in
      let handler_var = Fiber.Ivar.create () in
      let handler = Fiber.Ivar.read handler_var in
      let client = create ~initialize ~chan ~handler ~on_preemptive_abort in
      let run () =
        let* init =
          let id = Id.make (List [ Atom "initialize" ]) in
          let initialize = Initialize.Request.to_call initialize in
          request_untyped client (id, initialize)
        in
        match no_cancel init with
        | Error e -> raise (Response.Error.E e)
        | Ok csexp ->
          let* menu =
            match
              Conv.of_sexp ~version:initialize.dune_version
                Initialize.Response.sexp csexp
            with
            | Error e -> raise (Abort (Invalid_session e))
            | Ok _resp -> (
              let id = Id.make (List [ Atom "version menu" ]) in
              let supported_versions =
                let request =
                  Version_negotiation.Request.create
                    (V.Builder.registered_procedures builder)
                in
                Version_negotiation.Request.to_call request
              in
              let+ resp = request_untyped client (id, supported_versions) in
              (* we don't allow cancelling negotiation *)
              match no_cancel resp with
              | Error e -> raise (Response.Error.E e)
              | Ok sexp -> (
                match
                  Conv.of_sexp ~version:initialize.dune_version
                    Version_negotiation.Response.sexp sexp
                with
                | Error e -> raise (Abort (Invalid_session e))
                | Ok (Selected methods) -> (
                  match Menu.of_list methods with
                  | Ok m -> m
                  | Error (method_, a, b) ->
                    Code_error.raise
                      "server responded with invalid version menu"
                      [ ( "duplicated"
                        , Dyn.Tuple [ Dyn.String method_; Dyn.Int a; Dyn.Int b ]
                        )
                      ])))
          in
          let handler =
            V.Builder.to_handler builder
              ~session_version:(fun () -> client.initialize.dune_version)
              ~menu
          in
          client.handler_initialized <- true;
          let* () = Fiber.Ivar.fill handler_var handler in
          Fiber.finalize
            (fun () -> f client)
            ~finally:(fun () -> Chan.write chan None)
      in
      Fiber.fork_and_join_unit (fun () -> read_packets client packets) run

    let connect_with_menu ?(handler = Handler.default) ~private_menu chan init
        ~f =
      connect_raw (Chan.of_chan chan) init ~handler ~private_menu ~f

    let connect = connect_with_menu ~private_menu:[]
  end
end
