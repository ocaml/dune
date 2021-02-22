open Stdune
open Dune_rpc_private

module Session = struct
  type 'a state =
    | Uninitialized
    | Initialized of
        { init : Initialize.Request.t
        ; state : 'a
        ; closed : bool
        }

  module Id = Stdune.Id.Make ()

  type 'a t =
    { messages : Message.t Fiber.Stream.In.t
    ; id : Id.t
    ; send : Packet.t option -> unit Fiber.t
    ; mutable state : 'a state
    }

  let set t state =
    match t.state with
    | Initialized s -> t.state <- Initialized { s with state }
    | Uninitialized -> Code_error.raise "set: state not available" []

  let get t =
    match t.state with
    | Initialized s -> s.state
    | Uninitialized -> Code_error.raise "get: state not available" []

  let active t =
    match t.state with
    | Uninitialized -> true
    | Initialized s -> s.closed

  let initialize t =
    match t.state with
    | Initialized s -> s.init
    | Uninitialized -> Code_error.raise "initialize: request not available" []

  let create ~messages ~send =
    { messages; send; state = Uninitialized; id = Id.gen () }

  let notification t (decl : _ Decl.notification) n =
    let call =
      { Call.params = Conv.to_sexp decl.req n; method_ = decl.method_ }
    in
    t.send (Some (Notification call))

  let close t =
    match t.state with
    | Uninitialized -> assert false
    | Initialized s -> t.state <- Initialized { s with closed = true }

  let compare x y = Id.compare x.id y.id

  let dyn_of_state f =
    let open Dyn.Encoder in
    function
    | Uninitialized -> constr "Uninitialized" []
    | Initialized { init; state; closed } ->
      let record =
        record
          [ ("init", opaque init); ("state", f state); ("closed", bool closed) ]
      in
      constr "Initialized" [ record ]

  let to_dyn f { id; state; messages = _; send = _ } =
    let open Dyn.Encoder in
    record [ ("id", Id.to_dyn id); ("state", dyn_of_state f state) ]
end

module H = struct
  type 'a t =
    { on_request : 'a Session.t -> Request.t -> Response.t Fiber.t
    ; on_notification : 'a Session.t -> Call.t -> unit Fiber.t
    ; on_init : 'a Session.t -> Initialize.Request.t -> 'a Fiber.t
    ; on_terminate : 'a Session.t -> unit Fiber.t
    ; version : int * int
    }

  let handle (type a) (t : a t) (session : a Session.t) =
    let open Fiber.O in
    let* message = Fiber.Stream.In.read session.messages in
    match message with
    | None -> session.send None
    | Some init -> (
      let id, call =
        match init with
        | Notification _ ->
          (* TODO handle invalid request here *)
          assert false
        | Request (id, call) -> (id, call)
      in
      match Initialize.Request.of_call ~version:t.version call with
      | Error e -> session.send (Some (Response (id, Error e)))
      | Ok init ->
        if Initialize.Request.version init > t.version then
          let response =
            let payload =
              Sexp.record
                [ ( "supported versions until"
                  , Conv.to_sexp Version.sexp t.version )
                ]
            in
            Error
              (Response.Error.create ~payload ~kind:Version_error
                 ~message:"Unsupported version" ())
          in
          session.send (Some (Response (id, response)))
        else
          let* a = t.on_init session init in
          let () =
            session.state <- Initialized { init; state = a; closed = false }
          in
          let* () =
            let response =
              Ok
                (Initialize.Response.to_response
                   (Initialize.Response.create ()))
            in
            session.send (Some (Response (id, response)))
          in
          let* () =
            Fiber.Stream.In.parallel_iter session.messages
              ~f:(fun (message : Message.t) ->
                match message with
                | Notification n -> t.on_notification session n
                | Request (id, r) ->
                  let* response = t.on_request session (id, r) in
                  session.send (Some (Response (id, response))))
          in
          let* () = session.send None in
          let+ () = t.on_terminate session in
          Session.close session )

  module Builder = struct
    type info =
      | Private
      | Public of
          { since : int * int
          ; until : (int * int) option
          }

    let private_ = Private

    let public ?until ~since () = Public { since; until }

    type ('state, 'req, 'resp) callback =
      { info : info
      ; f : 'state Session.t -> 'req -> 'resp Fiber.t
      }

    let callback' info f = { info; f }

    let callback info f =
      let f _ x = f x in
      { info; f }

    type 's n_handler =
      | N : ('s, 'a, unit) callback * 'a Decl.notification -> 's n_handler

    type 's r_handler =
      | R : ('s, 'a, 'b) callback * ('a, 'b) Decl.request -> 's r_handler

    type 's t =
      { mutable notification_handlers : 's n_handler list
      ; mutable request_handlers : 's r_handler list
      ; on_init : 's Session.t -> Initialize.Request.t -> 's Fiber.t
      ; on_terminate : 's Session.t -> unit Fiber.t
      ; version : int * int
      }

    let to_handler
        { on_init
        ; notification_handlers
        ; request_handlers
        ; version
        ; on_terminate
        } =
      let request_handlers, notification_handlers =
        let r = Table.create (module String) 16 in
        let n = Table.create (module String) 16 in
        List.iter notification_handlers ~f:(fun (N (cb, decl)) ->
            Table.Multi.cons n decl.method_ (N (cb, decl)));
        List.iter request_handlers ~f:(fun (R (cb, decl)) ->
            Table.Multi.cons r decl.method_ (R (cb, decl)));
        (* TODO This all needs proper validation. It shouldn't be possible for
           the same methods to overlap versions *)
        (r, n)
      in
      let find_cb cbs ~version ~info =
        List.find cbs ~f:(fun cb ->
            match info cb with
            | Private -> true
            | Public { since; until } -> (
              version >= since
              &&
              match until with
              | None -> true
              | Some until -> version <= until ))
      in
      let no_method_version_error ~method_ ~infos:_ =
        let payload = Sexp.record [ ("method", Atom method_) ] in
        Response.Error.create ~kind:Version_error
          ~message:"no method matching this client version" ~payload ()
      in
      let on_notification session (n : Call.t) =
        let version = Initialize.Request.version (Session.initialize session) in
        match Table.find notification_handlers n.method_ with
        | None ->
          Session.notification session Server_notifications.log
            { Log.message = "invalid notification"
            ; payload = Some (Sexp.record [ ("payload", Atom n.method_) ])
            }
        | Some [] -> assert false (* not possible *)
        | Some cbs -> (
          (* TODO extract version from session *)
          match find_cb cbs ~info:(fun (N (cb, _)) -> cb.info) ~version with
          | None ->
            (* XXX log *)
            Fiber.return ()
          | Some (N (cb, v)) -> (
            match Conv.of_sexp v.req ~version n.params with
            | Error _ -> (* XXX shall we log? *) Fiber.return ()
            | Ok v -> cb.f session v ) )
      in
      let on_request session (_id, (n : Call.t)) =
        let version = Initialize.Request.version (Session.initialize session) in
        match Table.find request_handlers n.method_ with
        | None ->
          let payload = Sexp.record [ ("method", Atom n.method_) ] in
          Fiber.return
            (Error
               (Response.Error.create ~kind:Invalid_request
                  ~message:"invalid method" ~payload ()))
        | Some [] -> assert false (* not possible *)
        | Some cbs -> (
          match find_cb cbs ~version ~info:(fun (R (cb, _)) -> cb.info) with
          | None ->
            Fiber.return
              (Error (no_method_version_error ~method_:n.method_ ~infos:cbs))
          | Some (R (cb, decl)) -> (
            match Conv.of_sexp decl.req ~version n.params with
            | Error e -> Fiber.return (Error (Response.Error.of_conv e))
            | Ok input -> (
              let open Fiber.O in
              let+ r = Fiber.collect_errors (fun () -> cb.f session input) in
              match r with
              | Ok s -> Ok (Conv.to_sexp decl.resp s)
              | Error
                  [ { Exn_with_backtrace.exn = Response.Error.E e
                    ; backtrace = _
                    }
                  ] ->
                Error e
              | Error xs ->
                let payload =
                  Sexp.List
                    (List.map xs ~f:(fun x ->
                         Exn_with_backtrace.to_dyn x |> Sexp.of_dyn))
                in
                Error
                  (Response.Error.create ~kind:Code_error
                     ~message:"server error" ~payload ()) ) ) )
      in
      { on_request; on_notification; on_init; version; on_terminate }

    let create ?(on_terminate = fun _ -> Fiber.return ()) ~on_init ~version () =
      { request_handlers = []
      ; notification_handlers = []
      ; on_init
      ; version
      ; on_terminate
      }

    let request (t : _ t) cb decl =
      t.request_handlers <- R (cb, decl) :: t.request_handlers

    let notification (t : _ t) cb decl =
      t.notification_handlers <- N (cb, decl) :: t.notification_handlers
  end
end

type t = Server : 'a H.t -> t

let make h = Server (H.Builder.to_handler h)

let version (Server h) = h.version

let new_session (Server handler) ~messages ~send =
  let session = Session.create ~messages ~send in
  H.handle handler session

exception Invalid_session of Conv.error

let create_sequence f ~version conv =
  let read () =
    let open Fiber.O in
    let+ read = f () in
    Option.map read ~f:(fun sexp ->
        match Conv.of_sexp conv ~version sexp with
        | Error e -> raise (Invalid_session e)
        | Ok message -> message)
  in
  Fiber.Stream.In.create read

module Make (S : sig
  type t

  val write : t -> Sexp.t option -> unit Fiber.t

  val read : t -> Sexp.t option Fiber.t
end) =
struct
  open Fiber.O

  let serve sessions server =
    Fiber.Stream.In.parallel_iter sessions ~f:(fun session ->
        let+ res =
          Fiber.collect_errors (fun () ->
              let send packet =
                Option.map packet ~f:(Conv.to_sexp Packet.sexp)
                |> S.write session
              in
              let messages =
                create_sequence
                  (fun () -> S.read session)
                  ~version:(version server) Message.sexp
              in
              new_session server ~send ~messages)
        in
        match res with
        | Ok () -> ()
        | Error _ ->
          (* XXX shall we log this? *)
          ())
end

module Handler = H.Builder
