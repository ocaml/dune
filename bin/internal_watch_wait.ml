open Import
module Client = Dune_rpc_client.Client

let doc =
  "Run alongside `build --watch`, this command will exit after the nth rebuild \
   has completed"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Run this while dune is running in watch mode. This command takes an integer argument n and blocks until the nth rebuild has completed since dune was started. |}
  ; `Blocks Common.help_secs
  ]

let info = Cmd.info "watch-wait" ~doc ~man

let witness = Dune_rpc_private.Decl.Request.witness

let request_exn client witness n =
  let open Fiber.O in
  let* decl = Client.Versioned.prepare_request client witness in
  match decl with
  | Error e -> raise (Dune_rpc_private.Version_error.E e)
  | Ok decl -> Client.request client decl n

let run ~seqno =
  let open Fiber.O in
  let where = Rpc.active_server () in
  let* conn = Client.Connection.connect_exn where in
  Client.client conn
    ~private_menu:[ Request Dune_rpc_impl.Decl.watch_mode_event_long_poll ]
    (Dune_rpc.Initialize.Request.create
       ~id:(Dune_rpc.Id.make (Csexp.Atom "status")))
    ~f:(fun client ->
      let* _response =
        request_exn client
          (witness Dune_rpc_impl.Decl.watch_mode_event_long_poll)
          seqno
        >>| Result.get_ok
      in
      Fiber.return ())

let term =
  let+ (common : Common.t) = Common.term
  and+ seqno =
    Arg.(required & pos 0 (some int) None & Arg.info [] ~docv:"INT")
  in
  Rpc.client_term common @@ fun _common -> run ~seqno

let command = Cmd.v info term
