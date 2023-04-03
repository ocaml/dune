open Import
module Client = Dune_rpc_client.Client

let doc =
  "Run alongside `build --watch`, this command will exit after the next \
   rebuild has completed"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Run this while dune is running in watch mode. This command blocks until the next rebuild has completed. |}
  ; `Blocks Common.help_secs
  ]

let info = Cmd.info "watch-wait" ~doc ~man

let witness = Dune_rpc_private.Decl.Request.witness

let request_exn client witness =
  let open Fiber.O in
  let* decl = Client.Versioned.prepare_request client witness in
  match decl with
  | Error e -> raise (Dune_rpc_private.Version_error.E e)
  | Ok decl -> Client.request client decl ()

let run () =
  let open Fiber.O in
  let where = Rpc.active_server () in
  let* conn = Client.Connection.connect_exn where in
  Client.client conn
    ~private_menu:[ Request Dune_rpc_impl.Decl.watch_mode_wait ]
    (Dune_rpc.Initialize.Request.create
       ~id:(Dune_rpc.Id.make (Csexp.Atom "status")))
    ~f:(fun client ->
      let* () =
        request_exn client (witness Dune_rpc_impl.Decl.watch_mode_wait)
        >>| Result.get_ok
      in
      Fiber.return ())

let term =
  let+ (common : Common.t) = Common.term in
  Rpc.client_term common @@ fun _common -> run ()

let command = Cmd.v info term
