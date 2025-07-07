open Import
module Client = Dune_rpc_client.Client

let send_ping cli =
  let open Fiber.O in
  let+ response = Rpc_common.request_exn cli Dune_rpc_private.Public.Request.ping () in
  match response with
  | Ok () -> Console.print [ Pp.text "Server appears to be responding normally" ]
  | Error e -> Rpc_common.raise_rpc_error e
;;

let exec common =
  let open Fiber.O in
  let where = Rpc_common.active_server common in
  let* conn = Client.Connection.connect_exn where in
  Dune_rpc_impl.Client.client
    conn
    ~f:send_ping
    (Dune_rpc_private.Initialize.Request.create
       ~id:(Dune_rpc_private.Id.make (Sexp.Atom "ping_cmd")))
;;

let info =
  let doc = "Ping the build server running in the current directory" in
  Cmd.info "ping" ~doc
;;

let term =
  let+ (builder : Common.Builder.t) = Common.Builder.term in
  Rpc_common.client_term builder exec
;;

let cmd = Cmd.v info term
