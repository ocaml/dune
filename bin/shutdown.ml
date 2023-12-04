open Import
module Client = Dune_rpc_client.Client

let send_shutdown cli =
  let open Fiber.O in
  let* decl =
    Client.Versioned.prepare_notification
      cli
      Dune_rpc_private.Public.Notification.shutdown
  in
  match decl with
  | Ok decl -> Client.notification cli decl ()
  | Error e -> raise (Dune_rpc_private.Version_error.E e)
;;

let exec common =
  let open Fiber.O in
  let where = Rpc_common.active_server common in
  let* conn = Client.Connection.connect_exn where in
  Dune_rpc_impl.Client.client
    conn
    ~f:send_shutdown
    (Dune_rpc_private.Initialize.Request.create
       ~id:(Dune_rpc_private.Id.make (Sexp.Atom "shutdown_cmd")))
;;

let info =
  let doc = "Cancel and shutdown any builds in the current workspace." in
  Cmd.info "shutdown" ~doc
;;

let term =
  let+ builder = Common.Builder.term in
  Rpc_common.client_term builder exec
;;

let command = Cmd.v info term
