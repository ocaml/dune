open! Stdune
open Import
module Client = Dune_rpc_impl.Client

let send_shutdown cli =
  let open Fiber.O in
  let* decl =
    Client.Versioned.prepare_notification cli
      Dune_rpc_private.Public.Notification.shutdown
  in
  match decl with
  | Ok decl -> Dune_rpc_impl.Client.notification cli decl ()
  | Error e -> raise (Dune_rpc_private.Version_error.E e)

let exec common =
  let open Fiber.O in
  let where = Rpc.active_server common in
  let* conn = Dune_rpc_impl.Client.Connection.connect_exn where in
  Dune_rpc_impl.Client.client conn ~f:send_shutdown
    (Dune_rpc_private.Initialize.Request.create
       ~id:(Dune_rpc_private.Id.make (Sexp.Atom "shutdown_cmd")))

let info =
  let doc = "cancel and shutdown any builds in the current workspace" in
  Term.info "shutdown" ~doc

let term =
  let+ (common : Common.t) = Common.term in
  Rpc.client_term common exec

let command = (term, info)
