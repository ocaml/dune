open! Stdune
open Import

(* cwong: Should we put this into [dune-rpc]? *)
let interpret_kind = function
  | Dune_rpc_private.Response.Error.Invalid_request -> "Invalid_request"
  | Code_error -> "Code_error"
  | Version_error -> "Version_error"

let send_ping cli =
  let open Fiber.O in
  let+ response =
    Dune_rpc_impl.Client.request cli Dune_rpc_private.Public.Request.ping ()
  in
  match response with
  | Ok () ->
    User_message.print
      (User_message.make [ Pp.text "Server appears to be responding normally" ])
  | Error (e : Dune_rpc_private.Response.Error.t) ->
    User_error.raise
      [ Pp.text "Server returned error: "
      ; Pp.textf "%s (error kind: %s)" e.message (interpret_kind e.kind)
      ]

let on_notification _ = Fiber.return ()

let exec common =
  let where = Rpc.wait_for_server common in
  Dune_rpc_impl.Run.client where
    (Dune_rpc_private.Initialize.Request.create
       ~id:(Dune_rpc_private.Id.make (Sexp.Atom "ping_cmd")))
    ~on_notification ~f:send_ping

let info =
  let doc = "Ping the build server running in the current directory" in
  Term.info "ping" ~doc

let term =
  let+ (common : Common.t) = Common.term in
  Rpc.client_term common exec

let command = (term, info)
