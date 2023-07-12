open Import
module Client = Dune_rpc_client.Client

let active_server () =
  match Dune_rpc_impl.Where.get () with
  | Some p -> p
  | None -> User_error.raise [ Pp.text "RPC server not running." ]

(* cwong: Should we put this into [dune-rpc]? *)
let interpret_kind = function
  | Dune_rpc_private.Response.Error.Invalid_request -> "Invalid_request"
  | Code_error -> "Code_error"
  | Connection_dead -> "Connection_dead"

let raise_rpc_error (e : Dune_rpc_private.Response.Error.t) =
  User_error.raise
    [ Pp.text "Server returned error: "
    ; Pp.textf "%s (error kind: %s)" e.message (interpret_kind e.kind)
    ]

let request_exn client witness n =
  let open Fiber.O in
  let* decl = Client.Versioned.prepare_request client witness in
  match decl with
  | Error e -> raise (Dune_rpc_private.Version_error.E e)
  | Ok decl -> Client.request client decl n

let client_term common f =
  let common = Common.forbid_builds common in
  let config = Common.init ~log_file:No_log_file common in
  Scheduler.go ~common ~config f

let wait_term =
  let doc =
    "poll until server starts listening and then establish connection."
  in
  Arg.(value & flag & info [ "wait" ] ~doc)
