open Import
module Client = Dune_rpc_client.Client

let active_server () =
  match Dune_rpc_impl.Where.get () with
  | Some p -> Ok p
  | None -> Error (User_error.make [ Pp.text "RPC server not running." ])
;;

let active_server_exn () = active_server () |> User_error.ok_exn

(* cwong: Should we put this into [dune-rpc]? *)
let interpret_kind = function
  | Dune_rpc_private.Response.Error.Invalid_request -> "Invalid_request"
  | Code_error -> "Code_error"
  | Connection_dead -> "Connection_dead"
;;

let raise_rpc_error (e : Dune_rpc_private.Response.Error.t) =
  User_error.raise
    [ Pp.text "Server returned error: "
    ; Pp.textf "%s (error kind: %s)" e.message (interpret_kind e.kind)
    ]
;;

let request_exn client witness n =
  let open Fiber.O in
  let* decl = Client.Versioned.prepare_request client witness in
  match decl with
  | Error e -> raise (Dune_rpc_private.Version_error.E e)
  | Ok decl -> Client.request client decl n
;;

let client_term builder f =
  let builder = Common.Builder.forbid_builds builder in
  let builder = Common.Builder.disable_log_file builder in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config f
;;

let wait_term =
  let doc = "poll until server starts listening and then establish connection." in
  Arg.(value & flag & info [ "wait" ] ~doc)
;;

let establish_connection () =
  let where = active_server () in
  match where with
  | Error e -> Fiber.return (Error e)
  | Ok where -> Client.Connection.connect where
;;

let establish_connection_exn () =
  let open Fiber.O in
  establish_connection () >>| User_error.ok_exn
;;

let establish_connection_with_retry () =
  let open Fiber.O in
  let pause_between_retries_s = 0.2 in
  let rec loop () =
    establish_connection ()
    >>= function
    | Ok x -> Fiber.return x
    | Error _ ->
      let* () = Scheduler.sleep ~seconds:pause_between_retries_s in
      loop ()
  in
  loop ()
;;

let establish_client_session ~wait =
  if wait then establish_connection_exn () else establish_connection_with_retry ()
;;
