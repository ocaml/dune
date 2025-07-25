open Import
module Client = Dune_rpc_client.Client

let active_server () =
  match Dune_rpc_impl.Where.get () with
  | Some p -> p
  | None -> User_error.raise [ Pp.text "RPC server not running." ]
;;

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

let retry_loop once =
  let open Fiber.O in
  let rec loop () =
    let* res = once () in
    match res with
    | Some result -> Fiber.return result
    | None ->
      let* () = Scheduler.sleep ~seconds:0.2 in
      loop ()
  in
  loop ()
;;

let establish_connection_or_raise ~wait once =
  let open Fiber.O in
  if wait
  then retry_loop once
  else
    let+ res = once () in
    match res with
    | Some conn -> conn
    | None ->
      let (_ : Dune_rpc_private.Where.t) = active_server () in
      User_error.raise
        [ Pp.text "failed to establish connection even though server seems to be running"
        ]
;;

let establish_client_session ~wait =
  let open Fiber.O in
  let once () =
    let where = Dune_rpc_impl.Where.get () in
    match where with
    | None -> Fiber.return None
    | Some where ->
      let+ connection = Client.Connection.connect where in
      (match connection with
       | Ok conn -> Some conn
       | Error message ->
         if not wait then Console.print_user_message message;
         None)
  in
  establish_connection_or_raise ~wait once
;;

let wait_term =
  let doc = "poll until server starts listening and then establish connection." in
  Arg.(value & flag & info [ "wait" ] ~doc)
;;
