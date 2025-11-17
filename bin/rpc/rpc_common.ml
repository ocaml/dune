open Import
module Client = Dune_rpc_client.Client
module Rpc_error = Dune_rpc.Response.Error

let active_server () =
  match Dune_rpc_impl.Where.get () with
  | Some p -> Ok p
  | None -> Error (User_error.make [ Pp.paragraph "RPC server not running." ])
;;

let active_server_exn () = active_server () |> User_error.ok_exn

(* cwong: Should we put this into [dune-rpc]? *)
let interpret_kind = function
  | Rpc_error.Invalid_request -> "Invalid_request"
  | Code_error -> "Code_error"
  | Connection_dead -> "Connection_dead"
;;

let raise_rpc_error (e : Rpc_error.t) =
  User_error.raise
    [ Pp.paragraph "Server returned error: "
    ; Pp.paragraphf "%s (error kind: %s)" e.message (interpret_kind e.kind)
      (* CR-soon ElectreAAS: Should we be printing the payload? *)
    ]
;;

let request_exn client request arg =
  let open Fiber.O in
  let* decl =
    Client.Versioned.prepare_request client (Dune_rpc.Decl.Request.witness request)
  in
  match decl with
  | Ok decl ->
    Client.request client decl arg
    >>| (function
     | Ok response -> response
     | Error e -> raise_rpc_error e)
  | Error e -> raise (Dune_rpc.Version_error.E e)
;;

let notify_exn client notification arg =
  let open Fiber.O in
  let* res =
    Client.Versioned.prepare_notification
      client
      (Dune_rpc.Decl.Notification.witness notification)
  in
  match res with
  | Ok decl -> Client.notification client decl arg
  | Error e -> raise (Dune_rpc.Version_error.E e)
;;

let client_term builder f =
  let builder = Common.Builder.forbid_builds builder in
  let builder = Common.Builder.disable_log_file builder in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config f
;;

let wait_term =
  let doc = "Poll until server starts listening and then establish connection." in
  Arg.(value & flag & info [ "wait" ] ~doc:(Some doc))
;;

let establish_connection () =
  match active_server () with
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
      let* () = Dune_engine.Scheduler.sleep ~seconds:pause_between_retries_s in
      loop ()
  in
  loop ()
;;

let establish_client_session ~wait =
  if wait then establish_connection_with_retry () else establish_connection_exn ()
;;

let prepare_targets targets =
  List.map targets ~f:(fun target ->
    let sexp = Dune_lang.Dep_conf.encode target in
    Dune_lang.to_string sexp)
;;

let warn_ignore_arguments lock_held_by =
  User_warning.emit
    [ Pp.paragraphf
        "Your build request is being forwarded to a running Dune instance%s. Note that \
         certain command line arguments may be ignored."
        (match lock_held_by with
         | Dune_util.Global_lock.Lock_held_by.Unknown -> ""
         | Pid_from_lockfile pid -> sprintf " (pid: %d)" pid)
    ]
;;

let should_warn ~warn_forwarding builder =
  warn_forwarding && not (Common.Builder.equal builder Common.Builder.default)
;;

let send_request ~f connection name =
  Dune_rpc_impl.Client.client
    connection
    (Dune_rpc.Initialize.Request.create ~id:(Dune_rpc.Id.make (Sexp.Atom name)))
    ~f
;;

let fire_request
      ~name
      ~wait
      ?(warn_forwarding = true)
      ?(lock_held_by = Dune_util.Global_lock.Lock_held_by.Unknown)
      builder
      request
      arg
  =
  let open Fiber.O in
  let* connection = establish_client_session ~wait in
  if should_warn ~warn_forwarding builder then warn_ignore_arguments lock_held_by;
  send_request connection name ~f:(fun client -> request_exn client request arg)
;;

let fire_notification
      ~name
      ~wait
      ?(warn_forwarding = true)
      ?(lock_held_by = Dune_util.Global_lock.Lock_held_by.Unknown)
      builder
      notification
      arg
  =
  let open Fiber.O in
  let* connection = establish_client_session ~wait in
  if should_warn ~warn_forwarding builder then warn_ignore_arguments lock_held_by;
  send_request connection name ~f:(fun client -> notify_exn client notification arg)
;;

let wrap_build_outcome_exn ~print_on_success build_outcome =
  match build_outcome with
  | Dune_rpc.Build_outcome_with_diagnostics.Success ->
    if print_on_success
    then Console.print [ Pp.text "Success" |> Pp.tag User_message.Style.Success ]
  | Failure errors ->
    let error_msg =
      match List.length errors with
      | 0 ->
        Code_error.raise
          "Build via RPC failed, but the RPC server did not send an error message."
          []
      | 1 -> Pp.paragraph "Build failed with 1 error."
      | n -> Pp.paragraphf "Build failed with %d errors." n
    in
    List.iter errors ~f:(fun { Dune_rpc.Compound_user_error.main; _ } ->
      Console.print_user_message main);
    User_error.raise [ error_msg |> Pp.tag User_message.Style.Error ]
;;
