open Import
module Client = Dune_rpc_client.Client
module Rpc_error = Dune_rpc.Response.Error

let active_server () =
  match Dune_rpc_impl.Where.get () with
  | Some p -> Ok p
  | None -> Error (User_error.make [ Pp.text "RPC server not running." ])
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
    [ Pp.text "Server returned error: "
    ; Pp.textf "%s (error kind: %s)" e.message (interpret_kind e.kind)
    ]
;;

let request_exn client witness n =
  let open Fiber.O in
  let* decl = Client.Versioned.prepare_request client witness in
  match decl with
  | Error e -> raise (Dune_rpc.Version_error.E e)
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
      let* () = Scheduler.sleep ~seconds:pause_between_retries_s in
      loop ()
  in
  loop ()
;;

let establish_client_session ~wait =
  if wait then establish_connection_with_retry () else establish_connection_exn ()
;;

let fire_request ~name ~wait request arg =
  let open Fiber.O in
  let* connection = establish_client_session ~wait in
  Dune_rpc_impl.Client.client
    connection
    (Dune_rpc.Initialize.Request.create ~id:(Dune_rpc.Id.make (Sexp.Atom name)))
    ~f:(fun client -> request_exn client (Dune_rpc.Decl.Request.witness request) arg)
;;

let wrap_build_outcome_exn ~print_on_success f args () =
  let open Fiber.O in
  let+ response = f args in
  match response with
  | Error (error : Rpc_error.t) ->
    Printf.eprintf "Error: %s\n%!" (Dyn.to_string (Rpc_error.to_dyn error))
  | Ok Dune_rpc.Build_outcome_with_diagnostics.Success ->
    if print_on_success
    then
      Console.print_user_message
        (User_message.make [ Pp.text "Success" |> Pp.tag User_message.Style.Success ])
  | Ok (Failure errors) ->
    List.iter errors ~f:(fun { Dune_rpc.Compound_user_error.main; _ } ->
      Console.print_user_message main);
    User_error.raise
      [ (match List.length errors with
         | 0 ->
           Code_error.raise
             "Build via RPC failed, but the RPC server did not send an error message."
             []
         | 1 -> Pp.textf "Build failed with 1 error."
         | n -> Pp.textf "Build failed with %d errors." n)
      ]
;;

let run_via_rpc ~builder ~common ~config lock_held_by f args =
  if not (Common.Builder.equal builder Common.Builder.default)
  then
    User_warning.emit
      [ Pp.textf
          "Your build request is being forwarded to a running Dune instance%s. Note that \
           certain command line arguments may be ignored."
          (match lock_held_by with
           | Dune_util.Global_lock.Lock_held_by.Unknown -> ""
           | Pid_from_lockfile pid -> sprintf " (pid: %d)" pid)
      ];
  Scheduler.go_without_rpc_server
    ~common
    ~config
    (wrap_build_outcome_exn ~print_on_success:true f args)
;;
