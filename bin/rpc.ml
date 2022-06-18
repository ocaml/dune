open! Stdune
open Import

let active_server () =
  match Dune_rpc_impl.Where.get () with
  | Some p -> p
  | None -> User_error.raise [ Pp.text "rpc server not running" ]

let client_term common f =
  let common = Common.set_print_directory common false in
  let config = Common.init ~log_file:No_log_file common in
  Scheduler.go ~common ~config f

(* cwong: Should we put this into [dune-rpc]? *)
let interpret_kind = function
  | Dune_rpc_private.Response.Error.Invalid_request -> "Invalid_request"
  | Code_error -> "Code_error"

let raise_rpc_error (e : Dune_rpc_private.Response.Error.t) =
  User_error.raise
    [ Pp.text "Server returned error: "
    ; Pp.textf "%s (error kind: %s)" e.message (interpret_kind e.kind)
    ]

let request_exn client witness n =
  let open Fiber.O in
  let* decl = Dune_rpc_impl.Client.Versioned.prepare_request client witness in
  match decl with
  | Error e -> raise (Dune_rpc_private.Version_error.E e)
  | Ok decl -> Dune_rpc_impl.Client.request client decl n

let retry_loop once =
  let open Fiber.O in
  let rec loop () =
    let* res = once () in
    match res with
    | Some result -> Fiber.return result
    | None ->
      let* () = Scheduler.sleep 0.2 in
      loop ()
  in
  loop ()

let establish_connection_or_raise ~wait once =
  let open Fiber.O in
  if wait then retry_loop once
  else
    let+ res = once () in
    match res with
    | Some conn -> conn
    | None ->
      let (_ : Dune_rpc_private.Where.t) = active_server () in
      User_error.raise
        [ Pp.text
            "failed to establish connection even though server seems to be \
             running"
        ]

let wait_term =
  let doc =
    "poll until server starts listening and then establish connection."
  in
  Arg.(value & flag & info [ "wait" ] ~doc)

let establish_client_session ~wait =
  let open Fiber.O in
  let once () =
    let where = Dune_rpc_impl.Where.get () in
    match where with
    | None -> Fiber.return None
    | Some where -> (
      let+ connection = Dune_rpc_impl.Client.Connection.connect where in
      match connection with
      | Ok conn -> Some conn
      | Error message ->
        Console.print_user_message message;
        None)
  in
  establish_connection_or_raise ~wait once

let report_error error =
  Printf.printf "Error: %s\n%!"
    (Dyn.to_string (Dune_rpc_private.Response.Error.to_dyn error))

let witness = Dune_rpc_private.Decl.Request.witness

module Status = struct
  let term =
    let+ (common : Common.t) = Common.term in
    client_term common @@ fun _common ->
    let where = active_server () in
    printfn "Server is listening on %s" (Dune_rpc.Where.to_string where);
    printfn "Connected clients (including this one):\n";
    let open Fiber.O in
    let* conn = Dune_rpc_impl.Client.Connection.connect_exn where in
    Dune_rpc_impl.Client.client conn
      (Dune_rpc.Initialize.Request.create
         ~id:(Dune_rpc.Id.make (Sexp.Atom "status")))
      ~f:(fun session ->
        let open Fiber.O in
        let+ response =
          request_exn session (witness Dune_rpc_impl.Decl.status) ()
        in
        match response with
        | Error error -> report_error error
        | Ok { clients } ->
          List.iter clients ~f:(fun (client, menu) ->
              let id =
                let sexp = Dune_rpc.Conv.to_sexp Dune_rpc.Id.sexp client in
                Sexp.to_string sexp
              in
              let message =
                match (menu : Dune_rpc_impl.Decl.Status.Menu.t) with
                | Uninitialized ->
                  User_message.make
                    [ Pp.textf "Client [%s], conducting version negotiation" id
                    ]
                | Menu menu ->
                  User_message.make
                    [ Pp.box ~indent:2
                        (Pp.concat ~sep:Pp.newline
                           (Pp.textf
                              "Client [%s] with the following RPC versions:" id
                           :: List.map menu ~f:(fun (method_, version) ->
                                  Pp.textf "%s: %d" method_ version)))
                    ]
              in
              User_message.print message))

  let info =
    let doc = "show active connections" in
    Term.info "status" ~doc

  let term = (Term.Group.Term term, info)
end

module Build = struct
  let term =
    let name_ = Arg.info [] ~docv:"TARGET" in
    let+ (common : Common.t) = Common.term
    and+ wait = wait_term
    and+ targets = Arg.(value & pos_all string [] name_) in
    client_term common @@ fun _common ->
    let open Fiber.O in
    let* conn = establish_client_session ~wait in
    Dune_rpc_impl.Client.client conn
      (Dune_rpc.Initialize.Request.create
         ~id:(Dune_rpc.Id.make (Sexp.Atom "build")))
      ~f:(fun session ->
        let open Fiber.O in
        let+ response =
          request_exn session (witness Dune_rpc_impl.Decl.build) targets
        in
        match response with
        | Error (error : Dune_rpc_private.Response.Error.t) ->
          report_error error
        | Ok Success -> print_endline "Success"
        | Ok Failure -> print_endline "Failure")

  let info =
    let doc =
      "build a given target (requires dune to be running in passive watching \
       mode)"
    in
    Term.info "build" ~doc

  let term = (Term.Group.Term term, info)
end

module Ping = struct
  let send_ping cli =
    let open Fiber.O in
    let+ response = request_exn cli Dune_rpc_private.Public.Request.ping () in
    match response with
    | Ok () ->
      User_message.print
        (User_message.make
           [ Pp.text "Server appears to be responding normally" ])
    | Error e -> raise_rpc_error e

  let exec common =
    let open Fiber.O in
    let where = active_server common in
    let* conn = Dune_rpc_impl.Client.Connection.connect_exn where in
    Dune_rpc_impl.Client.client conn ~f:send_ping
      (Dune_rpc_private.Initialize.Request.create
         ~id:(Dune_rpc_private.Id.make (Sexp.Atom "ping_cmd")))

  let info =
    let doc = "Ping the build server running in the current directory" in
    Term.info "ping" ~doc

  let term =
    let+ (common : Common.t) = Common.term in
    client_term common exec

  let term = (Term.Group.Term term, info)
end

let info =
  let doc = "Dune's RPC mechanism. Experimental." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|This is experimental. do not use|}
    ; `Blocks Common.help_secs
    ]
  in
  Term.info "rpc" ~doc ~man

let group = (Term.Group.Group [ Status.term; Build.term; Ping.term ], info)
