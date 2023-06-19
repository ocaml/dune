open! Stdune
open Import
module Client = Dune_rpc_client.Client

let active_server () =
  match Dune_rpc_impl.Where.get () with
  | Some p -> p
  | None -> User_error.raise [ Pp.text "rpc server not running" ]

let client_term common f =
  let common = Common.forbid_builds common in
  let config = Common.init ~log_file:No_log_file common in
  Scheduler.go ~common ~config f

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
      let+ connection = Client.Connection.connect where in
      match connection with
      | Ok conn -> Some conn
      | Error message ->
        if not wait then Console.print_user_message message;
        None)
  in
  establish_connection_or_raise ~wait once

let report_error error =
  Printf.printf "Error: %s\n%!"
    (Dyn.to_string (Dune_rpc_private.Response.Error.to_dyn error))

let witness = Dune_rpc_private.Decl.Request.witness

module Status = struct
  let ( let** ) x f =
    let open Fiber.O in
    let* x = x in
    match x with
    | Ok s -> f s
    | Error e -> Fiber.return (Error e)

  let ( let++ ) x f =
    let open Fiber.O in
    let+ x = x in
    match x with
    | Ok s -> Ok (f s)
    | Error e -> Error e

  (** Get the status of a server at a given location and apply a function to the
      list of clients *)
  let server_response_map ~where ~f =
    (* TODO: add timeout for status check *)
    let open Fiber.O in
    let** conn =
      Client.Connection.connect where
      >>| Result.map_error ~f:User_message.to_string
    in
    Dune_rpc_impl.Client.client conn
      (Dune_rpc.Initialize.Request.create
         ~id:(Dune_rpc.Id.make (Sexp.Atom "status")))
      ~f:(fun session ->
        let open Fiber.O in
        let++ response =
          let** decl =
            Client.Versioned.prepare_request session
              (witness Dune_rpc_impl.Decl.status)
            >>| Result.map_error ~f:Dune_rpc_private.Version_error.message
          in
          Client.request session decl ()
          >>| Result.map_error ~f:Dune_rpc.Response.Error.message
        in
        f response.Dune_rpc_impl.Decl.Status.clients)

  (** Get a list of registered Dunes from the RPC registry *)
  let registered_dunes () : Dune_rpc.Registry.Dune.t list Fiber.t =
    let config =
      Dune_rpc_private.Registry.Config.create (Lazy.force Dune_util.xdg)
    in
    let registry = Dune_rpc_private.Registry.create config in
    let open Fiber.O in
    let+ _result = Dune_rpc_impl.Poll_active.poll registry in
    Dune_rpc_private.Registry.current registry

  (** The type of server statuses *)
  type status =
    { root : string
    ; pid : Pid.t
    ; result : (int, string) result
    }

  (** Fetch the status of a single Dune instance *)
  let get_status (dune : Dune_rpc.Registry.Dune.t) =
    let root = Dune_rpc_private.Registry.Dune.root dune in
    let pid = Dune_rpc_private.Registry.Dune.pid dune |> Pid.of_int in
    let where = Dune_rpc_private.Registry.Dune.where dune in
    let open Fiber.O in
    let+ result = server_response_map ~where ~f:List.length in
    { root; pid; result }

  (** Print a list of statuses to the console *)
  let print_statuses statuses =
    List.sort statuses ~compare:(fun x y -> String.compare x.root y.root)
    |> Pp.concat_map ~sep:Pp.newline ~f:(fun { root; pid; result } ->
           Pp.concat ~sep:Pp.newline
             [ Pp.textf "root: %s" root
             ; Pp.enumerate ~f:Fun.id
                 [ Pp.textf "pid: %d" (Pid.to_int pid)
                 ; Pp.textf "clients: %s"
                     (match result with
                     | Ok n -> string_of_int n
                     | Error e -> e)
                 ]
             ])
    |> List.singleton |> Console.print

  let term =
    let+ (common : Common.t) = Common.term
    and+ all =
      Arg.(
        value & flag
        & info [ "all" ]
            ~doc:
              "Show all running Dune instances together with their root, pids \
               and number of clients.")
    in
    client_term common @@ fun () ->
    let open Fiber.O in
    if all then
      let* dunes = registered_dunes () in
      let+ statuses = Fiber.parallel_map ~f:get_status dunes in
      print_statuses statuses
    else
      let where = active_server () in
      Console.print
        [ Pp.textf "Server is listening on %s" (Dune_rpc.Where.to_string where)
        ; Pp.text "Connected clients (including this one):"
        ];
      server_response_map ~where ~f:(fun clients ->
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
              Console.print_user_message message))
      >>| function
      | Ok () -> ()
      | Error e -> Printf.printf "Error: %s\n" e

  let info =
    let doc = "show active connections" in
    Cmd.info "status" ~doc

  let cmd = Cmd.v info term
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
    Cmd.info "build" ~doc

  let cmd = Cmd.v info term
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
    let* conn = Client.Connection.connect_exn where in
    Dune_rpc_impl.Client.client conn ~f:send_ping
      (Dune_rpc_private.Initialize.Request.create
         ~id:(Dune_rpc_private.Id.make (Sexp.Atom "ping_cmd")))

  let info =
    let doc = "Ping the build server running in the current directory" in
    Cmd.info "ping" ~doc

  let term =
    let+ (common : Common.t) = Common.term in
    client_term common exec

  let cmd = Cmd.v info term
end

let info =
  let doc = "Dune's RPC mechanism. Experimental." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|This is experimental. do not use|}
    ; `Blocks Common.help_secs
    ]
  in
  Cmd.info "rpc" ~doc ~man

let group = Cmd.group info [ Status.cmd; Build.cmd; Ping.cmd ]
