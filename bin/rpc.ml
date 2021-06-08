open! Stdune
open Import

let wait_for_server common =
  match (Dune_rpc.Where.get (), Common.rpc common) with
  | None, None -> User_error.raise [ Pp.text "rpc server not running" ]
  | Some p, Some _ ->
    User_error.raise
      [ Pp.textf "cannot start rpc. It's already running at %s"
          (Dune_rpc.Where.to_string p)
      ]
  | Some w, None -> w
  | None, Some _ ->
    User_error.raise [ Pp.text "failed to establish rpc connection " ]

let client_term common f =
  let common = Common.set_print_directory common false in
  let config = Common.init common in
  Scheduler.go ~common ~config (fun () -> f common)

(* cwong: Should we put this into [dune-rpc]? *)
let interpret_kind = function
  | Dune_rpc_private.Response.Error.Invalid_request -> "Invalid_request"
  | Code_error -> "Code_error"
  | Version_error -> "Version_error"

let raise_rpc_error (e : Dune_rpc_private.Response.Error.t) =
  User_error.raise
    [ Pp.text "Server returned error: "
    ; Pp.textf "%s (error kind: %s)" e.message (interpret_kind e.kind)
    ]

module Init = struct
  let connect ~wait common =
    let open Fiber.O in
    let* client, session =
      let once () =
        let where = Dune_rpc.Where.get () in
        match where with
        | None -> Fiber.return None
        | Some where -> (
          let* client = Dune_rpc_impl.Run.Connect.csexp_client where in
          let+ session = Csexp_rpc.Client.connect client in
          match session with
          | Error _ -> None
          | Ok session -> Some (client, session))
      in
      let rec loop sleeper =
        let* res = once () in
        match res with
        | Some (client, session) ->
          (match sleeper with
          | None -> ()
          | Some s -> Scheduler.Worker.stop s);
          Fiber.return (client, session)
        | None ->
          let* sleeper = Scheduler.Worker.create () in
          let* () =
            Scheduler.Worker.task_exn sleeper ~f:(fun () -> Unix.sleepf 0.2)
          in
          loop (Some sleeper)
      in
      if wait then
        loop None
      else
        let+ res = once () in
        match res with
        | Some (client, session) -> (client, session)
        | None ->
          let (_ : Dune_rpc_private.Where.t) = wait_for_server common in
          User_error.raise
            [ Pp.text
                "failed to establish connection even though server seems to be \
                 running"
            ]
    in
    let* stdio = Csexp_rpc.Session.create ~socket:false stdin stdout in
    let forward f t =
      Fiber.repeat_while ~init:() ~f:(fun () ->
          let* read = Csexp_rpc.Session.read f in
          let+ () =
            Csexp_rpc.Session.write t (Option.map read ~f:List.singleton)
          in
          Option.map read ~f:(fun (_ : Sexp.t) -> ()))
    in
    Fiber.finalize
      (fun () ->
        Fiber.fork_and_join_unit
          (fun () -> forward session stdio)
          (fun () -> forward stdio session))
      ~finally:(fun () ->
        Csexp_rpc.Client.stop client;
        Fiber.return ())

  let term =
    let+ (common : Common.t) = Common.term
    and+ wait =
      let doc =
        "poll until server starts listening and then establish connection."
      in
      Arg.(value & flag & info [ "wait" ] ~doc)
    in
    client_term common (connect ~wait)

  let man = [ `Blocks Common.help_secs ]

  let doc = "establish a new rpc connection"

  let info = Term.info "init" ~doc ~man

  let term = (Term.Group.Term term, info)
end

module Status = struct
  let term =
    let+ (common : Common.t) = Common.term in
    client_term common @@ fun common ->
    let where = wait_for_server common in
    printfn "Server is listening on %s" (Dune_rpc.Where.to_string where);
    printfn "ID's of connected clients (include this one):";
    Dune_rpc_impl.Run.client where
      (Dune_rpc.Initialize.Request.create
         ~id:(Dune_rpc.Id.make (Sexp.Atom "status")))
      ~on_notification:(fun _ -> assert false)
      ~f:(fun session ->
        let open Fiber.O in
        let+ response =
          Dune_rpc_impl.Client.request session Dune_rpc_impl.Server.Decl.status
            ()
        in
        match response with
        | Error _ -> assert false
        (* TODO *)
        | Ok { clients } ->
          List.iter clients ~f:(fun client ->
              let sexp = Dune_rpc.Conv.to_sexp Dune_rpc.Id.sexp client in
              Sexp.to_string sexp |> print_endline))

  let info =
    let doc = "shot active connections" in
    Term.info "status" ~doc

  let term = (Term.Group.Term term, info)
end

module Ping = struct
  let send_ping cli =
    let open Fiber.O in
    let+ response =
      Dune_rpc_impl.Client.request cli Dune_rpc_private.Public.Request.ping ()
    in
    match response with
    | Ok () ->
      User_message.print
        (User_message.make
           [ Pp.text "Server appears to be responding normally" ])
    | Error e -> raise_rpc_error e

  let on_notification _ = Fiber.return ()

  let exec common =
    let where = wait_for_server common in
    Dune_rpc_impl.Run.client where
      (Dune_rpc_private.Initialize.Request.create
         ~id:(Dune_rpc_private.Id.make (Sexp.Atom "ping_cmd")))
      ~on_notification ~f:send_ping

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

let group = (Term.Group.Group [ Init.term; Status.term; Ping.term ], info)
