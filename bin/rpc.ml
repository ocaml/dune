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

module Init = struct
  let connect_persistent _common =
    let open Fiber.O in
    let* stdio = Csexp_rpc.Session.create stdin stdout in
    let* sessions, client = Dune_rpc_impl.Run.Connect.connect_persistent () in
    Fiber.Stream.In.sequential_iter sessions ~f:(fun session ->
        let connect =
          Dune_rpc.Conv.to_sexp Dune_rpc.Persistent.In.sexp New_connection
        in
        let* () = Csexp_rpc.Session.write stdio (Some connect) in
        let forward_to_stdout () =
          Fiber.repeat_while ~init:() ~f:(fun () ->
              let* read = Csexp_rpc.Session.read session in
              let packet =
                let packet =
                  match read with
                  | None -> Dune_rpc.Persistent.In.Close_connection
                  | Some s -> Packet s
                in
                Dune_rpc.Conv.to_sexp Dune_rpc.Persistent.In.sexp packet
              in
              let+ () = Csexp_rpc.Session.write stdio (Some packet) in
              Option.map read ~f:ignore)
        in
        let close = lazy (Csexp_rpc.Session.write session None) in
        let forward_from_stdin () =
          Fiber.repeat_while ~init:() ~f:(fun () ->
              let* read = Csexp_rpc.Session.read stdio in
              let packet =
                match read with
                | None -> None
                | Some p -> (
                  match
                    Dune_rpc.Conv.of_sexp Dune_rpc.Persistent.Out.sexp
                      ~version:(0, 0) p
                  with
                  | Error _ -> Code_error.raise "unexpected packet" []
                  | Ok s -> Some s)
              in
              match packet with
              | None
              | Some Close_connection ->
                let+ () = Lazy.force close in
                None
              | Some (Packet sexp) ->
                let+ () = Csexp_rpc.Session.write session (Some sexp) in
                Some ())
        in
        Fiber.finalize
          (fun () ->
            let+ res =
              Fiber.collect_errors (fun () ->
                  (* We want to close right away so we use with_error_handler *)
                  Fiber.with_error_handler
                    ~on_error:(fun exn ->
                      let+ () = Lazy.force close in
                      Dune_util.Report_error.report exn;
                      raise Dune_util.Report_error.Already_reported)
                    (fun () ->
                      Fiber.fork_and_join_unit forward_to_stdout
                        forward_from_stdin))
            in
            match res with
            | Ok () -> ()
            | Error _ -> ())
          ~finally:(fun () ->
            let+ () = Lazy.force close in
            Option.iter client ~f:Csexp_rpc.Client.stop;
            ()))

  let connect common =
    let where = wait_for_server common in
    let open Fiber.O in
    let* c = Dune_rpc_impl.Run.Connect.csexp_client where in
    let* session = Csexp_rpc.Client.connect c in
    let* stdio = Csexp_rpc.Session.create stdin stdout in
    let forward f t =
      Fiber.repeat_while ~init:() ~f:(fun () ->
          let* read = Csexp_rpc.Session.read f in
          let+ () = Csexp_rpc.Session.write t read in
          Option.map read ~f:(fun (_ : Sexp.t) -> ()))
    in
    Fiber.finalize
      (fun () ->
        Fiber.fork_and_join_unit
          (fun () -> forward session stdio)
          (fun () -> forward stdio session))
      ~finally:(fun () ->
        Csexp_rpc.Client.stop c;
        Fiber.return ())

  let term =
    let+ (common : Common.t) = Common.term
    and+ persistent =
      let doc = "Wait for build servers and automatically reconnect." in
      Arg.(value & flag & info [ "persistent" ] ~doc)
    in
    if persistent then
      client_term common connect_persistent
    else
      client_term common connect

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

let info =
  let doc = "Dune's RPC mechanism. Experimental." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|This is experimental. do not use|}
    ; `Blocks Common.help_secs
    ]
  in
  Term.info "rpc" ~doc ~man

let group = (Term.Group.Group [ Init.term; Status.term ], info)
