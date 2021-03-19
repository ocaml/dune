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
    let until = Unix.time () +. 1.0 in
    let rec loop () =
      if Unix.time () > until then
        User_error.raise [ Pp.text "failed to establish rpc connection " ]
      else
        match Dune_rpc.Where.get () with
        | Some w -> w
        | None ->
          Unix.sleepf 0.3;
          loop ()
    in
    loop ()

let client_term common f =
  let common = Common.set_print_directory common false in
  let config = Common.init common in
  Scheduler.go ~common ~config (fun () ->
      let open Fiber.O in
      let* csexp_scheduler = Scheduler.csexp_scheduler () in
      let run =
        let stats = Common.stats common in
        Dune_rpc_impl.Run.of_config Client csexp_scheduler stats
      in
      f common run)

module Init = struct
  let connect_persistent _common run_config =
    let open Fiber.O in
    let stdio = Dune_rpc_impl.Run.csexp_connect run_config stdin stdout in
    let where_file = Dune_rpc_impl.Run.client_address () in
    let server =
      let where =
        if Sys.win32 then
          let addr = Unix.inet_addr_of_string "0.0.0.0" in
          `Ip (addr, `Port 0)
        else
          `Unix (Path.build where_file)
      in
      Dune_rpc_impl.Run.csexp_server run_config where
    in
    let* listen_sessions =
      let+ res = Csexp_rpc.Server.serve server in
      (match Csexp_rpc.Server.listening_address server with
      | ADDR_UNIX _ -> ()
      | ADDR_INET (addr, port) ->
        let where = `Ip (addr, `Port port) in
        Io.write_file (Path.build where_file) (Dune_rpc.Where.to_string where));
      res
    in
    let client =
      Dune_rpc.Where.get ()
      |> Option.map ~f:(Dune_rpc_impl.Run.csexp_client run_config)
    in
    (* The combined sessions are the one that we established ourselves + the
       remaining sessions later servers will establish by connecting to the
       client *)
    let* sessions =
      match client with
      | None -> Fiber.return listen_sessions
      | Some c ->
        let+ session = Csexp_rpc.Client.connect c in
        Fiber.Stream.In.cons session listen_sessions
    in
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
                let+ () = Csexp_rpc.Session.write session None in
                None
              | Some (Packet sexp) ->
                let+ () = Csexp_rpc.Session.write session (Some sexp) in
                Some ())
        in
        Fiber.finalize
          (fun () ->
            Fiber.fork_and_join_unit forward_to_stdout forward_from_stdin)
          ~finally:(fun () ->
            Option.iter client ~f:Csexp_rpc.Client.stop;
            Fiber.return ()))

  let connect common run =
    let where = wait_for_server common in
    let c = Dune_rpc_impl.Run.csexp_client run where in
    let open Fiber.O in
    let* session = Csexp_rpc.Client.connect c in
    let stdio = Dune_rpc_impl.Run.csexp_connect run stdin stdout in
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
    client_term common @@ fun common run ->
    let where = wait_for_server common in
    printfn "Server is listening on %s" (Dune_rpc.Where.to_string where);
    printfn "ID's of connected clients (include this one):";
    Dune_rpc_impl.Run.client run where
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
