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
  Common.set_common common ~targets:[] ~external_lib_deps_mode:false;
  Scheduler.go ~common (fun () ->
      let where = wait_for_server common in
      f where)

module Init = struct
  let connect where =
    let c = Dune_engine.Scheduler.Rpc.csexp_client where in
    let open Fiber.O in
    let* session = Csexp_rpc.Client.connect c in
    let stdio = Dune_engine.Scheduler.Rpc.csexp_connect stdin stdout in
    let forward f t =
      Fiber.repeat_while ~init:() ~f:(fun () ->
          let* read = Csexp_rpc.Session.read f in
          let+ () = Csexp_rpc.Session.write t read in
          Option.map read ~f:(fun (_ : Sexp.t) -> ()))
    in
    Fiber.fork_and_join_unit
      (fun () -> forward session stdio)
      (fun () -> forward stdio session)

  let term =
    let+ (common : Common.t) = Common.term in
    client_term common (fun where ->
        let open Fiber.O in
        let* () = connect where in
        Dune_engine.Scheduler.Rpc.stop ())

  let man = [ `Blocks Common.help_secs ]

  let doc = "establish a new rpc connection"

  let info = Term.info "init" ~doc ~man

  let term = (Term.Group.Term term, info)
end

module Test = struct
  let rec to_dune_lang (s : Sexp.t) : Dune_lang.t =
    match s with
    | Atom s -> Dune_lang.atom_or_quoted_string s
    | List s -> List (List.map ~f:to_dune_lang s)

  let rec of_dune_lang (s : Dune_lang.t) : Sexp.t =
    match s with
    | Atom s -> Atom (Dune_lang.Atom.to_string s)
    | Quoted_string s -> Atom s
    | List s -> List (List.map ~f:of_dune_lang s)
    | Template _ -> Atom (Dune_lang.to_string s)

  let connect where =
    let lexbuf = Lexing.from_channel stdin in
    let input =
      Dune_lang.Parser.parse ~mode:Many lexbuf
      |> List.map ~f:(fun ast ->
             let sexp = Dune_lang.Ast.remove_locs ast in
             of_dune_lang sexp)
    in
    let c = Dune_engine.Scheduler.Rpc.csexp_client where in
    let open Fiber.O in
    let* session = Csexp_rpc.Client.connect c in
    let i () =
      Fiber.repeat_while ~init:input ~f:(function
        | [] ->
          let+ () = Csexp_rpc.Session.write session None in
          None
        | x :: xs ->
          let+ () = Csexp_rpc.Session.write session (Some x) in
          Some xs)
    in
    let o () =
      Fiber.repeat_while ~init:() ~f:(fun () ->
          let+ read = Csexp_rpc.Session.read session in
          match read with
          | None -> None
          | Some sexp ->
            let sexp = to_dune_lang sexp in
            printfn "%s\n" (Dune_lang.to_string sexp);
            Some ())
    in
    Fiber.fork_and_join_unit i o

  let term =
    let+ (common : Common.t) = Common.term in
    let common = Common.set_rpc common (Dune_rpc_impl.Server.create ()) in
    client_term common (fun where ->
        let open Fiber.O in
        let* () = connect where in
        Dune_engine.Scheduler.Rpc.stop ())

  let man =
    [ `S "DESCRIPTION"
    ; `P "This is for internal use only"
    ; `Blocks Common.help_secs
    ]

  let doc = "test dune rpc with a series of requests"

  let info = Term.info "test" ~doc ~man

  let term = (Term.Group.Term term, info)
end

module Status = struct
  let term =
    let+ (common : Common.t) = Common.term in
    client_term common @@ fun where ->
    printfn "Server is listening on %s" (Dune_rpc.Where.to_string where);
    printfn "ID's of connected clients (include this one):";
    Scheduler.Rpc.client where
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

let group = (Term.Group.Group [ Init.term; Test.term; Status.term ], info)
