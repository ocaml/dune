open Import
module Client = Dune_rpc_client.Client

let doc =
  "When dune is running in watch-mode this command prints out the number of \
   builds that have completed since dune started"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|When dune is running in watch-mode this command prints out the number of builds that have completed since dune started|}
  ; `Blocks Common.help_secs
  ]

let info = Cmd.info "build-count" ~doc ~man

let witness = Dune_rpc_private.Decl.Request.witness

let request_exn client witness =
  let open Fiber.O in
  let* decl = Client.Versioned.prepare_request client witness in
  match decl with
  | Error e -> raise (Dune_rpc_private.Version_error.E e)
  | Ok decl -> Client.request client decl ()

let run () =
  let open Fiber.O in
  let where = Rpc.active_server () in
  let* conn = Client.Connection.connect_exn where in
  Client.client conn
    ~private_menu:[ Request Dune_rpc_impl.Decl.build_count ]
    (Dune_rpc.Initialize.Request.create
       ~id:(Dune_rpc.Id.make (Csexp.Atom "status ")))
    ~f:(fun client ->
      let* count =
        request_exn client (witness Dune_rpc_impl.Decl.build_count)
        >>| Result.get_ok
      in
      print_endline (Printf.sprintf "%d" count);
      Fiber.return ())

let term =
  let+ (common : Common.t) = Common.term in
  Rpc.client_term common @@ fun _common -> run ()

let command = Cmd.v info term
