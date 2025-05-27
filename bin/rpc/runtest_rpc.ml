open! Import

let runtest ~wait ~dir_or_cram_test_paths =
  let open Fiber.O in
  let* connection = Rpc_common.establish_client_session ~wait in
  Dune_rpc_impl.Client.client
    connection
    (Dune_rpc.Initialize.Request.create ~id:(Dune_rpc.Id.make (Sexp.Atom "runtest")))
    ~f:(fun session ->
      Rpc_common.request_exn
        session
        (Dune_rpc_private.Decl.Request.witness Dune_rpc_impl.Decl.runtest)
        dir_or_cram_test_paths)
;;
