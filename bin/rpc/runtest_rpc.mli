open! Import

(** Sends a request to run the specified tests on the RPC server at [where]. *)
val runtest
  :  wait:bool
  -> dir_or_cram_test_paths:string list
  -> ( Dune_rpc_impl.Decl.Build_outcome_with_diagnostics.t
       , Dune_rpc.Response.Error.t )
       result
       Fiber.t
