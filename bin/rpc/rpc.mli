(** dune rpc command group *)
val group : unit Cmdliner.Cmd.t

module Build = Rpc_build
module Runtest_rpc = Runtest_rpc
