(** dune rpc command group *)
val group : unit Cmdliner.Cmd.t

module Build = Build
module Runtest_rpc = Runtest_rpc
