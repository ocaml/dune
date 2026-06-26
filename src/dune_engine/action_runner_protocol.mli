open Import

module Request : sig
  module Exec : sig
    type response =
      | Completed of Process_runner.response
      | Cancelled

    type t =
      { run_id : Run_id.t
      ; process : Process_runner.request
      }
  end

  module Ready : sig
    type t = { name : Action_runner_name.t }
  end

  module Cancel_build : sig
    type t = { run_id : Run_id.t }
  end

  module Finish_build : sig
    type t = { run_id : Run_id.t }
  end
end

module Decl : sig
  val exec : (Request.Exec.t, Request.Exec.response) Dune_rpc.Decl.Request.t
  val ready : (Request.Ready.t, unit) Dune_rpc.Decl.Request.t
  val cancel_build : (Request.Cancel_build.t, unit) Dune_rpc.Decl.Request.t
  val finish_build : (Request.Finish_build.t, unit) Dune_rpc.Decl.Request.t
end
