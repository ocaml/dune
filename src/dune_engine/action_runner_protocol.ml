open Import

module Request = struct
  module Exec = struct
    type response =
      | Completed of Process_runner.response
      | Cancelled

    type t =
      { run_id : Run_id.t
      ; process : Process_runner.request
      }
  end

  module Ready = struct
    type t = { name : Action_runner_name.t }
  end

  module Cancel_build = struct
    type t = { run_id : Run_id.t }
  end

  module Finish_build = struct
    type t = { run_id : Run_id.t }
  end
end

module Decl = struct
  module Conv = Dune_rpc.Conv
  module Decl = Dune_rpc.Decl

  let marshal () =
    let to_ data = Marshal.from_string data in
    let from value = Marshal.to_string value ~sharing:true in
    Conv.iso Conv.string to_ from
  ;;

  let make method_ ~resp =
    let v1 = Decl.Request.make_current_gen ~req:(marshal ()) ~resp ~version:1 in
    Decl.Request.make
      ~method_:(Dune_rpc.Method.Name.of_string ("action/" ^ method_))
      ~generations:[ v1 ]
  ;;

  let exec = make "exec" ~resp:(marshal ())
  let ready = make "ready" ~resp:Conv.unit
  let cancel_build = make "cancel-build" ~resp:Conv.unit
  let finish_build = make "finish-build" ~resp:Conv.unit
end
