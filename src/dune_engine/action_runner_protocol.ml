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

  module Exec = struct
    let decl =
      let v1 =
        Decl.Request.make_current_gen ~req:(marshal ()) ~resp:(marshal ()) ~version:1
      in
      Decl.Request.make
        ~method_:(Dune_rpc.Method.Name.of_string "action/exec")
        ~generations:[ v1 ]
    ;;
  end

  module Ready = struct
    let decl =
      let v1 =
        Decl.Request.make_current_gen ~req:(marshal ()) ~resp:Conv.unit ~version:1
      in
      Decl.Request.make
        ~method_:(Dune_rpc.Method.Name.of_string "action/ready")
        ~generations:[ v1 ]
    ;;
  end

  module Cancel_build = struct
    let decl =
      let v1 =
        Decl.Request.make_current_gen ~req:(marshal ()) ~resp:Conv.unit ~version:1
      in
      Decl.Request.make
        ~method_:(Dune_rpc.Method.Name.of_string "action/cancel-build")
        ~generations:[ v1 ]
    ;;
  end

  module Finish_build = struct
    let decl =
      let v1 =
        Decl.Request.make_current_gen ~req:(marshal ()) ~resp:Conv.unit ~version:1
      in
      Decl.Request.make
        ~method_:(Dune_rpc.Method.Name.of_string "action/finish-build")
        ~generations:[ v1 ]
    ;;
  end

  let exec = Exec.decl
  let ready = Ready.decl
  let cancel_build = Cancel_build.decl
  let finish_build = Finish_build.decl
end
