open Import
open Fiber.O
include Dune_rpc.Client.Make (Private.Fiber) (Csexp_rpc.Session)

type chan = Csexp_rpc.Session.t

module Connection = struct
  type t = Csexp_rpc.Session.t

  let connect where =
    let sock = Where.to_socket where in
    let* client = Csexp_rpc.Client.create sock in
    let+ res = Csexp_rpc.Client.connect client in
    match res with
    | Ok s -> Ok s
    | Error exn ->
      Error
        (User_error.make
           [ Pp.text "failed to connect to RPC server %s"
           ; Exn_with_backtrace.pp exn
           ])

  let connect_exn where =
    let+ conn = connect where in
    match conn with
    | Ok s -> s
    | Error msg -> raise (User_error.E msg)
end

let client ?handler connection init ~f =
  let f client =
    Fiber.finalize
      (fun () -> f client)
      ~finally:(fun () -> Csexp_rpc.Session.write connection None)
  in
  connect_with_menu ?handler
    ~private_menu:[ Request Decl.build; Request Decl.status ]
    connection init ~f
