open Import
open Fiber.O

include
  Dune_rpc.Client.Make
    (Private.Fiber)
    (struct
      include Csexp_rpc.Session

      let write t = function
        | None -> close t
        | Some packets ->
          write t packets
          >>| (function
           | Ok () -> ()
           | Error `Closed -> raise Dune_util.Report_error.Already_reported)
      ;;
    end)

type chan = Csexp_rpc.Session.t

module Connection = struct
  type t = Csexp_rpc.Session.t

  let connect where =
    let sock = Where.to_socket where in
    let client = Csexp_rpc.Client.create sock in
    let+ res = Csexp_rpc.Client.connect client in
    match res with
    | Ok s -> Ok s
    | Error exn ->
      Error
        (User_error.make
           [ Pp.textf "failed to connect to RPC server %s" (Where.to_string where)
           ; Exn_with_backtrace.pp exn
           ])
  ;;

  let connect_exn where =
    let+ conn = connect where in
    User_error.ok_exn conn
  ;;
end

let client ?handler ~private_menu connection init ~f =
  let f client =
    Fiber.finalize
      (fun () -> f client)
      ~finally:(fun () -> Csexp_rpc.Session.close connection)
  in
  connect_with_menu ?handler ~private_menu connection init ~f
;;
