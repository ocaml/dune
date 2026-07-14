open Import
open Fiber.O

include
  Dune_rpc.Client.Make
    (Private.Fiber)
    (struct
      include Csexp_rpc.Session

      let write t packets =
        write t packets
        >>| function
        | Ok () -> ()
        | Error `Closed -> raise Dune_util.Report_error.Already_reported
      ;;
    end)

module Connection = struct
  type t = Csexp_rpc.Session.t

  let connect_sock sock = Csexp_rpc.Client.create sock |> Csexp_rpc.Client.connect

  let of_fd fd =
    match Csexp_rpc.Session.of_fd fd with
    | fd -> fd
    | exception exn ->
      User_error.raise [ Pp.text "failed to use RPC file descriptor"; Exn.pp exn ]
  ;;

  let connect where =
    match Where.to_socket where with
    | exception exn ->
      Fiber.return
        (Error
           (User_error.make
              [ Pp.textf "failed to connect to RPC server %s" (Where.to_string where)
              ; Exn.pp exn
              ]))
    | sock ->
      connect_sock sock
      >>| (function
       | Ok s -> Ok s
       | Error exn ->
         Error
           (User_error.make
              [ Pp.textf "failed to connect to RPC server %s" (Where.to_string where)
              ; Exn_with_backtrace.pp exn
              ]))
  ;;

  let connect_exn where = connect where >>| User_error.ok_exn

  let is_retryable_connect_error { Exn_with_backtrace.exn; _ } =
    match exn with
    | Unix.Unix_error ((Unix.ENOENT | Unix.ECONNREFUSED), "connect", _) -> true
    | _ -> false
  ;;

  let connect_retrying_exn where ~retry_delay =
    let sock = Where.to_socket where in
    let rec loop () =
      connect_sock sock
      >>= function
      | Ok connection -> Fiber.return connection
      | Error exn when is_retryable_connect_error exn ->
        let* () = Dune_scheduler.Scheduler.sleep retry_delay in
        loop ()
      | Error exn -> Exn_with_backtrace.reraise exn
    in
    loop ()
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
