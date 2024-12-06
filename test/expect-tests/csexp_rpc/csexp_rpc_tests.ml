open! Stdune
open Csexp_rpc
open Fiber.O
module Scheduler = Dune_engine.Scheduler

let () = Dune_tests_common.init ()

type event =
  | Fill of Fiber.fill
  | Abort

let server (where : Unix.sockaddr) =
  (match where with
   | ADDR_UNIX p ->
     let p = Path.of_string p in
     Path.unlink_no_err p;
     Path.mkdir_p (Path.parent_exn p)
   | _ -> ());
  match Server.create [ where ] ~backlog:10 with
  | Ok t -> t
  | Error `Already_in_use -> assert false
;;

let client where = Csexp_rpc.Client.create where

module Logger = struct
  (* A little helper to make the output from the client and server
     deterministic. Log messages are batched and outputted at the end. *)
  type t =
    { mutable messages : string list
    ; name : string
    }

  let create ~name = { messages = []; name }
  let log t fmt = Printf.ksprintf (fun m -> t.messages <- m :: t.messages) fmt

  let print { messages; name } =
    List.rev messages |> List.iter ~f:(fun msg -> printfn "%s: %s" name msg)
  ;;
end

let ok_exn = function
  | Ok s -> s
  | Error `Closed -> failwith "closed"
  | Error (`Exn exn) -> raise exn
;;

let%expect_test "csexp server life cycle" =
  let tmp_dir = Temp.create Dir ~prefix:"test" ~suffix:"dune_rpc" in
  let addr : Unix.sockaddr =
    if Sys.win32
    then ADDR_INET (Unix.inet_addr_loopback, 0)
    else ADDR_UNIX (Path.to_string (Path.relative tmp_dir "dunerpc.sock"))
  in
  let client_log = Logger.create ~name:"client" in
  let server_log = Logger.create ~name:"server" in
  let run () =
    let server = server addr in
    let* sessions = Server.serve server in
    let client = Csexp_rpc.Server.listening_address server |> List.hd |> client in
    Fiber.fork_and_join_unit
      (fun () ->
         let log fmt = Logger.log client_log fmt in
         let* client = Client.connect_exn client in
         let* () = Session.write client [ List [ Atom "from client" ] ] >>| ok_exn in
         log "written";
         let* response = Session.read client in
         (match response with
          | None -> log "no response"
          | Some sexp -> log "received %s" (Csexp.to_string sexp));
         let* () = Session.close client in
         log "closed";
         Server.stop server)
      (fun () ->
         let log fmt = Logger.log server_log fmt in
         let+ () =
           Fiber.Stream.In.parallel_iter sessions ~f:(fun session ->
             log "received session";
             let* res = Csexp_rpc.Session.read session in
             match res with
             | None ->
               log "session terminated";
               Fiber.return ()
             | Some csexp ->
               log "received %s" (Csexp.to_string csexp);
               Session.write session [ List [ Atom "from server" ] ] >>| ok_exn)
         in
         log "sessions finished")
  in
  Dune_engine.Clflags.display := Quiet;
  let config =
    { Scheduler.Config.concurrency = 1
    ; stats = None
    ; print_ctrl_c_warning = false
    ; watch_exclusions = []
    }
  in
  Scheduler.Run.go config run ~on_event:(fun _ _ -> ());
  Logger.print client_log;
  Logger.print server_log;
  [%expect
    {|
    client: written
    client: received (11:from server)
    client: closed
    server: received session
    server: received (11:from client)
    server: sessions finished |}]
;;
