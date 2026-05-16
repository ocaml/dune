open Stdune
module Csexp_rpc = Rpc.Csexp_rpc
open Csexp_rpc
open Fiber.O
open Dune_scheduler

let () = Dune_tests_common.init ()

type event =
  | Fill of Fiber.fill
  | Abort

let server (where : Unix.sockaddr) =
  (match where with
   | ADDR_UNIX p ->
     Fpath.unlink_no_err p;
     let p = Path.of_string p in
     Path.mkdir_p (Path.parent_exn p)
   | _ -> ());
  match Server.create [ where ] ~backlog:10 with
  | Ok t -> t
  | Error `Already_in_use -> assert false
;;

let client where = Csexp_rpc.Client.create where

let create_server_exn addr =
  match Server.create [ addr ] ~backlog:10 with
  | Ok server -> server
  | Error `Already_in_use -> failwith "address already in use"
;;

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

let scheduler_config =
  { Scheduler.Config.concurrency = 1
  ; print_ctrl_c_warning = false
  ; watch_exclusions = []
  }
;;

let run_scheduler f =
  Dune_engine.Clflags.display := Quiet;
  Scheduler.Run.go scheduler_config f ~on_event:(fun _ -> ())
;;

let stop_server server = run_scheduler (fun () -> Server.stop server)

let temp_rpc_dir () =
  Temp.temp_dir ~parent_dir:(Path.of_string ".") ~prefix:"test" ~suffix:"dune_rpc"
;;

let%expect_test "csexp server create on unix sockets" =
  (let tmp_dir = temp_rpc_dir () in
   let addr : Unix.sockaddr =
     Unix.ADDR_UNIX (Path.to_string (Path.relative tmp_dir "dunerpc.sock"))
   in
   let server = server addr in
   stop_server server);
  [%expect {| |}]
;;

let%expect_test "csexp server stop before serve releases address" =
  (let tmp_dir = temp_rpc_dir () in
   let path = Path.to_string (Path.relative tmp_dir "dunerpc.sock") in
   let addr = Unix.ADDR_UNIX path in
   Fpath.unlink_no_err path;
   let run () =
     let server = create_server_exn addr in
     let* () = Server.stop server in
     let replacement = create_server_exn addr in
     Server.stop replacement
   in
   run_scheduler run);
  [%expect {| |}]
;;

let%expect_test "csexp server stop before consuming serve stream releases address" =
  let tmp_dir = temp_rpc_dir () in
  let path = Path.to_string (Path.relative tmp_dir "dunerpc.sock") in
  let addr = Unix.ADDR_UNIX path in
  Fpath.unlink_no_err path;
  let run () =
    let server = create_server_exn addr in
    let* _sessions = Server.serve server in
    let* () = Server.stop server in
    let replacement = create_server_exn addr in
    Server.stop replacement
  in
  run_scheduler run;
  [%expect {| |}]
;;

let%expect_test "csexp server create cleans up partial binds" =
  let tmp_dir = temp_rpc_dir () in
  let path1 = Path.to_string (Path.relative tmp_dir "dunerpc-1.sock") in
  let path2 = Path.to_string (Path.relative tmp_dir "dunerpc-2.sock") in
  let addr1 = Unix.ADDR_UNIX path1 in
  let addr2 = Unix.ADDR_UNIX path2 in
  Fpath.unlink_no_err path1;
  Fpath.unlink_no_err path2;
  let run () =
    let busy = create_server_exn addr2 in
    let* () =
      match Server.create [ addr1; addr2 ] ~backlog:10 with
      | Ok _ -> failwith "expected address conflict"
      | Error `Already_in_use -> Fiber.return ()
    in
    match create_server_exn addr1 with
    | recovered ->
      Fiber.fork_and_join_unit
        (fun () -> Server.stop busy)
        (fun () -> Server.stop recovered)
    | exception Failure msg ->
      let+ () = Server.stop busy in
      printfn "Error: exception Failure(%S)" msg
  in
  run_scheduler run;
  [%expect {| |}]
;;

let%expect_test "csexp server stop before serve removes unix socket" =
  let tmp_dir = temp_rpc_dir () in
  let path = Path.to_string (Path.relative tmp_dir "dunerpc.sock") in
  let addr = Unix.ADDR_UNIX path in
  let server = server addr in
  run_scheduler (fun () -> Server.stop server);
  printfn "%b" (Fpath.exists path);
  [%expect {| false |}]
;;

let write_raw addr data =
  let fd = Unix.socket ~cloexec:true (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.connect fd addr;
  let (_ : int) = Unix.single_write_substring fd data 0 (String.length data) in
  fd
;;

let%expect_test "csexp session reports malformed eof" =
  let tmp_dir = temp_rpc_dir () in
  let path = Path.to_string (Path.relative tmp_dir "dunerpc.sock") in
  let addr = Unix.ADDR_UNIX path in
  let run () =
    let accepted = Fiber.Ivar.create () in
    let server = server addr in
    let* sessions = Server.serve server in
    Fiber.fork_and_join_unit
      (fun () ->
         let fd = write_raw addr "5:abc" in
         let+ () = Fiber.Ivar.read accepted in
         Unix.close fd)
      (fun () ->
         Fiber.Stream.In.parallel_iter sessions ~f:(fun session ->
           let* () = Fiber.Ivar.fill accepted () in
           let* () =
             Session.read session
             >>| function
             | None -> printfn "server: read returned None"
             | Some sexp -> printfn "server: received %s" (Csexp.to_string sexp)
           in
           Server.stop server))
  in
  let old_verbose = !Log.verbose in
  Exn.protect
    ~f:(fun () ->
      Log.verbose := true;
      run_scheduler run)
    ~finally:(fun () -> Log.verbose := old_verbose);
  Fpath.unlink_no_err path;
  [%expect
    {|
    Warning: malformed csexp rpc packet id: 0 error: "Csexp.Make(Sexp).Parser.Parse_error(\"premature end of input\")"
    server: read returned None
    |}]
;;

let%expect_test "csexp server life cycle" =
  let tmp_dir = temp_rpc_dir () in
  let addr : Unix.sockaddr =
    ADDR_UNIX (Path.to_string (Path.relative tmp_dir "dunerpc.sock"))
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
  run_scheduler run;
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

let%expect_test "csexp server create cleans up after partial tcp bind failure" =
  let fd_count () = Sys.readdir "/dev/fd" |> Array.length in
  let busy = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
  Unix.bind busy (ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen busy 10;
  let busy_addr = Unix.getsockname busy in
  Exn.protect
    ~f:(fun () ->
      let run () =
        let before = fd_count () in
        let* () =
          match
            Server.create
              [ ADDR_INET (Unix.inet_addr_loopback, 0); busy_addr ]
              ~backlog:10
          with
          | Error `Already_in_use -> Fiber.return ()
          | Ok server ->
            let* () = Server.stop server in
            failwith "expected address conflict"
        in
        let after = fd_count () in
        printfn "leaked fds: %d" (after - before);
        Fiber.return ()
      in
      run_scheduler run)
    ~finally:(fun () -> Unix.close busy);
  [%expect {| leaked fds: 0 |}]
;;

let%expect_test "csexp server stop rejects new connections" =
  let tmp_dir = temp_rpc_dir () in
  let addr : Unix.sockaddr =
    ADDR_UNIX (Path.to_string (Path.relative tmp_dir "dunerpc.sock"))
  in
  let run () =
    let server = server addr in
    let* sessions = Server.serve server in
    let listening_address = Csexp_rpc.Server.listening_address server |> List.hd in
    Fiber.fork_and_join_unit
      (fun () ->
         let* () = Server.stop server in
         let client = client listening_address in
         let* res = Client.connect client in
         let* outcome =
           match res with
           | Error _ ->
             Client.stop client;
             Fiber.return "connect failed"
           | Ok session ->
             let+ () = Session.close session in
             "connected"
         in
         printfn "%s" outcome;
         Fiber.return ())
      (fun () -> Fiber.Stream.In.parallel_iter sessions ~f:(fun _ -> Fiber.return ()))
  in
  run_scheduler run;
  [%expect {| connect failed |}]
;;
