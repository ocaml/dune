(* end to end tests of dune_rpc. Verify that whatever is exposed to the client
   is usable *)

open Stdune
open Lwt.Syntax
open Dune_rpc.V1
open Dune_rpc_lwt.V1

let connect ~root_dir ~persistent =
  let args = [| "dune"; "rpc"; "init"; "--root"; root_dir |] in
  let args =
    if persistent then
      Array.append args [| "--persistent" |]
    else
      args
  in
  Lwt_process.open_process ("dune", args)

let build_watch ~root_dir =
  Lwt_process.open_process_none
    ( "dune"
    , [| "dune"
       ; "build"
       ; "--no-print-directory"
       ; "--root"
       ; root_dir
       ; "-w"
       ; "--file-watcher"
       ; "manual"
       ; "@install"
      |] )

let run_with_timeout f =
  Lwt.catch
    (fun () ->
      let+ () =
        Lwt_unix.with_timeout 3.0 (fun () ->
            let+ _ = f () in
            ())
      in
      print_endline "success")
    (fun exn ->
      (match exn with
      | Lwt_unix.Timeout -> print_endline "timeout"
      | _ -> ());
      Lwt.return_unit)

let%expect_test "run and connect" =
  let initialize = Initialize.create ~id:(Id.make (Csexp.Atom "test")) in
  Lwt_main.run
    (let* root_dir = Lwt_io.create_temp_dir () in
     let build = build_watch ~root_dir in
     let rpc =
       let+ () = Lwt_unix.sleep 0.5 in
       connect ~root_dir ~persistent:false
     in
     let run_client =
       let* rpc = rpc in
       let chan = (rpc#stdout, rpc#stdin) in
       Client.connect chan initialize ~f:(fun t ->
           print_endline "started session";
           let* res = Client.request t Request.ping () in
           match res with
           | Error _ -> failwith "unexpected"
           | Ok () ->
             print_endline "received ping. shutting down.";
             Client.notification t Notification.shutdown ())
     in
     let run_rpc =
       let* rpc = rpc in
       let+ res = rpc#status in
       match res with
       | WEXITED i -> printfn "rpc init finished with %i" i
       | _ -> assert false
     in
     let run_build =
       let* _ = run_rpc in
       let+ res = build#status in
       match res with
       | WEXITED i -> printfn "dune build finished with %i" i
       | _ -> assert false
     in
     Lwt.finalize
       (fun () ->
         run_with_timeout (fun () -> Lwt.all [ run_client; run_rpc; run_build ]))
       (fun () ->
         let+ rpc = rpc in
         rpc#terminate;
         build#terminate));
  [%expect
    {|
    started session
    received ping. shutting down.
    rpc init finished with 0
    dune build finished with 1
    success |}]

let%expect_test "run and connect persistent" =
  let test =
    let* root_dir = Lwt_io.create_temp_dir () in
    let build () = build_watch ~root_dir in
    let build1 =
      print_endline "build1: connecting";
      build ()
    in
    let build2 =
      let+ _ = build1#status in
      print_endline "build2: connecting";
      build ()
    in
    let rpc =
      let+ () = Lwt_unix.sleep 0.5 in
      connect ~root_dir ~persistent:true
    in
    let run_rpc =
      let* rpc = rpc in
      let+ res = rpc#status in
      match res with
      | WEXITED i -> printfn "rpc init finished with %i" i
      | _ -> assert false
    in
    let run_client =
      let* rpc = rpc in
      let chan = (rpc#stdout, rpc#stdin) in
      let count = ref 0 in
      let on_connect () =
        incr count;
        printfn "incoming connection %d" !count;
        let initialize = Initialize.create ~id:(Id.make (Csexp.Atom "test")) in
        Lwt.return ((), initialize, None)
      in
      let on_connected () t =
        printfn "on_connected: %d" !count;
        let* res = Client.request t Request.ping () in
        let+ () =
          match res with
          | Error _ -> failwith "unexpected"
          | Ok () ->
            print_endline "received ping. shutting down server";
            Client.notification t Notification.shutdown ()
        in
        if !count = 3 then (
          (* TODO clean way for shutdown *)
          print_endline "received second session. shutting down";
          rpc#terminate
        )
      in
      let on_disconnect () =
        printfn "on_disconnect: %d" !count;
        Lwt.return ()
      in
      Client.connect_persistent chan ~on_connected ~on_connect ~on_disconnect
    in
    let run_build1 =
      let* _ = rpc in
      let+ res = build1#status in
      match res with
      | WEXITED i -> printfn "dune build finished with %i" i
      | _ -> assert false
    in
    let run_build2 =
      let* build2 = build2 in
      let* _ = run_build1 in
      let* _ = rpc in
      let* res = build2#status in
      match res with
      | WEXITED i ->
        printfn "dune build finished with %i" i;
        let+ rpc = rpc in
        rpc#terminate
      | _ -> assert false
    in
    Lwt.finalize
      (fun () ->
        run_with_timeout (fun () ->
            Lwt.all [ run_client; run_rpc; run_build1; run_build2 ]))
      (fun () ->
        let* rpc = rpc in
        rpc#terminate;
        build1#terminate;
        let+ build2 = build2 in
        build2#terminate)
  in
  Lwt_main.run test;
  [%expect
    {|
    build1: connecting
    incoming connection 1
    on_connected: 1
    received ping. shutting down server
    on_disconnect: 1
    dune build finished with 1
    build2: connecting
    incoming connection 2
    on_connected: 2
    received ping. shutting down server
    on_disconnect: 2
    dune build finished with 0 |}]
