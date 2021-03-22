(* end to end tests of dune_rpc. Verify that whatever is exposed to the client
   is usable *)

open Stdune
open Dune_rpc.V1
open Dune_rpc_lwt.V1

let%expect_test "run and connect" =
  let open Lwt.Syntax in
  let initialize = Initialize.create ~id:(Id.make (Csexp.Atom "test")) in
  Lwt_main.run
    (let* root_dir = Lwt_io.create_temp_dir () in
     let build =
       Lwt_process.open_process_none
         ( "dune"
         , [| "dune"
            ; "build"
            ; "--no-print-directory"
            ; "--root"
            ; root_dir
            ; "-w"
            ; "@install"
           |] )
     in
     let rpc =
       let+ () = Lwt_unix.sleep 0.5 in
       Lwt_process.open_process_full
         ("dune", [| "dune"; "rpc"; "init"; "--root"; root_dir |])
     in
     let client =
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
     let rpc =
       let* rpc = rpc in
       let+ res = rpc#status in
       match res with
       | WEXITED i -> printfn "rpc init finished with %i" i
       | _ -> assert false
     in
     let build =
       let+ res = build#status in
       match res with
       | WEXITED i -> printfn "dune build finished with %i" i
       | _ -> assert false
     in
     Lwt.catch
       (fun () ->
         let+ () =
           Lwt_unix.with_timeout 3.0 (fun () ->
               let+ _ = Lwt.all [ client; rpc; build ] in
               ())
         in
         print_endline "success")
       (fun exn ->
         (match exn with
         | Lwt_unix.Timeout -> print_endline "timeout"
         | _ -> ());
         Lwt.return_unit));
  [%expect
    {|
    started session
    received ping. shutting down.
    rpc init finished with 0
    dune build finished with 0
    success |}]
