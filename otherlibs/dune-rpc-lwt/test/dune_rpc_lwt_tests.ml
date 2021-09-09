(* end to end tests of dune_rpc. Verify that whatever is exposed to the client
   is usable *)

open Stdune
open Lwt.Syntax
open Dune_rpc.V1
open Dune_rpc_lwt.V1

let connect ~root_dir =
  let build_dir = Filename.concat root_dir "_build" in
  let* res = Where.get ~build_dir in
  match res with
  | None ->
    Lwt.fail_with (sprintf "unable to establish to connection in %s" build_dir)
  | Some w -> connect_chan w

let build_watch ~root_dir ~suppress_stderr =
  Lwt_process.open_process_none ~stdin:`Close
    ~stderr:
      (if suppress_stderr then
        `Dev_null
      else
        `Keep)
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

let initial_cwd = Sys.getcwd ()

let%expect_test "run and connect" =
  let initialize = Initialize.create ~id:(Id.make (Csexp.Atom "test")) in
  Sys.chdir initial_cwd;
  Lwt_main.run
    (let* root_dir = Lwt_io.create_temp_dir () in
     Sys.chdir root_dir;
     let build = build_watch ~root_dir ~suppress_stderr:false in
     let rpc =
       let* () = Lwt_unix.sleep 0.5 in
       connect ~root_dir
     in
     let run_client =
       let* rpc = rpc in
       Client.connect rpc initialize ~f:(fun t ->
           print_endline "started session";
           let* res = Client.request t Request.ping () in
           match res with
           | Error _ -> failwith "unexpected"
           | Ok () ->
             print_endline "received ping. shutting down.";
             Client.notification t Notification.shutdown ())
     in
     let run_build =
       let+ res = build#status in
       match res with
       | WEXITED i -> printfn "dune build finished with %i" i
       | _ -> assert false
     in
     Lwt.finalize
       (fun () ->
         run_with_timeout (fun () -> Lwt.all [ run_client; run_build ]))
       (fun () ->
         build#terminate;
         Lwt.return_unit));
  [%expect
    {|
    Success, waiting for filesystem changes...
    started session
    received ping. shutting down.
    dune build finished with 0 |}]

module Logger = struct
  (* A little helper to make the output from the client and server deterministic.
     Log messages are batched and outputted at the end. *)
  type t =
    { mutable messages : string list
    ; name : string
    }

  let create ~name = { messages = []; name }

  let log t fmt = Printf.ksprintf (fun m -> t.messages <- m :: t.messages) fmt

  let print { messages; name } =
    List.rev messages |> List.iter ~f:(fun msg -> printfn "%s: %s" name msg)
end
