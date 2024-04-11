(* end to end tests of dune_rpc. Verify that whatever is exposed to the client
   is usable *)

open Stdune
open Lwt.Syntax
open Dune_rpc.V1
open Dune_rpc_lwt.V1

let _XDG_STATE_HOME = "XDG_STATE_HOME"
let xdg_state_dir = Temp.create Dir ~prefix:"lwt" ~suffix:"dune"
let () = Unix.putenv _XDG_STATE_HOME (Stdune.Path.to_absolute_filename xdg_state_dir)

let connect ~root_dir =
  let build_dir = Filename.concat root_dir "_build" in
  let env =
    let env =
      Env.add
        Env.initial
        ~var:_XDG_STATE_HOME
        ~value:(Stdune.Path.to_absolute_filename xdg_state_dir)
    in
    Env.get env
  in
  let* res = Where.get ~env ~build_dir in
  match res with
  | Error e -> Lwt.fail e
  | Ok None -> Lwt.fail_with (sprintf "unable to establish to connection in %s" build_dir)
  | Ok (Some where) ->
    let where =
      match where with
      | `Unix addr ->
        (* this hackery is needed because the temp dir we wrote the socket to is
           symlinked on a mac *)
        let addr =
          match Unix.realpath addr with
          | s -> s
          | exception Unix.Unix_error _ -> addr
        in
        `Unix
          (match String.drop_prefix addr ~prefix:(Sys.getcwd () ^ "/") with
           | None -> addr
           | Some addr -> Filename.concat "." addr)
      | _ as s -> s
    in
    connect_chan where
;;

let build_watch ~root_dir =
  Lwt_process.open_process_none
    ~stdin:`Close
    ~stderr:`Dev_null
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
;;

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
;;

let initial_cwd = Sys.getcwd ()

let%expect_test "run and connect" =
  let initialize = Initialize.create ~id:(Id.make (Csexp.Atom "test")) in
  Sys.chdir initial_cwd;
  Lwt_main.run
    (let* root_dir = Lwt_io.create_temp_dir () in
     Sys.chdir root_dir;
     let build = build_watch ~root_dir in
     let rpc =
       let* () = Lwt_unix.sleep 0.5 in
       connect ~root_dir
     in
     let run_client =
       let* rpc = rpc in
       Client.connect rpc initialize ~f:(fun t ->
         print_endline "started session";
         let* ping = Client.Versioned.prepare_request t Request.ping in
         let ping =
           match ping with
           | Ok p -> p
           | Error _ -> assert false
         in
         let* res = Client.request t ping () in
         match res with
         | Error _ -> failwith "unexpected"
         | Ok () ->
           print_endline "received ping. shutting down.";
           let* shutdown =
             Client.Versioned.prepare_notification t Notification.shutdown
           in
           let shutdown =
             match shutdown with
             | Ok s -> s
             | Error _ -> assert false
           in
           Client.notification t shutdown ())
     in
     let run_build =
       let+ res = build#status in
       match res with
       | WEXITED i -> printfn "dune build finished with %i" i
       | _ -> assert false
     in
     Lwt.finalize
       (fun () -> run_with_timeout (fun () -> Lwt.all [ run_client; run_build ]))
       (fun () ->
         build#terminate;
         Lwt.return_unit));
  [%expect
    {|
    started session
    received ping. shutting down.
    dune build finished with 0
    success |}]
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
