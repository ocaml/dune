open Dune_rpc_lwt.V1.Action_plugin

let touch path =
  let oc = open_out path in
  close_out oc
;;

let rec wait_for_file path =
  if not (Sys.file_exists path)
  then (
    ignore (Unix.select [] [] [] 0.01);
    wait_for_file path)
;;

let noop () = run (fun _ -> Lwt.return_unit)

let hold started release =
  touch started;
  wait_for_file release;
  run (fun _ -> Lwt.return_unit)
;;

let touch_and_respond path =
  (try touch path with
   | Sys_error _ -> ());
  run (fun _ -> Lwt.return_unit)
;;

let initialize_only started release =
  let module Rpc = Dune_rpc.Private in
  let module Canonical = Csexp.Make (Stdune.Sexp) in
  let open Lwt.Infix in
  Dune_rpc_lwt.V1.Where.get ~env:Sys.getenv_opt ~build_dir:"_build"
  >>= function
  | Error exn -> Lwt.fail exn
  | Ok None -> Lwt.fail_with "No RPC server"
  | Ok (Some where) ->
    Dune_rpc_lwt.V1.connect_chan where
    >>= fun (input, output) ->
    Lwt.finalize
      (fun () ->
         let init =
           Rpc.Initialize.Request.create
             ~id:(Rpc.Id.make (Stdune.Sexp.Atom "held-client"))
         in
         let request =
           Rpc.Packet.Request
             ( Rpc.Id.make (Stdune.Sexp.Atom "initialize")
             , Rpc.Initialize.Request.to_call init )
         in
         Lwt_io.write
           output
           (Canonical.to_string (Stdune.Conv.to_sexp Rpc.Packet.sexp request))
         >>= fun () ->
         Lwt_io.flush output
         >>= fun () ->
         let buffer = Buffer.create 128 in
         let rec read_response () =
           Lwt_io.read_char input
           >>= fun c ->
           Buffer.add_char buffer c;
           match Canonical.parse_string (Buffer.contents buffer) with
           | Error _ -> read_response ()
           | Ok response ->
             (match
                Stdune.Conv.of_sexp Rpc.Packet.sexp ~version:Rpc.Version.latest response
              with
              | Ok (Rpc.Packet.Response (_, Ok _)) ->
                touch started;
                wait_for_file release;
                Lwt.return_unit
              | _ -> Lwt.fail_with "RPC initialization failed")
         in
         read_response ())
      (fun () -> Lwt_io.close input >>= fun () -> Lwt_io.close output)
;;

let () =
  Lwt_main.run
    (match Array.to_list Sys.argv with
     | [ _; "noop" ] -> noop ()
     | [ _; "hold"; started; release ] -> hold started release
     | [ _; "touch"; path ] -> touch_and_respond path
     | [ _; "initialize-only"; started; release ] -> initialize_only started release
     | _ ->
       prerr_endline
         "Usage: action_plugin_helper (noop | hold <started> <release> | touch <path> | \
          initialize-only <started> <release>)";
       exit 1)
;;
