open Stdune
open Fiber.O
open Dune_rpc_e2e
module Client = Dune_rpc_client.Client
module Dune_rpc = Dune_rpc_private

include struct
  open Dune_rpc
  module Job = Job
  module Conv = Conv
end

let files = List.iter ~f:(fun (f, contents) -> Io.String_path.write_file f contents)

let poll_exn client decl =
  let+ poll = Client.poll client decl in
  match poll with
  | Ok p -> p
  | Error e -> raise (Dune_rpc.Version_error.E e)
;;

let print_job_events poll =
  let+ res = Client.Stream.next poll in
  let id_to_string id = Conv.to_sexp Job.Id.sexp id |> Sexp.to_string in
  match res with
  | None -> printfn "client: no more diagnostics"
  | Some job_event_list ->
    List.iter job_event_list ~f:(fun job_event ->
      match (job_event : Job.Event.t) with
      | Start job ->
        printfn
          "Start %s %s"
          (id_to_string job.id)
          (Format.asprintf "%a" Pp.to_fmt job.description)
      | Stop id -> printfn "Stop %s" (id_to_string id))
;;

let%expect_test "rpc jobs after rebuild" =
  let rec wait_for_running_file () =
    match Unix.stat "_build/default/running" with
    | _stat -> printfn "Background process is running, let's interrupt it..."
    | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      Unix.sleepf 0.01;
      wait_for_running_file ()
  in
  ignore wait_for_running_file;
  let exec _pid =
    run_client (fun client ->
      let* () =
        Fiber.return
        @@ files
             [ "dune-project", "(lang dune 3.10)"
             ; ( "dune"
               , {|
                (rule
                 (target foo)
                 (deps bar)
                 (alias runtest)
                 (action
                  (progn
                   (write-file running "hello")
                   (system "sleep 100")
                   (write-file foo "foo"))))
                |}
               )
             ]
      in
      let* poll = poll_exn client Dune_rpc.Public.Sub.running_jobs in
      files [ "bar", "" ];
      wait_for_running_file ();
      let* () = print_job_events poll in
      files [ "bar", "more" ];
      wait_for_running_file ();
      let* () = print_job_events poll in
      Client.Stream.cancel poll)
  in
  run (fun () -> with_dune_watch ~watch_mode_args:[ "-w"; "@runtest" ] exec);
  [%expect
    {|
    Background process is running, let's interrupt it...
    Start 1 _build/default/foo
    Background process is running, let's interrupt it...
    Stop 1
    Start 2 _build/default/foo |}]
;;
