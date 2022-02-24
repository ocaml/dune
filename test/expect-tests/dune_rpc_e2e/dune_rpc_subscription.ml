open Stdune
open Fiber.O
open Dune_rpc_e2e
module Dune_rpc = Dune_rpc_private
module Sub = Dune_rpc.Sub
module Client = Dune_rpc_impl.Client

let rec await_progress (poll : Dune_rpc.Progress.t Client.Stream.t) =
  let* res = Client.Stream.next poll in
  match res with
  | None ->
    printfn "client: progress stream finished";
    Fiber.return ()
  | Some Success ->
    printfn "Succeeded";
    Fiber.return ()
  | Some Failed ->
    printfn "Failed";
    Fiber.return ()
  | Some (Waiting | In_progress _ | Interrupted) -> await_progress poll

let display_progress : Dune_rpc.Progress.t -> unit = function
  | Waiting -> printfn "(Waiting)"
  | In_progress { complete; remaining } ->
    printfn "(In_progress ((complete %d) (remaining %d)))" complete remaining
  | Failed -> printfn "(Failed)"
  | Interrupted -> printfn "(Interrupted)"
  | Success -> printfn "(Success)"

let%expect_test "cancelling subscription while request is in-flight" =
  let exec _pid =
    let pool = Fiber.Pool.create () in
    Fiber.fork_and_join_unit
      (fun () -> Fiber.Pool.run pool)
      (fun () ->
        run_client (fun client ->
            files
              [ ("dune", "(executable (name foo))"); ("foo.ml", "let () = ()") ];
            let* poll = poll_exn client Dune_rpc.Public.Sub.progress in
            let* () = dune_build client "./foo.exe" in
            (* Clear the pending queue *)
            let* () = await_progress poll in
            [%expect
              {|
              Building ./foo.exe
              Build ./foo.exe succeeded
              Succeeded |}];
            let result = Fiber.Ivar.create () in
            (* This ordering is a bit strange. We need the pool to begin
               executing the [next] fiber before we cancel the subscription,
               which is inherently a bit of a race condition. *)
            let* () =
              Fiber.Pool.task pool ~f:(fun () -> Client.Stream.cancel poll)
            in
            let* () = Fiber.return () in
            let* () =
              (* Because we've already received all pending progress updates,
                 this request will hang forever unless cancelled. *)
              let* response = Client.Stream.next poll in
              Fiber.Ivar.fill result response
            in
            let* response = Fiber.Ivar.read result in
            (match response with
            | None -> ()
            | Some progress ->
              printfn "unexpectedly received response";
              display_progress progress);
            [%expect {||}];
            Fiber.Pool.stop pool))
  in
  run (fun () -> with_dune_watch exec);
  [%expect {| |}]
