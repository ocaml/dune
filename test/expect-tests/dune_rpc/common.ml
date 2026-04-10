open Stdune
open Fiber.O
module Dune_rpc = Dune_rpc.Private
open Dune_rpc
open Dune_rpc_server
module Scheduler = Test_scheduler

module Chan = struct
  module Mvar = Fiber.Mvar

  type t =
    { (* Read end. Populated by writing by [snd] *)
      in_ : Sexp.t Fiber.Stream.In.t * Sexp.t Fiber.Stream.Out.t
    ; (* Write end. Can be read via [fst] *)
      out : Sexp.t Fiber.Stream.In.t * Sexp.t Fiber.Stream.Out.t
    }

  let create () = { in_ = Fiber.Stream.pipe (); out = Fiber.Stream.pipe () }
  let close t = Fiber.Stream.Out.write (snd t.out) None

  let write t s =
    let+ () =
      Fiber.sequential_iter s ~f:(fun s -> Fiber.Stream.Out.write (snd t.out) (Some s))
    in
    Ok ()
  ;;

  let read t = Fiber.Stream.In.read (fst t.in_)

  let connect c1 c2 =
    Fiber.fork_and_join_unit
      (fun () -> Fiber.Stream.connect (fst c1.out) (snd c2.in_))
      (fun () -> Fiber.Stream.connect (fst c2.out) (snd c1.in_))
  ;;

  let name _ = "unnamed"
end

module Drpc = struct
  module Client =
    Dune_rpc.Client.Make
      (Dune_rpc_client.Private.Fiber)
      (struct
        include Chan

        let write t packets = write t packets >>| Result.ok_exn
      end)

  module Server = Dune_rpc_server.Make (Chan)
end

let on_init _ _ = Fiber.return ()

let setup_client_server () =
  let client_chan = Chan.create () in
  let server_chan = Chan.create () in
  let sessions = Fiber.Stream.In.of_list [ server_chan ] in
  let connect () = Chan.connect client_chan server_chan in
  client_chan, sessions, connect
;;

let test ?(private_menu = []) ?(real_methods = true) ~client ~handler ~init () =
  if real_methods
  then
    Handler.implement_notification handler Procedures.Public.shutdown (fun _ _ ->
      failwith "shutdown called");
  let run =
    let client_chan, sessions, connect = setup_client_server () in
    let client () =
      Drpc.Client.connect_with_menu client_chan init ~private_menu ~f:(fun c ->
        let* () = client c in
        Chan.close client_chan)
    in
    let server () =
      let+ () = Drpc.Server.serve sessions (Dune_rpc_server.make handler) in
      printfn "server: finished."
    in
    Fiber.parallel_iter [ connect; client; server ] ~f:(fun f -> f ())
  in
  Scheduler.run (Scheduler.create ()) run
;;

let simple_request
      (type a b)
      ?(version = 1)
      ~method_
      (req : (a, Conv.values) Conv.t)
      (resp : (b, Conv.values) Conv.t)
  =
  let v = Decl.Request.make_current_gen ~req ~resp ~version in
  Decl.Request.make ~method_ ~generations:[ v ]
;;

let request_exn client witness n =
  let* staged = Drpc.Client.Versioned.prepare_request client witness in
  let staged =
    match staged with
    | Ok s -> s
    | Error e -> raise (Dune_rpc.Version_error.E e)
  in
  Drpc.Client.request client staged n
;;
