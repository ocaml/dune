open Stdune
open Fiber.O
module Dune_rpc = Dune_rpc.Private
open Dune_rpc
open Rpc.Server
module Scheduler = Test_scheduler

module Chan = struct
  type t =
    { queue : Sexp.t Queue.t
    ; mutable reader : Sexp.t option Fiber.Ivar.t option
    ; mutable closed : bool
    ; mutable peer_closed : bool
    ; mutable peer : t option
    }

  let create () =
    { queue = Queue.create ()
    ; reader = None
    ; closed = false
    ; peer_closed = false
    ; peer = None
    }
  ;;

  let create_pair () =
    let c1 = create () in
    let c2 = create () in
    c1.peer <- Some c2;
    c2.peer <- Some c1;
    c1, c2
  ;;

  let write_to_reader t value =
    match t.reader with
    | None ->
      (match value with
       | None -> t.peer_closed <- true
       | Some value -> Queue.push t.queue value);
      Fiber.return ()
    | Some reader ->
      t.reader <- None;
      (match value with
       | None -> t.peer_closed <- true
       | Some _ -> ());
      Fiber.Ivar.fill reader value
  ;;

  let close t =
    if t.closed
    then Fiber.return ()
    else (
      t.closed <- true;
      match t.peer with
      | None -> Fiber.return ()
      | Some peer -> write_to_reader peer None)
  ;;

  let write t s =
    if t.closed
    then Fiber.return (Error `Closed)
    else (
      match t.peer with
      | None -> Code_error.raise "channel is not connected" []
      | Some peer ->
        let+ () = Fiber.sequential_iter s ~f:(fun s -> write_to_reader peer (Some s)) in
        Ok ())
  ;;

  let read t =
    match Queue.pop t.queue with
    | Some value -> Fiber.return (Some value)
    | None ->
      if t.closed || t.peer_closed
      then Fiber.return None
      else (
        let reader = Fiber.Ivar.create () in
        t.reader <- Some reader;
        Fiber.Ivar.read reader)
  ;;

  let connect c1 c2 =
    match c1.peer, c2.peer with
    | None, None ->
      c1.peer <- Some c2;
      c2.peer <- Some c1;
      Fiber.return ()
    | _ -> Code_error.raise "channels are already connected" []
  ;;

  let name _ = "unnamed"
end

module Drpc = struct
  module Client =
    Dune_rpc.Client.Make
      (Rpc.Private.Fiber)
      (struct
        include Chan

        let write t packets =
          write t packets
          >>| function
          | Ok () -> ()
          | Error `Closed -> raise Dune_util.Report_error.Already_reported
        ;;
      end)

  module Server = Rpc.Server.Make (Chan)
end

let on_init _ _ = Fiber.return ()

let setup_client_server () =
  let client_chan = Chan.create () in
  let server_chan = Chan.create () in
  let sessions = Fiber.Stream.In.of_list [ server_chan ] in
  let connect () = Chan.connect client_chan server_chan in
  client_chan, sessions, connect
;;

let setup_direct_client_server () =
  let client_chan, server_chan = Chan.create_pair () in
  let sessions = Fiber.Stream.In.of_list [ server_chan ] in
  client_chan, sessions
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
      let+ () = Drpc.Server.serve sessions (Rpc.Server.make handler) in
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
