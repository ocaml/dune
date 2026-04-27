open Import
open Fiber.O

type server =
  { run : unit Fiber.t
  ; stop : unit Fiber.t
  ; ready : unit Fiber.t
  }

type t =
  { server : server
  ; pool : Fiber.Pool.t
  ; mutable state : [ `Awaiting_start | `Running | `Stopped ]
  }

let current : t option ref = ref None

let get_exn () =
  match !current with
  | Some t -> t
  | None -> Code_error.raise "rpc server not available" []
;;

let stop ({ state; server; pool } as t) =
  let* () = Fiber.return () in
  match state with
  | `Stopped -> Fiber.return ()
  | `Awaiting_start -> Fiber.Pool.close pool
  | `Running ->
    t.state <- `Stopped;
    Fiber.fork_and_join_unit (fun () -> Fiber.Pool.close pool) (fun () -> server.stop)
;;

let with_background_rpc server f =
  let pool = Fiber.Pool.create () in
  let v = { state = `Awaiting_start; server; pool } in
  let previous = !current in
  current := Some v;
  Fiber.finalize
    (fun () ->
       Fiber.fork_and_join_unit
         (fun () -> Fiber.Pool.run pool)
         (fun () -> Fiber.finalize f ~finally:(fun () -> stop v)))
    ~finally:(fun () ->
      current := previous;
      Fiber.return ())
;;

let ensure_ready () =
  let ({ state; server; pool } as t) = get_exn () in
  match state with
  | `Stopped -> Code_error.raise "server already stopped" []
  | `Running -> Fiber.return ()
  | `Awaiting_start ->
    t.state <- `Running;
    let* () = Fiber.Pool.task pool ~f:(fun () -> server.run) in
    server.ready
;;

let stop () = stop (get_exn ())
