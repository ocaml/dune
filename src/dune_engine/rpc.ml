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

let t = Fiber.Var.create ()

let stop ({ state; server; pool } as t) =
  let* () = Fiber.return () in
  match state with
  | `Stopped -> Fiber.return ()
  | `Awaiting_start -> Fiber.Pool.stop pool
  | `Running ->
    t.state <- `Stopped;
    Fiber.fork_and_join_unit
      (fun () -> Fiber.Pool.stop pool)
      (fun () -> server.stop)

let with_background_rpc server f =
  let pool = Fiber.Pool.create () in
  let v = { state = `Awaiting_start; server; pool } in
  Fiber.Var.set t v (fun () ->
      Fiber.fork_and_join_unit
        (fun () -> Fiber.Pool.run pool)
        (fun () -> Fiber.finalize f ~finally:(fun () -> stop v)))

let ensure_ready () =
  let* ({ state; server; pool } as t) = Fiber.Var.get_exn t in
  match state with
  | `Stopped -> Code_error.raise "server already stopped" []
  | `Running -> Fiber.return ()
  | `Awaiting_start ->
    t.state <- `Running;
    let* () = Fiber.Pool.task pool ~f:(fun () -> server.run) in
    server.ready

let stop () =
  let* t = Fiber.Var.get_exn t in
  stop t
