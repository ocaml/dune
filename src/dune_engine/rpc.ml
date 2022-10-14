open Fiber.O

type server =
  { run : unit Fiber.t
  ; stop : unit Fiber.t
  ; ready : unit Fiber.t
  }

type t =
  { server : server
  ; pool : Fiber.Pool.t
  }

let t = Fiber.Var.create ()

let with_background_rpc server f =
  let pool = Fiber.Pool.create () in
  Fiber.Var.set t { server; pool } (fun () ->
      Fiber.fork_and_join_unit
        (fun () -> Fiber.Pool.run pool)
        (fun () -> Fiber.finalize f ~finally:(fun () -> Fiber.Pool.stop pool)))

let ensure_ready () =
  let* { server; pool } = Fiber.Var.get_exn t in
  let* () = Fiber.Pool.task pool ~f:(fun () -> server.run) in
  server.ready

let stop () =
  let* { server; pool } = Fiber.Var.get_exn t in
  Fiber.fork_and_join_unit
    (fun () -> Fiber.Pool.stop pool)
    (fun () -> server.stop)
