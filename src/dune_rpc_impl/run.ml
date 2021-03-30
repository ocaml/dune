open Stdune

module Config = struct
  type t =
    | Client
    | Server of
        { handler : Dune_rpc_server.t
        ; pool : Fiber.Pool.t
        ; backlog : int
        }
end

type cleanup =
  { symlink : Path.t
  ; socket : Path.t
  }

type t =
  | Client of { scheduler : Csexp_rpc.Scheduler.t }
  | Server of
      { server : Csexp_rpc.Server.t
      ; scheduler : Csexp_rpc.Scheduler.t
      ; handler : Dune_rpc_server.t
      ; pool : Fiber.Pool.t
      ; where : Dune_rpc_private.Where.t
      ; stats : Stats.t option
      ; cleanup : cleanup option
      }

let t_var = Fiber.Var.create ()

let t () = Fiber.Var.get_exn t_var

module Server = Dune_rpc_server.Make (Csexp_rpc.Session)

let delete_cleanup = function
  | None -> ()
  | Some { socket; symlink } ->
    Path.unlink_no_err socket;
    Path.unlink_no_err symlink

let where_to_socket = function
  | `Ip (addr, `Port port) -> Unix.ADDR_INET (addr, port)
  | `Unix p -> Unix.ADDR_UNIX (Path.to_string p)

let of_config config scheduler stats =
  match config with
  | Config.Client -> Client { scheduler }
  | Config.Server { handler; backlog; pool } ->
    let where = Dune_rpc_private.Where.default () in
    let real_where, cleanup =
      match where with
      | `Ip _ -> (where_to_socket where, None)
      | `Unix symlink ->
        let socket =
          let dir =
            Path.of_string
              (match Xdg.runtime_dir with
              | Some p -> p
              | None -> Filename.get_temp_dir_name ())
          in
          Temp.temp_path ~dir ~prefix:"dune" ~suffix:""
        in
        Unix.symlink (Path.to_string socket)
          (let from = Path.external_ (Path.External.cwd ()) in
           Path.mkdir_p (Path.parent_exn symlink);
           Path.reach_for_running ~from symlink);
        let cleanup = Some { socket; symlink } in
        at_exit (fun () -> delete_cleanup cleanup);
        (ADDR_UNIX (Path.to_string socket), cleanup)
    in
    let server = Csexp_rpc.Server.create real_where ~backlog scheduler in
    Server { server; handler; where; cleanup; stats; pool; scheduler }

let run t =
  Fiber.Var.set t_var t (fun () ->
      match t with
      | Client _ -> Fiber.return ()
      | Server t ->
        Fiber.finalize
          (fun () ->
            let open Fiber.O in
            Fiber.fork_and_join_unit
              (fun () ->
                let* sessions = Csexp_rpc.Server.serve t.server in
                let* () = Server.serve sessions t.stats t.handler in
                Fiber.Pool.stop t.pool)
              (fun () -> Fiber.Pool.run t.pool))
          ~finally:(fun () ->
            delete_cleanup t.cleanup;
            Fiber.return ()))

let csexp_client t p =
  let csexp_scheduler =
    match t with
    | Client c -> c.scheduler
    | Server s -> s.scheduler
  in
  Csexp_rpc.Client.create (where_to_socket p) csexp_scheduler

let client t p init ~on_notification ~f =
  let open Fiber.O in
  let c = csexp_client t p in
  let* session = Csexp_rpc.Client.connect c in
  Client.connect_raw session init ~on_notification ~f

let csexp_connect t in_ out =
  let csexp_scheduler =
    match t with
    | Client c -> c.scheduler
    | Server s -> s.scheduler
  in
  Csexp_rpc.Session.create in_ out csexp_scheduler

let stop () =
  let t = Fiber.Var.get_exn t_var in
  match t with
  | Client _ -> Code_error.raise "rpc not running" []
  | Server s ->
    Csexp_rpc.Server.stop s.server;
    Fiber.return ()
