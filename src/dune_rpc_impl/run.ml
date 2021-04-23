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

let clients_dir =
  lazy
    (Path.Build.relative (Lazy.force Dune_rpc_private.Where.rpc_dir) "clients")

let client_address () =
  let dir = Lazy.force clients_dir |> Path.build in
  Path.mkdir_p dir;
  Temp.temp_path ~dir ~prefix:"" ~suffix:".client" |> Path.as_in_build_dir_exn

let waiting_clients scheduler =
  let waiting = Path.build (Lazy.force clients_dir) in
  match Path.readdir_unsorted_with_kinds waiting with
  | Error _ -> Fiber.return []
  | Ok waiters ->
    List.filter_map waiters ~f:(fun (file, (kind : Unix.file_kind)) ->
        let path = Path.relative waiting file in
        let socket =
          match kind with
          | S_SOCK -> Some (Unix.ADDR_UNIX (Path.to_string path))
          | S_REG ->
            Some
              (Io.read_file path |> Dune_rpc_private.Where.of_string
             |> where_to_socket)
          | _ -> None
        in
        let open Option.O in
        let+ socket = socket in
        Csexp_rpc.Client.create socket scheduler)
    |> Fiber.parallel_map ~f:Csexp_rpc.Client.connect

let run t =
  Fiber.Var.set t_var t (fun () ->
      match t with
      | Client _ -> Fiber.return ()
      | Server t ->
        Fiber.finalize
          (fun () ->
            let open Fiber.O in
            let* waiting, serve =
              (* This waits until we listen on the socket before serving clients
                 that were already waiting. Not ideal. *)
              Fiber.fork_and_join
                (fun () -> waiting_clients t.scheduler)
                (fun () -> Csexp_rpc.Server.serve t.server)
            in
            let sessions =
              Fiber.Stream.In.append (Fiber.Stream.In.of_list waiting) serve
            in
            let* () = Server.serve sessions t.stats t.handler in
            Fiber.Pool.stop t.pool)
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

let csexp_server t p =
  let csexp_scheduler =
    match t with
    | Server _ -> assert false
    | Client c -> c.scheduler
  in
  Csexp_rpc.Server.create (where_to_socket p) ~backlog:1 csexp_scheduler

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
  let open Fiber.O in
  let* t = Fiber.Var.get_exn t_var in
  match t with
  | Client _ -> Code_error.raise "rpc not running" []
  | Server s ->
    Csexp_rpc.Server.stop s.server;
    Fiber.return ()
