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

module Symlink_socket : sig
  (** This module encapsulates the trick of using symlinks to get around the
      restriction on length of domain sockets. *)

  type t

  val create : Path.t -> t

  val cleanup : t -> unit

  val socket : t -> Path.t
end = struct
  type t =
    { symlink : Path.t
    ; socket : Path.t
    ; cleanup : unit Lazy.t
    }

  let make_cleanup ~symlink ~socket =
    lazy
      (Path.unlink_no_err socket;
       Path.unlink_no_err symlink)

  let cleanup t = Lazy.force t.cleanup

  let create desired_path =
    let socket =
      let dir =
        Path.of_string
          (match Xdg.runtime_dir with
          | Some p -> p
          | None -> Filename.get_temp_dir_name ())
      in
      Temp.temp_file ~dir ~prefix:"" ~suffix:".dune"
    in
    let () =
      let dest =
        let from = Path.external_ (Path.External.cwd ()) in
        Path.mkdir_p (Path.parent_exn desired_path);
        Path.reach_for_running ~from desired_path
      in
      Unix.symlink (Path.to_string socket) dest
    in
    let cleanup = make_cleanup ~symlink:desired_path ~socket in
    let t = { symlink = desired_path; socket; cleanup } in
    at_exit (fun () -> Lazy.force cleanup);
    t

  let socket t = t.socket
end

type t =
  | Client of { scheduler : Csexp_rpc.Scheduler.t }
  | Server of
      { server : Csexp_rpc.Server.t
      ; scheduler : Csexp_rpc.Scheduler.t
      ; handler : Dune_rpc_server.t
      ; pool : Fiber.Pool.t
      ; where : Dune_rpc_private.Where.t
      ; stats : Stats.t option
      ; symlink_socket : Symlink_socket.t option
      }

let t_var = Fiber.Var.create ()

let t () = Fiber.Var.get_exn t_var

module Server = Dune_rpc_server.Make (Csexp_rpc.Session)

let where_to_socket = function
  | `Ip (addr, `Port port) -> Unix.ADDR_INET (addr, port)
  | `Unix p -> Unix.ADDR_UNIX (Path.to_string p)

let of_config config scheduler stats =
  match config with
  | Config.Client -> Client { scheduler }
  | Config.Server { handler; backlog; pool } ->
    let where = Dune_rpc_private.Where.default () in
    let real_where, symlink_socket =
      match where with
      | `Ip _ -> (where_to_socket where, None)
      | `Unix path ->
        let symlink_socket = Symlink_socket.create path in
        ( Unix.ADDR_UNIX (Path.to_string (Symlink_socket.socket symlink_socket))
        , Some symlink_socket )
    in
    let server = Csexp_rpc.Server.create real_where ~backlog scheduler in
    Server { server; handler; where; symlink_socket; stats; pool; scheduler }

module Persistent = struct
  (** Connection negotiation for persistent connections *)

  let clients_dir =
    lazy
      (Path.Build.relative
         (Lazy.force Dune_rpc_private.Where.rpc_dir)
         "clients")

  let client_sock_fname = "s"

  let waiting_clients scheduler =
    let waiting = Path.build (Lazy.force clients_dir) in
    match Path.readdir_unsorted_with_kinds waiting with
    | Error _ -> Fiber.return []
    | Ok waiters ->
      List.filter_map waiters ~f:(fun (file, (kind : Unix.file_kind)) ->
          let path = Path.relative waiting file in
          let socket =
            match kind with
            | S_DIR ->
              let socket = Path.relative path client_sock_fname in
              Some (Unix.ADDR_UNIX (Path.to_string socket))
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
end

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
                let* waiting, serve =
                  (* This waits until we listen on the socket before serving
                     clients that were already waiting. Not ideal. *)
                  Fiber.fork_and_join
                    (fun () -> Persistent.waiting_clients t.scheduler)
                    (fun () -> Csexp_rpc.Server.serve t.server)
                in
                let sessions =
                  Fiber.Stream.In.append (Fiber.Stream.In.of_list waiting) serve
                in
                let* () = Server.serve sessions t.stats t.handler in
                Fiber.Pool.stop t.pool)
              (fun () -> Fiber.Pool.run t.pool))
          ~finally:(fun () ->
            Option.iter t.symlink_socket ~f:Symlink_socket.cleanup;
            Fiber.return ()))

let stop () =
  let open Fiber.O in
  let* t = Fiber.Var.get_exn t_var in
  match t with
  | Client _ -> Code_error.raise "rpc not running" []
  | Server s ->
    Csexp_rpc.Server.stop s.server;
    Fiber.return ()

module Connect = struct
  let csexp_client t p =
    let csexp_scheduler =
      match t with
      | Client c -> c.scheduler
      | Server s -> s.scheduler
    in
    Csexp_rpc.Client.create (where_to_socket p) csexp_scheduler

  let client_address what =
    let dir = Lazy.force Persistent.clients_dir |> Path.build in
    Path.mkdir_p dir;
    Temp.temp_in_dir what ~dir ~prefix:"" ~suffix:".client"
    |> Path.as_in_build_dir_exn

  let csexp_server t p =
    let csexp_scheduler =
      match t with
      | Server _ -> assert false
      | Client c -> c.scheduler
    in
    let p =
      match p with
      | `Ip _ as p -> p
      | `Unix (`Dir d) ->
        let symlink_socket =
          Symlink_socket.create (Path.relative d Persistent.client_sock_fname)
        in
        `Unix (Symlink_socket.socket symlink_socket)
    in
    Csexp_rpc.Server.create (where_to_socket p) ~backlog:1 csexp_scheduler

  let csexp_connect t in_ out =
    let csexp_scheduler =
      match t with
      | Client c -> c.scheduler
      | Server s -> s.scheduler
    in
    Csexp_rpc.Session.create in_ out csexp_scheduler

  let connect_persistent t =
    let server, where_file =
      let where, where_file =
        if Sys.win32 then
          let addr = Unix.inet_addr_of_string "0.0.0.0" in
          (`Ip (addr, `Port 0), Some (client_address File))
        else
          let dir = client_address Dir in
          (`Unix (`Dir (Path.build dir)), None)
      in
      (csexp_server t where, where_file)
    in
    let open Fiber.O in
    let* listen_sessions =
      let+ res = Csexp_rpc.Server.serve server in
      (match (Csexp_rpc.Server.listening_address server, where_file) with
      | ADDR_UNIX _, _ -> ()
      | ADDR_INET _, None ->
        (* the socket is already created, we don't need to write a file to
           announce where we are listening *)
        assert false
      | ADDR_INET (addr, port), Some where_file ->
        let where = `Ip (addr, `Port port) in
        Io.write_file (Path.build where_file)
          (Dune_rpc_private.Where.to_string where));
      res
    in
    let client =
      Dune_rpc_private.Where.get () |> Option.map ~f:(csexp_client t)
    in
    (* The combined sessions are the one that we established ourselves + the
       remaining sessions later servers will establish by connecting to the
       client *)
    match client with
    | None -> Fiber.return (listen_sessions, None)
    | Some c ->
      let+ session = Csexp_rpc.Client.connect c in
      (Fiber.Stream.In.cons session listen_sessions, Some c)
end

let client t p init ~on_notification ~f =
  let open Fiber.O in
  let c = Connect.csexp_client t p in
  let* session = Csexp_rpc.Client.connect c in
  Client.connect_raw session init ~on_notification ~f
