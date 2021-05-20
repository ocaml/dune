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
  (* Maximum path before we resort to the trick *)
  let max_path_length = 100

  type t =
    { symlink : Path.t option
    ; socket : Path.t
    ; cleanup : unit Lazy.t
    }

  let make_cleanup ~symlink ~socket =
    lazy
      (Path.unlink_no_err socket;
       Option.iter symlink ~f:Path.unlink_no_err)

  let cleanup t = Lazy.force t.cleanup

  let create desired_path =
    let desired_path_s = Path.to_string desired_path in
    if String.length desired_path_s <= max_path_length then
      let cleanup = make_cleanup ~symlink:None ~socket:desired_path in
      { symlink = None; socket = desired_path; cleanup }
    else
      let socket =
        let dir =
          Path.of_string
            (match Xdg.runtime_dir with
            | Some p -> p
            | None -> Filename.get_temp_dir_name ())
        in
        Temp.temp_file ~dir ~prefix:"" ~suffix:"dune"
      in
      let () =
        let dest =
          let from = Path.external_ (Path.External.cwd ()) in
          Path.mkdir_p (Path.parent_exn desired_path);
          Path.reach_for_running ~from desired_path
        in
        Unix.symlink (Path.to_string socket) dest
      in
      let cleanup = make_cleanup ~symlink:(Some desired_path) ~socket in
      let t = { symlink = Some desired_path; socket; cleanup } in
      at_exit (fun () -> Lazy.force cleanup);
      t

  let socket t = t.socket
end

type t =
  | Client
  | Server of
      { server : Csexp_rpc.Server.t
      ; handler : Dune_rpc_server.t
      ; pool : Fiber.Pool.t
      ; where : Dune_rpc_private.Where.t
      ; stats : Dune_stats.t option
      ; symlink_socket : Symlink_socket.t option
      }

let t_var : t Fiber.Var.t = Fiber.Var.create ()

module Server = Dune_rpc_server.Make (Csexp_rpc.Session)

let where_to_socket = function
  | `Ip (addr, `Port port) -> Unix.ADDR_INET (addr, port)
  | `Unix p -> Unix.ADDR_UNIX (Path.to_string p)

module Persistent : sig
  (** Connection negotiation for persistent connections *)
  val waiting_clients : unit -> Csexp_rpc.Session.t list Fiber.t

  val create_waiting_client :
    unit -> Csexp_rpc.Session.t Fiber.Stream.In.t Fiber.t
end = struct
  let clients_dir =
    lazy
      (Path.Build.relative
         (Lazy.force Dune_rpc_private.Where.rpc_dir)
         "clients")

  let client_sock_fname = "s"

  let client_address what =
    let dir = Lazy.force clients_dir |> Path.build in
    Path.mkdir_p dir;
    Temp.temp_in_dir what ~dir ~prefix:"" ~suffix:".client"
    |> Path.as_in_build_dir_exn

  let csexp_server p =
    let p =
      match p with
      | `Ip _ as p -> p
      | `Unix (`Dir d) ->
        let symlink_socket =
          Symlink_socket.create (Path.relative d client_sock_fname)
        in
        `Unix (Symlink_socket.socket symlink_socket)
    in
    Csexp_rpc.Server.create (where_to_socket p) ~backlog:1

  let create_waiting_client () =
    let server, where_file =
      let where, where_file =
        if Sys.win32 then
          let addr = Unix.inet_addr_of_string "0.0.0.0" in
          (`Ip (addr, `Port 0), Some (client_address File))
        else
          let dir = client_address Dir in
          (`Unix (`Dir (Path.build dir)), None)
      in
      (csexp_server where, where_file)
    in
    let open Fiber.O in
    let+ sessions = Csexp_rpc.Server.serve server in
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
    sessions

  let waiting_clients () =
    let waiting = Path.build (Lazy.force clients_dir) in
    match Path.readdir_unsorted_with_kinds waiting with
    | Error _ -> Fiber.return []
    | Ok waiters ->
      Fiber.parallel_map waiters ~f:(fun (file, (kind : Unix.file_kind)) ->
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
          match socket with
          | None -> Fiber.return None
          | Some socket ->
            let open Fiber.O in
            let* client = Csexp_rpc.Client.create socket in
            let+ session = Csexp_rpc.Client.connect client in
            Some session)
      |> Fiber.map ~f:List.filter_opt
end

let of_config config stats =
  match config with
  | Config.Client -> Client
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
    let server = Csexp_rpc.Server.create real_where ~backlog in
    Server { server; handler; where; symlink_socket; stats; pool }

let run config stats =
  let t = of_config config stats in
  Fiber.Var.set t_var t (fun () ->
      match t with
      | Client -> Fiber.return ()
      | Server t ->
        Fiber.finalize
          (fun () ->
            let open Fiber.O in
            Fiber.fork_and_join_unit
              (fun () ->
                let* waiting, serve =
                  (* This waits until we listen on the socket before serving
                     clients that were already waiting. Not ideal. *)
                  Fiber.fork_and_join Persistent.waiting_clients (fun () ->
                      Csexp_rpc.Server.serve t.server)
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
  | Client -> Code_error.raise "rpc not running" []
  | Server s ->
    Csexp_rpc.Server.stop s.server;
    Fiber.return ()

module Connect = struct
  let csexp_client p = Csexp_rpc.Client.create (where_to_socket p)

  let connect_persistent () =
    let open Fiber.O in
    let* listen_sessions = Persistent.create_waiting_client () in
    let* client =
      match Dune_rpc_private.Where.get () with
      | None -> Fiber.return None
      | Some t -> csexp_client t |> Fiber.map ~f:Option.some
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

let client p init ~on_notification ~f =
  let open Fiber.O in
  let* c = Connect.csexp_client p in
  let* session = Csexp_rpc.Client.connect c in
  Client.connect_raw session init ~on_notification ~f
