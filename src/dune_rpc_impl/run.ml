open Stdune
module Scheduler = Dune_engine.Scheduler
module Registry = Dune_rpc_private.Registry

module Config = struct
  type t =
    | Client
    | Server of
        { handler : Dune_rpc_server.t
        ; pool : Fiber.Pool.t
        ; backlog : int
        ; root : string
        }
end

module Symlink_socket : sig
  (** This module encapsulates the trick of using symlinks to get around the
      restriction on length of domain sockets. *)

  type t

  val create : Path.t -> t

  (* Actually create the symlink (if necessary). This function is idempotent *)
  val link : t -> unit

  val cleanup : t -> unit

  val socket : t -> Path.t
end = struct
  (* Maximum path before we resort to the trick *)
  let max_path_length = 100

  type t =
    { symlink : Path.t option
    ; socket : Path.t
    ; cleanup : unit Lazy.t
    ; link : unit Lazy.t
    }

  let link t = Lazy.force t.link

  let make_cleanup ~symlink ~socket =
    lazy
      (Option.iter symlink ~f:Path.unlink_no_err;
       Path.unlink_no_err socket)

  let cleanup t = Lazy.force t.cleanup

  let create desired_path =
    let desired_path_s = Path.to_absolute_filename desired_path in
    if String.length desired_path_s <= max_path_length then
      let cleanup = make_cleanup ~symlink:None ~socket:desired_path in
      { symlink = None; socket = desired_path; cleanup; link = lazy () }
    else
      let socket =
        let dir =
          Path.of_string
            (match Xdg.runtime_dir (Lazy.force Dune_util.xdg) with
            | None -> Filename.get_temp_dir_name ()
            | Some p ->
              let p = Filename.concat p "dune/s" in
              (match Fpath.mkdir_p p with
              | Already_exists
              | Created ->
                ());
              p)
        in
        Temp.temp_file ~dir ~prefix:"" ~suffix:"dune"
      in
      let link =
        let dest =
          let from = Path.external_ (Path.External.cwd ()) in
          Path.mkdir_p (Path.parent_exn desired_path);
          Path.reach_for_running ~from desired_path
        in
        lazy (Unix.symlink (Path.to_string socket) dest)
      in
      let cleanup = make_cleanup ~symlink:(Some desired_path) ~socket in
      let t = { symlink = Some desired_path; socket; cleanup; link } in
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
      ; root : string
      }

let t_var : t Fiber.Var.t = Fiber.Var.create ()

module Server = Dune_rpc_server.Make (Csexp_rpc.Session)

let of_config config stats =
  match config with
  | Config.Client -> Client
  | Config.Server { handler; backlog; pool; root } ->
    let where = Where.default () in
    let real_where, symlink_socket =
      match where with
      | `Ip _ -> (Where.to_socket where, None)
      | `Unix path ->
        let path = Path.of_string path in
        let symlink_socket = Symlink_socket.create path in
        ( Unix.ADDR_UNIX (Path.to_string (Symlink_socket.socket symlink_socket))
        , Some symlink_socket )
    in
    let server = Csexp_rpc.Server.create real_where ~backlog in
    Server { server; handler; where; symlink_socket; stats; pool; root }

let run config stats =
  let t = of_config config stats in
  Fiber.Var.set t_var t (fun () ->
      match t with
      | Client -> Fiber.return ()
      | Server t ->
        let cleanup_registry = ref None in
        let run_cleanup_registry () =
          match !cleanup_registry with
          | None -> ()
          | Some path ->
            Fpath.unlink_no_err path;
            cleanup_registry := None
        in
        let with_print_errors f () =
          Fiber.with_error_handler f ~on_error:(fun exn ->
              Format.eprintf "%a@." Exn_with_backtrace.pp_uncaught exn;
              Exn_with_backtrace.reraise exn)
        in
        Fiber.finalize
          (with_print_errors (fun () ->
               let open Fiber.O in
               Fiber.fork_and_join_unit
                 (fun () ->
                   let* sessions = Csexp_rpc.Server.serve t.server in
                   Option.iter t.symlink_socket ~f:Symlink_socket.link;
                   let () =
                     let (`Caller_should_write { Registry.File.path; contents })
                         =
                       let registry_config =
                         Registry.Config.create (Lazy.force Dune_util.xdg)
                       in
                       let dune =
                         let pid = Unix.getpid () in
                         Registry.Dune.create ~where:t.where ~root:t.root ~pid
                       in
                       Registry.Config.register registry_config dune
                     in
                     let (_ : Fpath.mkdir_p_result) =
                       Fpath.mkdir_p (Filename.dirname path)
                     in
                     Io.String_path.write_file path contents;
                     cleanup_registry := Some path;
                     at_exit run_cleanup_registry
                   in
                   let* () = Server.serve sessions t.stats t.handler in
                   Fiber.Pool.stop t.pool)
                 (fun () -> Fiber.Pool.run t.pool)))
          ~finally:(fun () ->
            run_cleanup_registry ();
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
  let csexp_client p = Csexp_rpc.Client.create (Where.to_socket p)
end

let client ?handler p init ~f =
  let open Fiber.O in
  let* c = Connect.csexp_client p in
  let* session = Csexp_rpc.Client.connect_exn c in
  Client.connect ?handler session init ~f

let client_with_session ?handler init ~session ~f =
  Client.connect ?handler session init ~f
