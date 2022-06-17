open Import
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

type t =
  | Client
  | Server of
      { server : Csexp_rpc.Server.t
      ; handler : Dune_rpc_server.t
      ; pool : Fiber.Pool.t
      ; where : Dune_rpc_private.Where.t
      ; stats : Dune_stats.t option
      ; root : string
      }

let t_var : t Fiber.Var.t = Fiber.Var.create ()

module Server = Dune_rpc_server.Make (Csexp_rpc.Session)

let of_config (config : Config.t) stats =
  match config with
  | Client -> Client
  | Server { handler; backlog; pool; root } ->
    let where = Where.default () in
    let server = Csexp_rpc.Server.create (Where.to_socket where) ~backlog in
    Server { server; handler; where; stats; pool; root }

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
                   let () =
                     let (`Caller_should_write { Registry.File.path; contents })
                         =
                       let registry_config =
                         Registry.Config.create (Lazy.force Dune_util.xdg)
                       in
                       let dune =
                         let pid = Unix.getpid () in
                         let where =
                           match t.where with
                           | `Ip (host, port) -> `Ip (host, port)
                           | `Unix a ->
                             `Unix
                               (if Filename.is_relative a then
                                Filename.concat (Sys.getcwd ()) a
                               else a)
                         in
                         Registry.Dune.create ~where ~root:t.root ~pid
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
            Fiber.return ()))

let stop () =
  let open Fiber.O in
  let* t = Fiber.Var.get_exn t_var in
  match t with
  | Client -> Code_error.raise "rpc not running" []
  | Server s ->
    Csexp_rpc.Server.stop s.server;
    Fiber.return ()
