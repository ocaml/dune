module Evt = Event
module Utils = Utils
open Stdune
open Utils
open Messages
open Result.O

type version = int * int

type config =
  { exit_no_client : bool
  ; duplication_mode : Dune_cache.duplication_mode option
  }

type event =
  | Stop
  | New_client of Unix.file_descr * Unix.sockaddr
  | Client_left of Unix.file_descr

type client =
  { fd : Unix.file_descr
  ; peer : Unix.sockaddr
  ; output : out_channel
  ; common_metadata : Sexp.t list
  ; cache : Dune_cache.Cache.t
  ; version : version option
  }

let default_port_file () =
  let runtime_dir =
    match Sys.getenv_opt "XDG_RUNTIME_DIR" with
    | Some p -> Path.relative (Path.of_string p) "dune-cache-daemon"
    | None ->
      (* The runtime directory is 0700 owned by the user for security reasons.
         Defaulting to a directory in the dune cache root makes sense in that
         regard, since if someone has access to this directory, it has access to
         the cache content, and having access to the socket does not make a
         difference. *)
      Path.relative (Dune_cache.default_root ()) "runtime"
  in
  Path.L.relative runtime_dir [ "dune-cache-daemon"; "port" ]

let max_port_size = 1024

let check_port_file ?(close = true) p =
  let p = Path.to_string p in
  match Result.try_with (fun () -> Unix.openfile p [ Unix.O_RDONLY ] 0o600) with
  | Result.Ok fd ->
    let f () =
      retry (fun () ->
          match Fcntl.lock_get fd Fcntl.Write with
          | None -> Some None
          | Some (Fcntl.Read, pid) -> Some (Some pid)
          | Some (Fcntl.Write, _) -> None)
      >>| Option.map ~f:(fun pid ->
              let buf = Bytes.make max_port_size ' ' in
              let read = Unix.read fd buf 0 max_port_size in
              (Bytes.sub_string buf ~pos:0 ~len:read, pid, fd))
    and finally () = if close then Unix.close fd in
    Exn.protect ~f ~finally
  | Result.Error (Unix.Unix_error (Unix.ENOENT, _, _)) -> Result.Ok None
  | Result.Error e -> Result.Error e

let send_sexp output sexp =
  output_string output (Csexp.to_string sexp);
  flush output

let send output message = send_sexp output (Messages.sexp_of_message message)

module ClientsKey = struct
  type t = Unix.file_descr

  let compare a b = Ordering.of_int (Stdlib.compare a b)

  let to_dyn _ = Dyn.Opaque
end

module Clients = Map.Make (ClientsKey)

type t =
  { root : Path.t option
  ; mutable socket : Unix.file_descr option
  ; mutable clients : (client * Thread.t) Clients.t
  ; mutable endpoint : string option
  ; mutable accept_thread : Thread.t option
  ; mutable trim_thread : Thread.t option
  ; config : config
  ; events : event Evt.channel
  ; cache : Dune_cache.Cache.t
  }

exception Error of string

let make ?root ~config () : t =
  match Dune_cache.Cache.make ?root (fun _ -> ()) with
  | Result.Error msg -> User_error.raise [ Pp.text msg ]
  | Result.Ok cache ->
    { root
    ; socket = None
    ; clients = Clients.empty
    ; endpoint = None
    ; accept_thread = None
    ; trim_thread = None
    ; config
    ; events = Evt.new_channel ()
    ; cache
    }

let getsockname = function
  | Unix.ADDR_UNIX _ ->
    User_error.raise
      [ Pp.textf "got a Unix socket connection on our TCP socket ?" ]
  | Unix.ADDR_INET (addr, port) -> (addr, port)

let peer_name s =
  let addr, port = getsockname s in
  Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

let stop daemon = Evt.sync (Evt.send daemon.events Stop)

let my_versions : version list = [ (1, 0) ]

let my_versions_command =
  Messages.Lang
    (List.map ~f:(fun (major, minor) -> { Messages.major; minor }) my_versions)

let find_highest_common_version (a : version list) (b : version list) :
    version option =
  let a = Int.Map.of_list_exn a
  and b = Int.Map.of_list_exn b in
  let common =
    Int.Map.merge
      ~f:(fun _ minor_in_a minor_in_b ->
        match (minor_in_a, minor_in_b) with
        | Some a, Some b -> Some (min a b)
        | _ -> None)
      a b
  in
  Int.Map.max_binding common

let endpoint m = m.endpoint

let err msg = User_error.E (User_error.make [ Pp.text msg ])

let errf msg = User_error.E (User_error.make msg)

module Client = struct
  type t =
    { socket : out_channel
    ; fd : Unix.file_descr
    ; input : char Stream.t
    ; cache : Dune_cache.Cache.t
    ; thread : Thread.t
    ; finally : (unit -> unit) option
    }

  let read input =
    let* sexp = Csexp.parse input in
    Messages.incoming_message_of_sexp sexp >>| function
    | Messages.Dedup v -> Dune_cache.Dedup v

  let client_handle output = function
    | Dune_cache.Dedup f -> send output (Messages.Dedup f)

  (* FIXME *)

  let client_thread (events, (client : client)) =
    try
      let handle_cmd client sexp =
        let* msg = Messages.outgoing_message_of_sexp sexp in
        let* () =
          match msg with
          | Lang _ -> Result.Ok ()
          | _ when Option.is_none client.version ->
            Unix.shutdown client.fd Unix.SHUTDOWN_ALL;
            Result.Error "version was not negotiated"
          | _ -> Result.Ok ()
        in
        match msg with
        | Lang versions -> (
          match
            find_highest_common_version my_versions
              (List.map
                 ~f:(fun (v : Messages.version) -> (v.major, v.minor))
                 versions)
          with
          | None ->
            Unix.close client.fd;
            Result.Error "no compatible versions"
          | Some (major, minor) as v ->
            Log.infof "%s: negotiated version: %i.%i" (peer_name client.peer)
              major minor;
            Result.ok { client with version = v } )
        | Promote promotion ->
          let+ () =
            Dune_cache.Cache.promote client.cache promotion.files promotion.key
              (promotion.metadata @ client.common_metadata)
              ~repository:promotion.repository
          in
          client
        | SetBuildRoot root ->
          Result.Ok
            { client with
              cache = Dune_cache.Cache.set_build_dir client.cache root
            }
        | SetCommonMetadata metadata ->
          Result.ok { client with common_metadata = metadata }
        | SetRepos repositories ->
          let cache =
            Dune_cache.Cache.with_repositories client.cache repositories
          in
          Result.Ok { client with cache }
      in
      let input = Stream.of_channel (Unix.in_channel_of_descr client.fd) in
      let f () =
        send client.output my_versions_command;
        Log.infof "accept client: %s" (peer_name client.peer);
        let rec handle client =
          match Stream.peek input with
          | None -> Log.infof "%s: ended" (peer_name client.peer)
          | Some '\n' ->
            (* Skip toplevel newlines, for easy netcat interaction *)
            Stream.junk input;
            (handle [@tailcall]) client
          | _ -> (
            match
              let* cmd =
                Result.map_error
                  ~f:(fun r -> "parse error: " ^ r)
                  (Csexp.parse input)
              in
              Log.infof "%s: received command: %s" (peer_name client.peer)
                (Sexp.to_string cmd);
              handle_cmd client cmd
            with
            | Result.Error e ->
              Log.infof "%s: command error: %s" (peer_name client.peer) e;
              handle client
            | Result.Ok client -> handle client )
        in
        handle client
      and finally () =
        ( try
            Unix.shutdown client.fd Unix.SHUTDOWN_ALL;
            Unix.close client.fd
          with Unix.Unix_error (Unix.ENOTCONN, _, _) -> () );
        Evt.sync (Evt.send events (Client_left client.fd))
      in
      try Exn.protect ~f ~finally with
      | Unix.Unix_error (Unix.EBADF, _, _) ->
        Log.infof "%s: ended" (peer_name client.peer)
      | Sys_error msg -> Log.infof "%s: ended: %s" (peer_name client.peer) msg
    with Code_error.E e as exn ->
      Log.infof "%s: fatal error: %a" (peer_name client.peer)
        Pp.render_ignore_tags
        (Dyn.pp (Code_error.to_dyn e));
      raise exn

  let run ?(port_f = ignore) ?(port = 0) daemon =
    let trim_thread max_size period cache =
      let rec trim () =
        Unix.sleep period;
        let () =
          match
            let size = Dune_cache.size cache in
            if size > max_size then (
              Log.infof "trimming %i bytes" (size - max_size);
              Some (Dune_cache.trim cache (size - max_size))
            ) else
              None
          with
          | Some { trimmed_files_size = freed; _ } ->
            Log.infof "trimming freed %i bytes" freed
          | None -> Log.infof "skip trimming"
        in
        trim ()
      in
      trim ()
    in
    let rec accept_thread sock =
      let rec accept () =
        try Unix.accept sock
        with Unix.Unix_error (Unix.EINTR, _, _) -> (accept [@tailcall]) ()
      in
      let fd, peer = accept () in
      ( try Evt.sync (Evt.send daemon.events (New_client (fd, peer)))
        with Unix.Unix_error (Unix.EBADF, _, _) -> () );
      (accept_thread [@tailcall]) sock
    in
    let f () =
      let+ trim_period =
        Option.value
          ~default:(Result.Ok (10 * 60))
          (Option.map ~f:int_of_string
             (Env.get Env.initial "DUNE_CACHE_TRIM_PERIOD"))
      and+ trim_size =
        Option.value
          ~default:(Result.Ok (10 * 1024 * 1024 * 1024))
          (Option.map ~f:int_of_string
             (Env.get Env.initial "DUNE_CACHE_TRIM_SIZE"))
      in
      let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      daemon.socket <- Some sock;
      Unix.bind sock
        (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", port));
      let addr, port = getsockname (Unix.getsockname sock) in
      let endpoint =
        Printf.sprintf "%s:%i" (Unix.string_of_inet_addr addr) port
      in
      daemon.endpoint <- Some endpoint;
      port_f endpoint;
      Unix.listen sock 1024;
      daemon.accept_thread <- Some (Thread.create accept_thread sock);
      daemon.trim_thread <-
        Some (Thread.create (trim_thread trim_size trim_period) daemon.cache);
      let rec handle () =
        let stop () =
          match daemon.socket with
          | Some fd ->
            daemon.socket <- None;
            let clean f = ignore (Clients.iter ~f daemon.clients) in
            clean (fun (client, _) -> Unix.shutdown client.fd Unix.SHUTDOWN_ALL);
            clean (fun (_, tid) -> Thread.join tid);
            clean (fun (client, _) -> Unix.close client.fd);
            Unix.close fd
          | _ -> Log.infof "stop"
        in
        ( match Evt.sync (Evt.receive daemon.events) with
        | Stop -> stop ()
        | New_client (fd, peer) ->
          let output = Unix.out_channel_of_descr fd in
          let client =
            { fd
            ; peer
            ; output
            ; version = None
            ; common_metadata = []
            ; cache =
                ( match
                    Dune_cache.Cache.make ?root:daemon.root
                      (client_handle output)
                  with
                | Result.Ok m -> m
                | Result.Error e -> User_error.raise [ Pp.textf "%s" e ] )
            }
          in
          let tid = Thread.create client_thread (daemon.events, client) in
          daemon.clients <-
            ( match Clients.add daemon.clients client.fd (client, tid) with
            | Result.Ok v -> v
            | Result.Error _ -> User_error.raise [ Pp.textf "duplicate socket" ]
            )
        | Client_left fd ->
          daemon.clients <- Clients.remove daemon.clients fd;
          if daemon.config.exit_no_client && Clients.is_empty daemon.clients
          then
            stop () );
        if Option.is_some daemon.socket then (handle [@tailcall]) ()
      in
      handle ()
    in
    match f () with
    | Result.Ok () -> ()
    | Result.Error msg -> User_error.raise [ Pp.text msg ]
    | exception Unix.Unix_error (errno, f, _) ->
      User_error.raise
        [ Pp.textf "unable to %s: %s\n" f (Unix.error_message errno) ]

  let daemon ~root ~config started =
    Path.mkdir_p root;
    let log_file = Path.relative root "log" in
    Log.init ~file:(This log_file) ();
    let daemon = make ~root ~config () in
    (* Event blocks signals when waiting. Use a separate thread to catch
       signals. *)
    let signal_handler s =
      Log.infof "caught signal %i, exiting" s;
      ignore (Thread.create stop daemon)
    and signals = [ Sys.sigint; Sys.sigterm ] in
    let rec signals_handler () =
      signal_handler (Thread.wait_signal signals);
      signals_handler ()
    in
    ignore (Thread.sigmask Unix.SIG_BLOCK signals);
    ignore (Thread.create signals_handler ());
    try run ~port_f:started daemon
    with Error s ->
      Printf.fprintf stderr "%s: fatal error: %s\n%!" Sys.argv.(0) s;
      exit 1

  let make ?finally handle =
    (* This is a bit ugly as it is global, but flushing a closed socket will
       nuke the program if we don't. *)
    let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore in
    let* cache = Result.map_error ~f:err (Dune_cache.Cache.make ignore) in
    let* port =
      let cmd =
        Format.sprintf "%s cache start --display progress --exit-no-client"
          Sys.executable_name
      and f stdout =
        match Io.input_lines stdout with
        | [] -> Result.Error (err "empty output starting cache")
        | [ line ] -> Result.Ok line
        | _ -> Result.Error (err "unrecognized output starting cache")
      and finally stdout = ignore (Unix.close_process_in stdout) (* FIXME *) in
      Exn.protectx (Unix.open_process_in cmd) ~finally ~f
    in
    let* addr, port =
      match String.split_on_char ~sep:':' port with
      | [ addr; port ] -> (
        match Int.of_string port with
        | Some i -> (
          try Result.Ok (Unix.inet_addr_of_string addr, i)
          with Failure _ ->
            Result.Error (errf [ Pp.textf "invalid address: %s" addr ]) )
        | None -> Result.Error (errf [ Pp.textf "invalid port: %s" port ]) )
      | _ -> Result.Error (errf [ Pp.textf "invalid endpoint: %s" port ])
    in
    let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let* _ =
      Result.try_with (fun () -> Unix.connect fd (Unix.ADDR_INET (addr, port)))
    in
    let socket = Unix.out_channel_of_descr fd in
    let input = Stream.of_channel (Unix.in_channel_of_descr fd) in
    let rec thread input =
      match
        let+ command = read input in
        Log.infof "dune-cache command: %a" Pp.render_ignore_tags
          (Dyn.pp (Dune_cache.command_to_dyn command));
        handle command
      with
      | Result.Error e ->
        Log.infof "dune-cache read error: %s" e;
        Option.iter ~f:(fun f -> f ()) finally
      | Result.Ok () -> (thread [@tailcall]) input
    in
    send socket my_versions_command;
    (* FIXME: find highest common version *)
    ignore (read input);
    let thread = Thread.create thread input in
    Result.Ok { socket; fd; input; cache; thread; finally }

  let with_repositories client repositories =
    send client.socket (Messages.SetRepos repositories);
    client

  let promote client files key metadata ~repository =
    try
      send client.socket (Messages.Promote { key; files; metadata; repository });
      Result.Ok ()
    with Sys_error (* "Broken_pipe" *) _ ->
      Result.Error "lost connection to cache daemon"

  let set_build_dir client path =
    send client.socket (Messages.SetBuildRoot path);
    client

  let search client key = Dune_cache.Cache.search client.cache key

  let retrieve client key = Dune_cache.Cache.retrieve client.cache key

  let teardown client =
    ( try Unix.shutdown client.fd Unix.SHUTDOWN_SEND
      with Unix.Unix_error (Unix.ENOTCONN, _, _) -> () );
    Thread.join client.thread
end

let run = Client.run

let daemon = Client.daemon
