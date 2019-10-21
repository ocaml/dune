module Evt = Event
open Stdune
module Utils = Utils
open Utils

type version = int * int

type config = { exit_no_client : bool }

type event =
  | Stop
  | New_client of Unix.file_descr * Unix.sockaddr
  | Client_left of Unix.file_descr

type client =
  { fd : Unix.file_descr
  ; peer : Unix.sockaddr
  ; output : out_channel
  ; common_metadata : Sexp.t list
  ; memory : Dune_memory.Memory.t
  ; version : version option
  }

let default_port_file () =
  let runtime_dir =
    match Sys.getenv_opt "XDG_RUNTIME_DIR" with
    | Some p -> Path.L.relative (Path.of_string p) [ "dune-cache-daemon" ]
    | None ->
      (* The runtime directory is 0700 owned by the user for security reasons.
         Defaulting to a directory in the dune cache root makes sense in that
         regard, since if someone has access to this directory, it has access
         to the cache content, and having access to the socket does not make a
         difference. *)
      Path.L.relative (Dune_memory.default_root ()) [ "runtime" ]
  in
  Path.L.relative runtime_dir [ "dune-cache-daemon"; "port" ]

let max_port_size = 1024

let check_port_file ?(close = true) p =
  let p = Path.to_string p in
  match
    Result.try_with (fun () -> Unix.openfile p [ Unix.O_RDONLY ] 0o600)
  with
  | Result.Ok fd ->
    let f () =
      let open Result.O in
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

let send client sexp =
  output_string client (Csexp.to_string sexp);
  (* We need to flush when sending the version. Other instances are more
     debatable. *)
  flush client

module ClientsKey = struct
  type t = Unix.file_descr

  let compare a b = Ordering.of_int (Pervasives.compare a b)

  let to_dyn _ = Dyn.Opaque
end

module Clients = Map.Make (ClientsKey)

type t =
  { root : Path.t option
  ; mutable socket : Unix.file_descr option
  ; mutable clients : (client * Thread.t) Clients.t
  ; mutable endpoint : string option
  ; mutable accept_thread : Thread.t option
  ; config : config
  ; events : event Evt.channel
  }

exception Error of string

let make ?root ~config () : t =
  { root
  ; socket = None
  ; clients = Clients.empty
  ; endpoint = None
  ; accept_thread = None
  ; config
  ; events = Evt.new_channel ()
  }

let getsockname = function
  | Unix.ADDR_UNIX _ ->
    User_error.raise
      [ Pp.textf "got a Unix socket connection on our TCP socket ?" ]
  | Unix.ADDR_INET (addr, port) -> (addr, port)

let peer_name s =
  let addr, port = getsockname s in
  Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

let stop manager = Evt.sync (Evt.send manager.events Stop)

let my_versions : version list = [ (1, 0) ]

let my_versions_command =
  Sexp.List
    ( Sexp.Atom "lang" :: Sexp.Atom "dune-memory-protocol"
    :: (List.map ~f:(function maj, min ->
            Sexp.List
              [ Sexp.Atom (string_of_int maj); Sexp.Atom (string_of_int min) ]))
         my_versions )

module Int_map = Map.Make (Int)

let find_highest_common_version (a : version list) (b : version list) :
    version option =
  let a = Int_map.of_list_exn a
  and b = Int_map.of_list_exn b in
  let common =
    Int_map.merge
      ~f:(fun _ minor_in_a minor_in_b ->
        match (minor_in_a, minor_in_b) with
        | Some a, Some b -> Some (min a b)
        | _ -> None)
      a b
  in
  Int_map.max_binding common

let int_of_string ?where s =
  try Result.Ok (int_of_string s)
  with Failure _ ->
    Result.Error
      (Printf.sprintf "invalid integer%s: %s"
         ( match where with
         | Some l -> " in " ^ l
         | None -> "" )
         s)

let client_thread (events, client) =
  try
    let open Result.O in
    let invalid_args args =
      Result.Error
        (Printf.sprintf "invalid arguments:%s"
           (List.fold_left ~init:""
              ~f:(fun a b -> a ^ " " ^ b)
              (List.map ~f:Sexp.to_string args)))
    in
    let handle_lang client = function
      | Sexp.Atom "dune-memory-protocol" :: versions -> (
        let decode_version = function
          | Sexp.List [ Sexp.Atom major; Sexp.Atom minor ] ->
            int_of_string ~where:"lang command version" major
            >>= fun major ->
            int_of_string ~where:"lang command version" minor
            >>| fun minor -> (major, minor)
          | v ->
            Result.Error
              (Printf.sprintf "invalid version in lang command: %s"
                 (Sexp.to_string v))
        in
        Result.List.map ~f:decode_version versions
        >>| find_highest_common_version my_versions
        >>= function
        | None ->
          Unix.close client.fd;
          Result.Error "no compatible versions"
        | Some (major, minor) as v ->
          Log.infof "%s: negotiated version: %i.%i" (peer_name client.peer)
            major minor;
          Result.ok { client with version = v } )
      | args -> invalid_args args
    and handle_promote client = function
      | Sexp.List [ Sexp.Atom "key"; Sexp.Atom key ]
        :: Sexp.List (Sexp.Atom "files" :: files)
           :: Sexp.List [ Sexp.Atom "metadata"; Sexp.List metadata ] :: rest as
        cmd -> (
        let repo =
          match rest with
          | [] -> Result.Ok None
          | [ Sexp.List [ Sexp.Atom "repo"; Sexp.Atom repo ] ] ->
            Result.map ~f:Option.some
              (int_of_string ~where:"repository index" repo)
          | _ ->
            Result.Error
              (Printf.sprintf "invalid promotion message: %s"
                 (Sexp.to_string (Sexp.List cmd)))
        and file = function
          | Sexp.List [ Sexp.Atom path; Sexp.Atom hash ] ->
            Dune_memory.key_of_string hash
            >>| fun d -> (Path.Build.of_local (Path.Local.of_string path), d)
          | sexp ->
            Result.Error
              (Printf.sprintf "invalid file in promotion message: %s"
                 (Sexp.to_string sexp))
        and f promotion =
          print_endline (Dune_memory.promotion_to_string promotion);
          match promotion with
          | Already_promoted (f, t, d) ->
            Some
              (Sexp.List
                 [ Sexp.Atom (Path.Local.to_string (Path.Build.local f))
                 ; Sexp.Atom (Path.to_string t)
                 ; Sexp.Atom (Digest.to_string d)
                 ])
          | _ -> None
        in
        repo
        >>= fun repo ->
        Result.List.map ~f:file files
        >>= fun files ->
        Dune_memory.key_of_string key
        >>= fun key ->
        Dune_memory.Memory.promote_sync client.memory files key
          (metadata @ client.common_metadata)
          repo
        >>| fun promotions ->
        match List.filter_map ~f promotions with
        | [] -> client
        | dedup ->
          Log.infof "send deduplication message for %s"
            (Sexp.to_string (Sexp.List dedup));
          send client.output (Sexp.List (Sexp.Atom "dedup" :: dedup));
          client )
      | args -> invalid_args args
    and handle_set_root client = function
      | [ Sexp.Atom dir ] ->
        Result.map_error
          ~f:(function
            | _ ->
              send client.output
                (Sexp.List
                   [ Sexp.Atom "cannot-read-dune-memory"
                   ; Sexp.List [ Sexp.Atom "supported-formats"; Sexp.Atom "v2" ]
                   ]);
              "unable to read Dune memory")
          ( Dune_memory.make ~root:(Path.of_string dir) ()
          >>| fun memory -> { client with memory } )
      | args -> invalid_args args
    and handle_set_build_root client = function
      | [ Sexp.Atom dir ] ->
        Result.ok
          { client with
            memory =
              Dune_memory.Memory.set_build_dir client.memory
                (Path.of_string dir)
          }
      | args -> invalid_args args
    and handle_set_metadata client arg =
      Result.ok { client with common_metadata = arg }
    and handle_set_repos client arg =
      let convert = function
        | Sexp.List
            [ Sexp.List [ Sexp.Atom "dir"; Sexp.Atom directory ]
            ; Sexp.List [ Sexp.Atom "remote"; Sexp.Atom remote ]
            ; Sexp.List [ Sexp.Atom "commit_id"; Sexp.Atom commit ]
            ] ->
          Result.ok { Dune_memory.Memory.directory; remote; commit }
        | invalid ->
          Result.Error
            (Printf.sprintf "invalid repo: %s" (Sexp.to_string invalid))
      in
      Result.List.map ~f:convert arg
      >>| fun repositories ->
      let memory =
        Dune_memory.Memory.with_repositories client.memory repositories
      in
      { client with memory }
    in
    let handle_cmd client = function
      | Sexp.List (Sexp.Atom cmd :: args) ->
        if cmd <> "lang" && Option.is_none client.version then (
          Unix.shutdown client.fd Unix.SHUTDOWN_ALL;
          Result.Error "version was not negotiated"
        ) else
          Result.map_error
            ~f:(fun s -> cmd ^ ": " ^ s)
            ( match cmd with
            | "lang" -> handle_lang client args
            | "promote" -> handle_promote client args
            | "set-build-root" -> handle_set_build_root client args
            | "set-common-metadata" -> handle_set_metadata client args
            | "set-dune-memory-root" -> handle_set_root client args
            | "set-repos" -> handle_set_repos client args
            | _ -> Result.Error (Printf.sprintf "unknown command: %s" cmd) )
      | cmd ->
        Result.Error
          (Printf.sprintf "invalid command format: %s" (Sexp.to_string cmd))
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
          let open Result.O in
          match
            Result.map_error
              ~f:(fun r -> "parse error: " ^ r)
              (Csexp.parse input)
            >>= fun cmd ->
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

let run ?(port_f = ignore) ?(port = 0) manager =
  let rec accept_thread sock =
    let rec accept () =
      try Unix.accept sock
      with Unix.Unix_error (Unix.EINTR, _, _) -> (accept [@tailcall]) ()
    in
    let fd, peer = accept () in
    ( try Evt.sync (Evt.send manager.events (New_client (fd, peer)))
      with Unix.Unix_error (Unix.EBADF, _, _) -> () );
    (accept_thread [@tailcall]) sock
  in
  let f () =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    manager.socket <- Some sock;
    Unix.bind sock
      (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", port));
    let addr, port = getsockname (Unix.getsockname sock) in
    let endpoint =
      Printf.sprintf "%s:%i" (Unix.string_of_inet_addr addr) port
    in
    manager.endpoint <- Some endpoint;
    port_f endpoint;
    Unix.listen sock 1024;
    manager.accept_thread <- Some (Thread.create accept_thread sock);
    let rec handle () =
      let stop () =
        match manager.socket with
        | Some fd ->
          manager.socket <- None;
          let clean f = ignore (Clients.iter ~f manager.clients) in
          clean (fun (client, _) -> Unix.shutdown client.fd Unix.SHUTDOWN_ALL);
          clean (fun (_, tid) -> Thread.join tid);
          clean (fun (client, _) -> Unix.close client.fd);
          Unix.close fd
        | _ -> Log.infof "stop"
      in
      ( match Evt.sync (Evt.receive manager.events) with
      | Stop -> stop ()
      | New_client (fd, peer) ->
        let client =
          { fd
          ; peer
          ; output = Unix.out_channel_of_descr fd
          ; version = None
          ; common_metadata = []
          ; memory =
              ( match Dune_memory.make ?root:manager.root () with
              | Result.Ok m -> m
              | Result.Error e -> User_error.raise [ Pp.textf "%s" e ] )
          }
        in
        let tid = Thread.create client_thread (manager.events, client) in
        manager.clients <-
          ( match Clients.add manager.clients client.fd (client, tid) with
          | Result.Ok v -> v
          | Result.Error _ -> User_error.raise [ Pp.textf "duplicate socket" ]
          )
      | Client_left fd ->
        manager.clients <- Clients.remove manager.clients fd;
        if manager.config.exit_no_client && Clients.is_empty manager.clients
        then
          stop () );
      if Option.is_some manager.socket then (handle [@tailcall]) ()
    in
    handle ()
  in
  try f ()
  with Unix.Unix_error (errno, f, _) ->
    User_error.raise
      [ Pp.textf "unable to %s: %s\n" f (Unix.error_message errno) ]

let daemon ~root ~config started =
  Path.mkdir_p root;
  let log_file = Path.relative root "log" in
  Log.init ~file:(This log_file) ();
  let manager = make ~root ~config () in
  (* Event blocks signals when waiting. Use a separate thread to catch signals. *)
  let signal_handler s =
    Log.infof "caught signal %i, exiting" s;
    ignore (Thread.create stop manager)
  and signals = [ Sys.sigint; Sys.sigterm ] in
  let rec signals_handler () =
    signal_handler (Thread.wait_signal signals);
    signals_handler ()
  in
  ignore (Thread.sigmask Unix.SIG_BLOCK signals);
  ignore (Thread.create signals_handler ());
  try run ~port_f:started manager
  with Error s ->
    Printf.fprintf stderr "%s: fatal error: %s\n%!" Sys.argv.(0) s;
    exit 1

let endpoint m = m.endpoint

let err msg = User_error.E (User_error.make [ Pp.text msg ])

module Client = struct
  include Dune_memory.MemoryTypes

  type t =
    { socket : out_channel
    ; fd : Unix.file_descr
    ; input : char Stream.t
    ; memory : Dune_memory.Memory.t
    ; thread : Thread.t
    ; finally : (unit -> unit) option
    }

  type command = Dedup of (Path.Build.t * Path.t * Digest.t)

  let command_to_dyn = function
    | Dedup (source, target, hash) ->
      let open Dyn.Encoder in
      constr "Dedup"
        [ Path.Build.to_dyn source; Path.to_dyn target; Digest.to_dyn hash ]

  let read input =
    let open Result.O in
    Csexp.parse input
    >>= function
    | Sexp.List
        [ Sexp.Atom "dedup"
        ; Sexp.List [ Sexp.Atom source; Sexp.Atom target; Sexp.Atom digest ]
        ] -> (
      match Digest.from_hex digest with
      | Some digest ->
        Result.Ok
          (Dedup (Path.Build.of_string source, Path.of_string target, digest))
      | None -> Result.Error (Printf.sprintf "invalid digest: %s" digest) )
    | exp ->
      Result.Error (Printf.sprintf "invalid command: %s" (Sexp.to_string exp))

  let make ?finally handle =
    (* This is a bit ugly as it is global, but flushing a closed socket will
       nuke the program if we don't. *)
    let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore in
    let open Result.O in
    let* memory = Result.map_error ~f:err (Dune_memory.make ()) in
    let* port =
      let root = Dune_memory.default_root () in
      Daemonize.daemonize ~workdir:root (default_port_file ())
        (daemon ~root ~config:{ exit_no_client = true })
      >>| (function
            | Started (ep, _)
            | Already_running (ep, _) ->
              ep
            | Finished ->
              Code_error.raise "dune-cache was run in the foreground" [])
      |> Result.map_error ~f:err
    in
    let* addr, port =
      match String.split_on_char ~sep:':' port with
      | [ addr; port ] -> (
        match int_of_string_opt port with
        | Some i -> Result.Ok (Unix.inet_addr_of_string addr, i)
        | None -> Result.Error (err (Printf.sprintf "invalid port: %s" port)) )
      | _ -> Result.Error (err (Printf.sprintf "invalid endpoint: %s" port))
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
          (Dyn.pp (command_to_dyn command));
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
    Result.Ok { socket; fd; input; memory; thread; finally }

  let with_repositories client repos =
    let repos =
      let f { directory; remote; commit } =
        Sexp.List
          [ Sexp.List [ Sexp.Atom "dir"; Sexp.Atom directory ]
          ; Sexp.List [ Sexp.Atom "remote"; Sexp.Atom remote ]
          ; Sexp.List [ Sexp.Atom "commit_id"; Sexp.Atom commit ]
          ]
      in
      List.map ~f repos
    in
    send client.socket (Sexp.List (Sexp.Atom "set-repos" :: repos));
    client

  let promote client paths key metadata repo =
    let key = Dune_memory.key_to_string key
    and f (path, digest) =
      Sexp.List
        [ Sexp.Atom (Path.Local.to_string (Path.Build.local path))
        ; Sexp.Atom (Digest.to_string digest)
        ]
    and repo =
      match repo with
      | Some idx ->
        [ Sexp.List [ Sexp.Atom "repo"; Sexp.Atom (string_of_int idx) ] ]
      | None -> []
    in
    try
      send client.socket
        (Sexp.List
           ( Sexp.Atom "promote"
           :: Sexp.List [ Sexp.Atom "key"; Sexp.Atom key ]
           :: Sexp.List (Sexp.Atom "files" :: List.map ~f paths)
           :: Sexp.List [ Sexp.Atom "metadata"; Sexp.List metadata ]
           :: repo ));
      Result.Ok ()
    with Sys_error (* "Broken_pipe" *) _ ->
      Result.Error "lost connection to cache daemon"

  let set_build_dir client path =
    send client.socket
      (Sexp.List
         [ Sexp.Atom "set-build-root"
         ; Sexp.Atom (Path.to_absolute_filename path)
         ]);
    client

  let search client key = Dune_memory.Memory.search client.memory key

  let teardown client =
    ( try Unix.shutdown client.fd Unix.SHUTDOWN_SEND
      with Unix.Unix_error (Unix.ENOTCONN, _, _) -> () );
    Thread.join client.thread
end
