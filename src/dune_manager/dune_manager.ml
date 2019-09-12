module Evt = Event
open Stdune
open Dune_memory
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
  ; build_root : Path.t option
  ; common_metadata : Dune_lang.t list
  ; memory : Dune_memory.Memory.t
  ; repositories : (string * string * string) list
  ; version : version option
  }

let default_port_file () =
  let runtime_dir =
    let xdg =
      try Sys.getenv "XDG_RUNTIME_DIR"
      with Not_found ->
        User_error.raise [ Pp.textf "XDG_RUNTIME_DIR is not set" ]
    in
    Filename.concat xdg "dune-manager"
  in
  Path.of_string (Filename.concat runtime_dir "port")

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

let make_path client path =
  if Filename.is_relative path then
    match client.build_root with
    | Some p ->
      Result.ok (Path.of_string (Filename.concat (Path.to_string p) path))
    | None -> Result.Error "relative path while no build root was set"
  else
    Result.ok (Path.of_string path)

let send client sexp =
  output_string client (Dune_lang.to_string sexp);
  output_char client '\n';
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
  Dune_lang.List
    ( Dune_lang.atom "lang"
    :: Dune_lang.atom "dune-memory-protocol"
    :: List.map my_versions ~f:Dune_lang.Syntax.Version.encode )

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
  let open Result.O in
  let invalid_args args =
    Result.Error
      (Printf.sprintf "invalid arguments:%s"
         (List.fold_left ~init:""
            ~f:(fun a b -> a ^ " " ^ b)
            (List.map ~f:Dune_lang.to_string args)))
  in
  let handle_lang client = function
    | Dune_lang.Atom (A "dune-memory-protocol") :: versions -> (
      let decode_version sexp =
        Dune_lang.Decoder.parse Dune_lang.Syntax.Version.decode Univ_map.empty
          (Dune_lang.Ast.add_loc sexp ~loc:Loc.none)
      in
      let versions = List.map ~f:decode_version versions in
      match find_highest_common_version my_versions versions with
      | None ->
        Unix.close client.fd;
        Result.Error "no compatible versions"
      | Some (major, minor) as v ->
        Log.infof "%s: negotiated version: %i.%i" (peer_name client.peer) major
          minor;
        Result.ok { client with version = v } )
    | args -> invalid_args args
  and handle_promote client = function
    | Dune_lang.List [ Dune_lang.Atom (A "key"); Dune_lang.Atom (A key) ]
      :: Dune_lang.List (Dune_lang.Atom (A "files") :: files)
         :: Dune_lang.List (Dune_lang.Atom (A "metadata") :: metadata) :: rest
      as cmd -> (
      let repo =
        match rest with
        | [] -> Result.ok None
        | [ Dune_lang.List
              [ Dune_lang.Atom (A "repo"); Dune_lang.Atom (A repo) ]
          ] -> (
          int_of_string ~where:"repository index" repo
          >>= fun repo ->
          try Result.Ok (List.nth client.repositories repo)
          with Failure _ ->
            Result.Error (Printf.sprintf "repository out of range: %i" repo) )
        | _ ->
          Result.Error
            (Printf.sprintf "invalid promotion message: %s"
               (Dune_lang.to_string (Dune_lang.List cmd)))
      and file = function
        | Dune_lang.List [ Dune_lang.Atom (A path); Dune_lang.Atom (A hash) ]
          ->
          make_path client path
          >>= fun path -> Dune_memory.key_of_string hash >>| fun d -> (path, d)
        | sexp ->
          Result.Error
            (Printf.sprintf "invalid file in promotion message: %s"
               (Dune_lang.to_string sexp))
      and f promotion =
        print_endline (Dune_memory.promotion_to_string promotion);
        match promotion with
        | Already_promoted (f, t) ->
          Some
            (Dune_lang.List
               [ Dune_lang.atom (Path.to_string f)
               ; Dune_lang.atom (Path.to_string t)
               ])
        | _ -> None
      in
      repo
      >>= fun repo ->
      Result.List.map ~f:file files
      >>= fun files ->
      Dune_memory.key_of_string key
      >>= fun key ->
      Dune_memory.Memory.promote client.memory files key
        (metadata @ client.common_metadata)
        (Option.map ~f:(fun (_, remote, commit) -> (remote, commit)) repo)
      >>| fun promotions ->
      match List.filter_map ~f promotions with
      | [] -> client
      | dedup ->
        send client.output (List (Dune_lang.atom "dedup" :: dedup));
        client )
    | args -> invalid_args args
  and handle_set_root client = function
    | [ Dune_lang.Atom (A dir) ] ->
      Result.map_error
        ~f:(function
          | _ ->
            send client.output
              (Dune_lang.List
                 [ Dune_lang.atom "cannot-read-dune-memory"
                 ; Dune_lang.List
                     [ Dune_lang.atom "supported-formats"; Dune_lang.atom "v2" ]
                 ]);
            "unable to read Dune memory")
        ( Dune_memory.make ~root:(Path.of_string dir) ()
        >>| fun memory -> { client with memory } )
    | args -> invalid_args args
  and handle_set_build_root client = function
    | [ Dune_lang.Atom (A dir) ] ->
      Result.ok { client with build_root = Some (Path.of_string dir) }
    | args -> invalid_args args
  and handle_set_metadata client arg =
    Result.ok { client with common_metadata = arg }
  and handle_set_repos client arg =
    let convert = function
      | Dune_lang.List
          [ Dune_lang.List [ Dune_lang.Atom (A "dir"); Dune_lang.Atom (A dir) ]
          ; Dune_lang.List
              [ Dune_lang.Atom (A "remote"); Dune_lang.Atom (A remote) ]
          ; Dune_lang.List
              [ Dune_lang.Atom (A "commit_id"); Dune_lang.Atom (A commit) ]
          ] ->
        Result.ok (dir, remote, commit)
      | invalid ->
        Result.Error
          (Printf.sprintf "invalid repo: %s" (Dune_lang.to_string invalid))
    in
    Result.List.map ~f:convert arg
    >>| fun repositories -> { client with repositories }
  in
  let handle_cmd client = function
    | Dune_lang.List (Dune_lang.Atom (A cmd) :: args) ->
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
        (Printf.sprintf "invalid command format: %s" (Dune_lang.to_string cmd))
  in
  let input = Unix.in_channel_of_descr client.fd in
  let f () =
    send client.output my_versions_command;
    Log.infof "accept client: %s" (peer_name client.peer);
    let rec handle client =
      match input_line input with
      | exception End_of_file -> Log.infof "%s: ended" (peer_name client.peer)
      | line -> (
        match
          let cmd =
            Dune_lang.Parser.parse_string line ~fname:"<protocol>" ~mode:Single
            |> Dune_lang.Ast.remove_locs
          in
          Log.infof "%s: received command: %s" (peer_name client.peer)
            (Dune_lang.to_string cmd);
          handle_cmd client cmd
        with
        | Error e ->
          Log.infof "%s: command error: %s" (peer_name client.peer) e
        | Ok client -> handle client )
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
          ; build_root = None
          ; common_metadata = []
          ; repositories = []
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
  let handler _ =
    Log.info "caught signal";
    ignore (Thread.create stop manager)
  in
  (* Unfortunately it seems using Event prevents signals from working :-( *)
  Sys.set_signal Sys.sigint (Sys.Signal_handle handler);
  Sys.set_signal Sys.sigterm (Sys.Signal_handle handler);
  try run ~port_f:started manager
  with Error s ->
    Printf.fprintf stderr "%s: fatal error: %s\n%!" Sys.argv.(0) s;
    exit 1

let endpoint m = m.endpoint

let err msg = User_error.E (User_error.make [ Pp.text msg ])

module Client = struct
  type t =
    { socket : out_channel
    ; fd : Unix.file_descr
    ; memory : Dune_memory.Memory.t
    }

  let make () =
    let open Result.O in
    let* memory = Result.map_error ~f:err (Dune_memory.make ()) in
    let* port =
      let root = Dune_memory.default_root () in
      Daemonize.daemonize ~workdir:root (default_port_file ())
        (daemon ~root ~config:{ exit_no_client = true })
      >>| (function
            | Started (ep, _)
             |Already_running (ep, _) ->
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
    send socket my_versions_command;
    Result.Ok { socket; fd; memory }

  let promote client paths key metadata repo =
    let key = Dune_memory.key_to_string key
    and f (path, digest) =
      Dune_lang.List
        [ Dune_lang.atom (Path.reach ~from:Path.build_dir path)
        ; Dune_lang.atom (Digest.to_string digest)
        ]
    and repo =
      match repo with
      | Some idx ->
        [ Dune_lang.List
            [ Dune_lang.atom "repo"; Dune_lang.atom (string_of_int idx) ]
        ]
      | None -> []
    in
    send client.socket
      (Dune_lang.List
         ( Dune_lang.atom "promote"
         :: Dune_lang.List [ Dune_lang.atom "key"; Dune_lang.atom key ]
         :: Dune_lang.List (Dune_lang.atom "files" :: List.map ~f paths)
         :: Dune_lang.List (Dune_lang.atom "metadata" :: metadata)
         :: repo ));
    Result.Ok ()

  let set_build_dir client path =
    send client.socket
      (Dune_lang.List
         [ Dune_lang.atom "set-build-root"
         ; Dune_lang.atom (Path.to_absolute_filename path)
         ])

  let search client key = Dune_memory.Memory.search client.memory key

  let teardown memory =
    flush memory.socket;
    Unix.shutdown memory.fd Unix.SHUTDOWN_ALL
end
