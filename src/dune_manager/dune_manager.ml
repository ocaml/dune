open Stdune
open Dune_memory
module Utils = Utils
open Utils

type version = int * int

type client =
  { fd : Unix.file_descr
  ; peer : Unix.sockaddr
  ; output : out_channel
  ; mutable build_root : Path.t option (* client owned *)
  ; mutable common_metadata : Sexp.t list (* client owned *)
  ; mutable memory : Dune_memory.Memory.t (* client owned*)
  ; mutable repositories : (string * string * string) list (* client owned *)
  ; mutable version : version option (* client owned*)
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
            | None ->
                Some None
            | Some (Fcntl.Read, pid) ->
                Some (Some pid)
            | Some (Fcntl.Write, _) ->
                None)
        >>| Option.map ~f:(fun pid ->
                let buf = Bytes.make max_port_size ' ' in
                let read = Unix.read fd buf 0 max_port_size in
                (Bytes.sub_string buf ~pos:0 ~len:read, pid, fd))
      and finally () = if close then Unix.close fd in
      Exn.protect ~f ~finally
  | Result.Error (Unix.Unix_error (Unix.ENOENT, _, _)) ->
      Result.Ok None
  | Result.Error e ->
      Result.Error e

let make_path client path =
  if Filename.is_relative path then
    match client.build_root with
    | Some p ->
        Result.ok (Path.of_string (Filename.concat (Path.to_string p) path))
    | None ->
        Result.Error "relative path while no build root was set"
  else
    Result.ok (Path.of_string path)

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
  }

exception Error of string

let make ?root () : t =
  { root; socket = None; clients = Clients.empty; endpoint = None }

let getsockname = function
  | Unix.ADDR_UNIX _ ->
      User_error.raise
        [ Pp.textf "got a Unix socket connection on our TCP socket ?" ]
  | Unix.ADDR_INET (addr, port) ->
      (addr, port)

let peer_name s =
  let addr, port = getsockname s in
  Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

exception Stop

let stop manager =
  match manager.socket with
  | Some fd ->
      Log.infof "stop";
      manager.socket <- None;
      let clean f = ignore (Clients.iter ~f manager.clients) in
      clean (fun (client, _) -> Unix.shutdown client.fd Unix.SHUTDOWN_ALL);
      clean (fun (_, tid) -> Thread.join tid);
      clean (fun (client, _) -> Unix.close client.fd);
      Unix.close fd
  | _ ->
      ()

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
        | Some a, Some b ->
            Some (min a b)
        | _ ->
            None)
      a b
  in
  Int_map.max_binding common

let int_of_string ?where s =
  try Result.Ok (int_of_string s)
  with Failure _ ->
    Result.Error
      (Printf.sprintf "invalid integer%s: %s"
         (match where with Some l -> " in " ^ l | None -> "")
         s)

let run ?(port_f = ignore) ?(port = 0) manager =
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
            client.version <- v;
            Result.ok () )
    | args ->
        invalid_args args
  and handle_promote client = function
    | Sexp.List [ Sexp.Atom "key"; Sexp.Atom key ]
      :: Sexp.List (Sexp.Atom "files" :: files)
         :: Sexp.List [ Sexp.Atom "metadata"; Sexp.List metadata ] :: rest as
      cmd -> (
        let repo =
          match rest with
          | [] ->
              Result.ok None
          | [ Sexp.List [ Sexp.Atom "repo"; Sexp.Atom repo ] ] -> (
              int_of_string ~where:"repository index" repo
              >>= fun repo ->
              try Result.Ok (List.nth client.repositories repo)
              with Failure _ ->
                Result.Error
                  (Printf.sprintf "repository out of range: %i" repo) )
          | _ ->
              Result.Error
                (Printf.sprintf "invalid promotion message: %s"
                   (Sexp.to_string (Sexp.List cmd)))
        and file = function
          | Sexp.List [ Sexp.Atom path; Sexp.Atom hash ] ->
              make_path client path
              >>= fun path ->
              Dune_memory.key_of_string hash >>| fun d -> (path, d)
          | sexp ->
              Result.Error
                (Printf.sprintf "invalid file in promotion message: %s"
                   (Sexp.to_string sexp))
        and f promotion =
          print_endline (Dune_memory.promotion_to_string promotion);
          match promotion with
          | Already_promoted (f, t) ->
              Some
                (Sexp.List
                   [ Sexp.Atom (Path.to_string f)
                   ; Sexp.Atom (Path.to_string t)
                   ])
          | _ ->
              None
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
        | [] ->
            ()
        | dedup ->
            send client.output (Sexp.List (Sexp.Atom "dedup" :: dedup)) )
    | args ->
        invalid_args args
  and handle_set_root client = function
    | [ Sexp.Atom dir ] ->
        Result.map_error
          ~f:(function
            | _ ->
                send client.output
                  (Sexp.List
                     [ Sexp.Atom "cannot-read-dune-memory"
                     ; Sexp.List
                         [ Sexp.Atom "supported-formats"; Sexp.Atom "v2" ]
                     ]);
                "unable to read Dune memory")
          ( Dune_memory.make ~root:(Path.of_string dir) ()
          >>| fun memory -> client.memory <- memory )
    | args ->
        invalid_args args
  and handle_set_build_root client = function
    | [ Sexp.Atom dir ] ->
        client.build_root <- Some (Path.of_string dir);
        Result.ok ()
    | args ->
        invalid_args args
  and handle_set_metadata client arg =
    client.common_metadata <- arg;
    Result.ok ()
  and handle_set_repos client arg =
    let convert = function
      | Sexp.List
          [ Sexp.List [ Sexp.Atom "dir"; Sexp.Atom dir ]
          ; Sexp.List [ Sexp.Atom "remote"; Sexp.Atom remote ]
          ; Sexp.List [ Sexp.Atom "commit_id"; Sexp.Atom commit ]
          ] ->
          Result.ok (dir, remote, commit)
      | invalid ->
          Result.Error
            (Printf.sprintf "invalid repo: %s" (Sexp.to_string invalid))
    in
    Result.List.map ~f:convert arg
    >>| fun repos -> client.repositories <- repos
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
            | "lang" ->
                handle_lang client args
            | "promote" ->
                handle_promote client args
            | "set-build-root" ->
                handle_set_build_root client args
            | "set-common-metadata" ->
                handle_set_metadata client args
            | "set-dune-memory-root" ->
                handle_set_root client args
            | "set-repos" ->
                handle_set_repos client args
            | _ ->
                Result.Error (Printf.sprintf "unknown command: %s" cmd) )
    | cmd ->
        Result.Error
          (Printf.sprintf "invalid command format: %s" (Sexp.to_string cmd))
  in
  let finally () = stop manager
  and f () =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let rec accept () =
      try Unix.accept sock with
      | Unix.Unix_error (Unix.EINTR, _, _) ->
          if Option.is_none manager.socket then
            raise Stop
          else
            accept ()
      | Unix.Unix_error (Unix.EBADF, _, _) as e ->
          if Option.is_none manager.socket then
            raise Stop
          else
            raise e
      | e ->
          raise e
    in
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
    while Option.is_some manager.socket do
      let client_thread client =
        let f () =
          send client.output my_versions_command;
          Log.infof "accept client: %s" (peer_name client.peer);
          let rec input =
            Stream.of_channel (Unix.in_channel_of_descr client.fd)
          and handle input =
            if Option.is_none manager.socket then
              ()
            else
              match Stream.peek input with
              | None ->
                  Log.infof "%s: ended" (peer_name client.peer)
              | Some '\n' ->
                  Stream.junk input;
                  (handle [@tailcall]) input
                  (* Skip toplevel newlines, for easy netcat interaction *)
              | _ ->
                  (let open Result.O in
                  match
                    Result.map_error
                      ~f:(fun r -> "parse error: " ^ r)
                      (Csexp.parse input)
                    >>= fun cmd ->
                    Log.infof "%s: received command: %s"
                      (peer_name client.peer) (Sexp.to_string cmd);
                    handle_cmd client cmd
                  with
                  | Result.Error e ->
                      Log.infof "%s: command error: %s" (peer_name client.peer)
                        e
                  | _ ->
                      ());
                  (handle [@tailcall]) input
          in
          handle input
        and finally () =
          ( try
              Unix.shutdown client.fd Unix.SHUTDOWN_ALL;
              Unix.close client.fd
            with
          | Unix.Unix_error (Unix.EBADF, _, _) ->
              Log.infof "%s: ended" (peer_name client.peer)
          | Sys_error msg ->
              Log.infof "%s: ended: %s" (peer_name client.peer) msg );
          manager.clients <- Clients.remove manager.clients client.fd
        in
        try Exn.protect ~f ~finally
        with Unix.Unix_error (Unix.EBADF, _, _) -> ()
      in
      let fd, peer = accept () in
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
            | Result.Ok m ->
                m
            | Result.Error e ->
                User_error.raise [ Pp.textf "%s" e ] )
        }
      in
      let tid = Thread.create client_thread client in
      manager.clients <-
        ( match Clients.add manager.clients client.fd (client, tid) with
        | Result.Ok v ->
            v
        | Result.Error _ ->
            User_error.raise [ Pp.textf "duplicate socket" ] )
    done
  in
  try Exn.protect ~f ~finally
  with Unix.Unix_error (errno, f, _) ->
    User_error.raise
      [ Pp.textf "unable to %s: %s\n" f (Unix.error_message errno) ]

let endpoint m = m.endpoint

let err msg = User_error.E (User_error.make [ Pp.text msg ])

module Client = struct
  type t =
    { socket : out_channel
    ; memory : Dune_memory.Memory.t
    }

  let make () =
    let open Result.O in
    let* memory = Result.map_error ~f:err (Dune_memory.make ()) in
    let* port = check_port_file (default_port_file ()) in
    let* addr, port =
      match port with
      | Some (port, _, _) -> (
        match String.split_on_char ~sep:':' port with
        | [ addr; port ] -> (
          match int_of_string_opt port with
          | Some i ->
              Result.Ok (Unix.inet_addr_of_string addr, i)
          | None ->
              Result.Error (err (Printf.sprintf "invalid port: %s" port)) )
        | _ ->
            Result.Error (err (Printf.sprintf "invalid endpoint: %s" port)) )
      | None ->
          Result.Error (err "dune-cache-manager is not started")
    in
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let* _ =
      Result.try_with (fun () ->
          Unix.connect socket (Unix.ADDR_INET (addr, port)))
    in
    let socket = Unix.out_channel_of_descr socket in
    send socket my_versions_command;
    Result.Ok { socket; memory }

  let promote client paths key metadata repo =
    let key = Dune_memory.key_to_string key
    and f (path, digest) =
      Sexp.List
        [ Sexp.Atom (Path.reach ~from:Path.build_dir path)
        ; Sexp.Atom (Digest.to_string digest)
        ]
    and repo =
      match repo with
      | Some idx ->
          [ Sexp.List [ Sexp.Atom "repo"; Sexp.Atom (string_of_int idx) ] ]
      | None ->
          []
    in
    send client.socket
      (Sexp.List
         ( Sexp.Atom "promote"
         :: Sexp.List [ Sexp.Atom "key"; Sexp.Atom key ]
         :: Sexp.List (Sexp.Atom "files" :: List.map ~f paths)
         :: Sexp.List [ Sexp.Atom "metadata"; Sexp.List metadata ]
         :: repo ));
    Result.Ok ()

  let set_build_dir client path =
    send client.socket
      (Sexp.List
         [ Sexp.Atom "set-build-root"
         ; Sexp.Atom (Path.to_absolute_filename path)
         ])

  let search client key = Dune_memory.Memory.search client.memory key
end
