open Stdune
open Dune_memory

type version = int * int

type client =
  { fd: Unix.file_descr
  ; peer: Unix.sockaddr
  ; output: out_channel
  ; mutable build_root: Path.t option (* client owned *)
  ; mutable common_metadata: Sexp.t list (* client owned *)
  ; mutable memory: Dune_memory.memory (* client owned*)
  ; mutable repositories: (string * string * string) list (* client owned *)
  ; mutable version: version option (* client owned*) }

let make_path client path =
  if Filename.is_relative path then
    match client.build_root with
    | Some p ->
        Result.ok (Path.of_string (Filename.concat (Path.to_string p) path))
    | None ->
        Result.Error "relative path while no build root was set"
  else Result.ok (Path.of_string path)

let send client sexp =
  output_string client.output (Csexp.to_string sexp) ;
  (* We need to flush when sending the version. Other
     instances are more debatable. *)
  flush client.output

module ClientsKey = struct
  type t = Unix.file_descr

  let compare a b = Ordering.of_int (Pervasives.compare a b)

  let to_dyn _ = Dyn.Opaque
end

module Clients = Map.Make (ClientsKey)

type t =
  { root: Path.t option
  ; log: Log.t
  ; mutable socket: Unix.file_descr option
  ; mutable clients: (client * Thread.t) Clients.t
  ; mutable endpoint: string option }

exception Error of string

let make ?root ?log () : t =
  let log = match log with Some log -> log | None -> Log.create () in
  {root; log; socket= None; clients= Clients.empty; endpoint= None}

let getsockname = function
  | Unix.ADDR_UNIX _ ->
      User_error.raise
        [Pp.textf "got a Unix socket connection on our TCP socket ?"]
  | Unix.ADDR_INET (addr, port) ->
      (addr, port)

let peer_name s =
  let addr, port = getsockname s in
  Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

exception Stop

let stop manager =
  match manager.socket with
  | Some fd ->
      Log.infof manager.log "stop" ;
      manager.socket <- None ;
      let clean f = ignore (Clients.iter ~f manager.clients) in
      clean (fun (client, _) -> Unix.shutdown client.fd Unix.SHUTDOWN_ALL) ;
      clean (fun (_, tid) -> Thread.join tid) ;
      clean (fun (client, _) -> Unix.close client.fd) ;
      Unix.close fd
  | _ ->
      ()

let my_versions : version list = [(2, 0)]

module Int_map = Map.Make (Int)

let find_highest_common_version (a : version list) (b : version list) :
    version option =
  let a = Int_map.of_list_exn a and b = Int_map.of_list_exn b in
  let common =
    Int_map.merge
      ~f:(fun _ minor_in_a minor_in_b ->
        match (minor_in_a, minor_in_b) with
        | Some a, Some b ->
            Some (min a b)
        | _ ->
            None )
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
          | Sexp.List [Sexp.Atom major; Sexp.Atom minor] ->
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
            Unix.close client.fd ; Result.Error "no compatible versions"
        | Some (major, minor) as v ->
            Log.infof manager.log "%s: negotiated version: %i.%i"
              (peer_name client.peer) major minor ;
            client.version <- v ;
            Result.ok () )
    | args ->
        invalid_args args
  and handle_promote client = function
    | Sexp.List [Sexp.Atom "key"; Sexp.Atom key]
      :: Sexp.List (Sexp.Atom "files" :: files)
         :: Sexp.List [Sexp.Atom "metadata"; Sexp.List metadata] :: rest as cmd
      -> (
        let repo =
          match rest with
          | [] ->
              Result.ok None
          | [Sexp.List [Sexp.Atom "repo"; Sexp.Atom repo]] -> (
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
          | Sexp.List [Sexp.Atom path; Sexp.Atom hash] ->
              make_path client path >>| fun path -> (path, Digest.from_hex hash)
          | sexp ->
              Result.Error
                (Printf.sprintf "invalid file in promotion message: %s"
                   (Sexp.to_string sexp))
        and f promotion =
          print_endline (Dune_memory.promotion_to_string promotion) ;
          match promotion with
          | Already_promoted (f, t) ->
              Some
                (Sexp.List
                   [Sexp.Atom (Path.to_string f); Sexp.Atom (Path.to_string t)])
          | _ ->
              None
        in
        repo
        >>= fun repo ->
        Result.List.map ~f:file files
        >>| fun files ->
        let promotions =
          Dune_memory.promote client.memory files
            (Dune_memory.key_of_string key)
            (metadata @ client.common_metadata)
            (Option.map ~f:(fun (_, remote, commit) -> (remote, commit)) repo)
        in
        match List.filter_map ~f promotions with
        | [] ->
            ()
        | dedup ->
            send client (Sexp.List (Sexp.Atom "dedup" :: dedup)) )
    | args ->
        invalid_args args
  and handle_set_root client = function
    | [Sexp.Atom dir] ->
        Result.map_error
          ~f:(function
            | _ ->
                send client
                  (Sexp.List
                     [ Sexp.Atom "cannot-read-dune-memory"
                     ; Sexp.List [Sexp.Atom "supported-formats"; Sexp.Atom "v2"]
                     ]) ;
                "unable to read Dune memory" )
          ( Dune_memory.make ~root:(Path.of_string dir) ()
          >>| fun memory -> client.memory <- memory )
    | args ->
        invalid_args args
  and handle_set_build_root client = function
    | [Sexp.Atom dir] ->
        client.build_root <- Some (Path.of_string dir) ;
        Result.ok ()
    | args ->
        invalid_args args
  and handle_set_metadata client arg =
    client.common_metadata <- arg ;
    Result.ok ()
  and handle_set_repos client arg =
    let convert = function
      | Sexp.List
          [ Sexp.List [Sexp.Atom "dir"; Sexp.Atom dir]
          ; Sexp.List [Sexp.Atom "remote"; Sexp.Atom remote]
          ; Sexp.List [Sexp.Atom "commit_id"; Sexp.Atom commit] ] ->
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
          Unix.shutdown client.fd Unix.SHUTDOWN_ALL ;
          Result.Error "version was not negotiated" )
        else
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
          if Option.is_none manager.socket then raise Stop else accept ()
      | Unix.Unix_error (Unix.EBADF, _, _) as e ->
          if Option.is_none manager.socket then raise Stop else raise e
      | e ->
          raise e
    in
    manager.socket <- Some sock ;
    Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", port)) ;
    let addr, port = getsockname (Unix.getsockname sock) in
    let endpoint =
      Printf.sprintf "%s:%i" (Unix.string_of_inet_addr addr) port
    in
    manager.endpoint <- Some endpoint ;
    port_f endpoint ;
    Unix.listen sock 1024 ;
    while Option.is_some manager.socket do
      let client_thread client =
        let f () =
          send client
            (Sexp.List
               ( Sexp.Atom "lang" :: Sexp.Atom "dune-memory-protocol"
               :: (List.map ~f:(function maj, min ->
                       Sexp.List
                         [ Sexp.Atom (string_of_int maj)
                         ; Sexp.Atom (string_of_int min) ] ))
                    my_versions )) ;
          Log.infof manager.log "accept client: %s" (peer_name client.peer) ;
          let rec input =
            Stream.of_channel (Unix.in_channel_of_descr client.fd)
          and handle input =
            if Option.is_none manager.socket then ()
            else
              match Stream.peek input with
              | None ->
                  Log.infof manager.log "%s: ended" (peer_name client.peer)
              | Some '\n' ->
                  Stream.junk input ;
                  (handle [@tailcall]) input
                  (* Skip toplevel newlines, for easy netcat interaction *)
              | _ ->
                  (let open Result.O in
                  match
                    Result.map_error
                      ~f:(fun r -> "parse error: " ^ r)
                      (Csexp.parse input)
                    >>= fun cmd ->
                    Log.infof manager.log "%s: received command: %s"
                      (peer_name client.peer) (Sexp.to_string cmd) ;
                    handle_cmd client cmd
                  with
                  | Result.Error e ->
                      Log.infof manager.log "%s: command error: %s"
                        (peer_name client.peer) e
                  | _ ->
                      ()) ;
                  (handle [@tailcall]) input
          in
          handle input
        and finally () =
          ( try
              Unix.shutdown client.fd Unix.SHUTDOWN_ALL ;
              Unix.close client.fd
            with
          | Unix.Unix_error (Unix.EBADF, _, _) ->
              Log.infof manager.log "%s: ended" (peer_name client.peer)
          | Sys_error msg ->
              Log.infof manager.log "%s: ended: %s" (peer_name client.peer) msg
          ) ;
          manager.clients <- Clients.remove manager.clients client.fd
        in
        try Exn.protect ~f ~finally
        with Unix.Unix_error (Unix.EBADF, _, _) -> ()
      in
      let fd, peer = accept () in
      let client =
        { fd
        ; peer
        ; output= Unix.out_channel_of_descr fd
        ; version= None
        ; build_root= None
        ; common_metadata= []
        ; repositories= []
        ; memory= Result.ok_exn (Dune_memory.make ?root:manager.root ()) }
      in
      let tid = Thread.create client_thread client in
      manager.clients
      <- ( match Clients.add manager.clients client.fd (client, tid) with
         | Result.Ok v ->
             v
         | Result.Error _ ->
             User_error.raise [Pp.textf "duplicate socket"] )
    done
  in
  try Exn.protect ~f ~finally
  with Unix.Unix_error (errno, f, _) ->
    User_error.raise
      [Pp.textf "unable to %s: %s\n" f (Unix.error_message errno)]

let endpoint m = m.endpoint
