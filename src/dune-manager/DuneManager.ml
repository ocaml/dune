open Stdune
open Dune_memory

type version = int * int

type client =
  {fd: Unix.file_descr; output: out_channel; mutable version: version option}

let send client sexp =
  output_string client.output (Csexp.to_string sexp) ;
  (* We need to flush when sending the version. Other
     instances are more debatable. *)
  flush client.output

module ClientsKey = struct
  type t = client

  let compare a b = Ordering.of_int (Pervasives.compare a.fd b.fd)

  let to_dyn _ = Dyn.Opaque
end

module Clients = Set.Make (ClientsKey) (Map.Make (ClientsKey))

type t =
  { memory: DuneMemory.memory
  ; mutable socket: Unix.file_descr option
  ; mutable clients: Clients.t
  ; mutable endpoint: string option }

exception Error of string

let make ?root () : t =
  { memory= DuneMemory.make ?root ()
  ; socket= None
  ; clients= Clients.empty
  ; endpoint= None }

let getsockname = function
  | Unix.ADDR_UNIX _ ->
      failwith "got a Unix socket connection on our TCP socket ?"
  | Unix.ADDR_INET (addr, port) ->
      (addr, port)

let peer_name s =
  let addr, port = getsockname s in
  Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

exception Stop

let stop manager =
  match manager.socket with
  | Some fd ->
      manager.socket <- None ;
      ignore
        (Clients.iter ~f:(fun client -> Unix.close client.fd) manager.clients) ;
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

let run ?(port_f = ignore) ?(port = 0) manager =
  let open Result.O in
  let memory = manager.memory in
  let handle_lang client = function
    | Sexp.Atom "dune-memory-protocol" :: versions -> (
        let decode_version = function
          | Sexp.List [Sexp.Atom major; Sexp.Atom minor] as v -> (
            try Result.ok (int_of_string major, int_of_string minor)
            with Failure _ ->
              Result.Error
                (Printf.sprintf "invalid intergers in lang command version: %s"
                   (Sexp.to_string v)) )
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
            Printf.printf "negotiated version: %i.%i\n%!" major minor ;
            client.version <- v ;
            Result.ok () )
    | cmd ->
        Result.Error
          (Printf.sprintf "invalid lang command: %s"
             (Sexp.to_string (Sexp.List cmd)))
  and handle_promote client = function
    | Sexp.List [Sexp.Atom "key"; Sexp.Atom key]
      :: Sexp.List (Sexp.Atom "files" :: files)
         :: Sexp.List [Sexp.Atom "metadata"; metadata] :: rest as cmd -> (
        let repo =
          match rest with
          | [] ->
              Result.ok None
          | [Sexp.List [Sexp.Atom "repo"; Sexp.Atom repo]] -> (
            match int_of_string_opt repo with
            | Some v ->
                Result.ok (Some v)
            | None ->
                Result.Error (Printf.sprintf "invalid repo: %s" repo) )
          | _ ->
              Result.Error
                (Printf.sprintf "invalid promotion message: %s"
                   (Sexp.to_string (Sexp.List cmd)))
        and file = function
          | Sexp.List [Sexp.Atom path; Sexp.Atom hash] ->
              Result.ok (Path.of_string path, Digest.from_hex hash)
          | sexp ->
              Result.Error
                (Printf.sprintf "invalid file in promotion message: %s"
                   (Sexp.to_string sexp))
        and f promotion =
          print_endline (DuneMemory.promotion_to_string promotion) ;
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
          DuneMemory.promote memory files
            (DuneMemory.key_of_string key)
            metadata repo
        in
        match List.filter_map ~f promotions with
        | [] ->
            ()
        | dedup ->
            send client (Sexp.List (Sexp.Atom "dedup" :: dedup)) )
    | args ->
        Result.Error
          (Printf.sprintf "invalid promotion message: %s"
             (Sexp.to_string (Sexp.List args)))
  in
  let handle_cmd client = function
    | Sexp.List (Sexp.Atom cmd :: args) -> (
        if cmd = "lang" then handle_lang client args
        else if Option.is_none client.version then (
          Unix.close client.fd ; Result.Error "version was not negotiated" )
        else
          match cmd with
          | "promote" ->
              handle_promote client args
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
      let fd, peer = accept () in
      let client = {fd; output= Unix.out_channel_of_descr fd; version= None} in
      let f () =
        try
          manager.clients <- Clients.add manager.clients client ;
          send client
            (Sexp.List
               ( Sexp.Atom "lang" :: Sexp.Atom "dune-memory-protocol"
               :: (List.map ~f:(function maj, min ->
                       Sexp.List
                         [ Sexp.Atom (string_of_int maj)
                         ; Sexp.Atom (string_of_int min) ] ))
                    my_versions )) ;
          Printf.printf "accept client: %s\n%!" (peer_name client.peer) ;
          let rec input =
            Stream.of_channel (Unix.in_channel_of_descr client.fd)
          and handle input =
            if Option.is_none manager.socket then ()
            else
              match Stream.peek input with
              | None ->
                  Printf.printf "client %s ended\n%!" (peer_name client.peer)
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
                    Printf.printf "received command: %s\n%!"
                      (Sexp.to_string cmd) ;
                    handle_cmd client cmd
                  with
                  | Result.Error e ->
                      Printf.fprintf stderr "command error: %s\n%!" e
                  | _ ->
                      ()) ;
                  (handle [@tailcall]) input
          in
          handle input
        and finally () =
          ( try Unix.close client.fd
            with Unix.Unix_error (Unix.EBADF, _, _) -> () ) ;
          manager.clients <- Clients.remove manager.clients client
        in
        try Exn.protect ~f ~finally
        with Unix.Unix_error (Unix.EBADF, _, _) -> ()
      in
      Exn.protect ~f ~finally
    done
  in
  try Exn.protect ~f ~finally
  with Unix.Unix_error (errno, f, _) ->
    raise
      (Error (Printf.sprintf "unable to %s: %s\n" f (Unix.error_message errno)))

let endpoint m = m.endpoint
