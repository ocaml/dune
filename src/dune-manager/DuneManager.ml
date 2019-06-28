open Stdune
open Dune_memory

type version = int * int

type client =
  {fd: Unix.file_descr; output: out_channel; mutable version: version option}

let send client sexp =
  output_string client.output (Csexp.to_string_canonical sexp) ;
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

exception CommandError of string

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

module Parser = Csexp.Parser (Csexp.FDStream)

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

let find_higest_common_version (a : version list) (b : version list) :
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
  let memory = manager.memory in
  let handle_lang client = function
    | Sexp.Atom "dune-memory-protocol" :: versions -> (
        let decode_version = function
          | Sexp.List [Sexp.Atom major; Sexp.Atom minor] as v -> (
            try (int_of_string major, int_of_string minor)
            with Failure _ ->
              raise
                (CommandError
                   (Printf.sprintf
                      "invalid intergers in lang command version: %s"
                      (Sexp.to_string v))) )
          | v ->
              raise
                (CommandError
                   (Printf.sprintf "invalid version in lang command: %s"
                      (Sexp.to_string v)))
        in
        match
          find_higest_common_version my_versions
            (List.map ~f:decode_version versions)
        with
        | None ->
            Unix.close client.fd ;
            raise (CommandError "no compatible versions")
        | Some (major, minor) as v ->
            Printf.printf "negotiated version: %i.%i\n%!" major minor ;
            client.version <- v )
    | cmd ->
        raise
          (CommandError
             (Printf.sprintf "invalid lang command: %s"
                (Sexp.to_string (Sexp.List cmd))))
  and handle_promote client = function
    | Sexp.List [Sexp.Atom "key"; Sexp.Atom key]
      :: Sexp.List (Sexp.Atom "files" :: files)
         :: Sexp.List [Sexp.Atom "metadata"; metadata] :: rest as cmd ->
        let repo =
          match rest with
          | [] ->
              None
          | [Sexp.List [Sexp.Atom "repo"; Sexp.Atom repo]] -> (
            match int_of_string_opt repo with
            | Some v ->
                Some v
            | None ->
                raise (CommandError (Printf.sprintf "invalid repo: %s" repo)) )
          | _ ->
              raise
                (CommandError
                   (Printf.sprintf "invalid promotion message: %s"
                      (Sexp.to_string (Sexp.List cmd))))
        and file = function
          | Sexp.List [Sexp.Atom path; Sexp.Atom hash] ->
              (Path.of_string path, Digest.from_hex hash)
          | sexp ->
              raise
                (CommandError
                   (Printf.sprintf "invalid file in promotion message: %s"
                      (Sexp.to_string sexp)))
        in
        let promotions =
          DuneMemory.promote memory (List.map ~f:file files)
            (DuneMemory.key_of_string key)
            metadata repo
        in
        let f promotion =
          print_endline (DuneMemory.promotion_to_string promotion) ;
          match promotion with
          | Already_promoted (f, t) ->
              Some
                (Sexp.List
                   [Sexp.Atom (Path.to_string f); Sexp.Atom (Path.to_string t)])
          | _ ->
              None
        in
        let dedup = List.filter_map ~f promotions in
        if dedup != [] then
          send client (Sexp.List (Sexp.Atom "dedup" :: dedup))
    | args ->
        raise
          (CommandError
             (Printf.sprintf "invalid promotion message: %s"
                (Sexp.to_string (Sexp.List args))))
  in
  let handle_cmd client = function
    | Sexp.List (Sexp.Atom cmd :: args) -> (
        if cmd = "lang" then handle_lang client args
        else if Option.is_none client.version then (
          Unix.close client.fd ;
          raise (CommandError "version was not negotiated") )
        else
          match cmd with
          | "promote" ->
              handle_promote client args
          | _ ->
              raise (CommandError (Printf.sprintf "unknown command: %s" cmd)) )
    | cmd ->
        raise
          (CommandError
             (Printf.sprintf "invalid command format: %s" (Sexp.to_string cmd)))
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
          Printf.printf "accept client: %s\n%!" (peer_name peer) ;
          let input = Csexp.FDStream.make fd in
          while Option.is_some manager.socket do
            (* Skip toplevel newlines, for easy netcat interaction *)
            while Csexp.FDStream.peek_byte input = int_of_char '\n' do
              ignore (Csexp.FDStream.input_byte input)
            done ;
            let cmd = Parser.parse_stream input in
            Printf.printf "received command: %s\n%!" (Sexp.to_string cmd) ;
            try handle_cmd client cmd
            with CommandError s ->
              Printf.fprintf stderr "Command error: %s\n%!" s
          done
        with
        | End_of_file | Unix.Unix_error (Unix.EBADF, _, _) ->
            Printf.printf "client ended\n%!"
        | Csexp.Parse_error msg ->
            Printf.fprintf stderr "Canonical SExp parse error: %s\n%!" msg
      and finally () =
        (try Unix.close fd with Unix.Unix_error (Unix.EBADF, _, _) -> ()) ;
        manager.clients <- Clients.remove manager.clients client
      in
      Exn.protect ~f ~finally
    done
  in
  try Exn.protect ~f ~finally
  with Unix.Unix_error (errno, f, _) ->
    raise
      (Error (Printf.sprintf "unable to %s: %s\n" f (Unix.error_message errno)))

let endpoint m = m.endpoint
