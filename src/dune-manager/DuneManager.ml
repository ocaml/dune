open Stdune
open Dune_memory

module FDSet = Set.Make (struct
  type t = Unix.file_descr

  let compare a b = Ordering.of_int (Pervasives.compare a b)
end)

type t =
  { root: Path.t
  ; mutable socket: Unix.file_descr option
  ; mutable clients: FDSet.t }

exception CommandError of string

exception Error of string

let make root : t = {root; socket= None; clients= FDSet.empty}

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
      ignore (FDSet.iter ~f:(fun fd -> Unix.close fd) manager.clients) ;
      Unix.close fd
  | _ ->
      ()

let run ?(port_f = ignore) ?(port = 0) manager =
  let memory = DuneMemory.make manager.root in
  let handle_promote = function
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
        ignore
          (List.map
             ~f:(fun p -> print_endline (DuneMemory.promotion_to_string p))
             promotions)
    | args ->
        raise
          (CommandError
             (Printf.sprintf "invalid promotion message: %s"
                (Sexp.to_string (Sexp.List args))))
  in
  let handle_cmd = function
    | Sexp.List (Sexp.Atom cmd :: args) -> (
      match cmd with
      | "promote" ->
          handle_promote args
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
    let _, port = getsockname (Unix.getsockname sock) in
    port_f port ;
    Printf.printf "listening on port %i\n%!" port ;
    Unix.listen sock 1024 ;
    while Option.is_some manager.socket do
      let fd, peer = accept () in
      let f () =
        try
          manager.clients <- FDSet.add manager.clients fd ;
          Printf.printf "accept client: %s\n%!" (peer_name peer) ;
          let input = Csexp.FDStream.make fd in
          while Option.is_some manager.socket do
            (* Skip toplevel newlines, for easy netcat interaction *)
            while Csexp.FDStream.peek_byte input = int_of_char '\n' do
              ignore (Csexp.FDStream.input_byte input)
            done ;
            let cmd = Parser.parse_stream input in
            try handle_cmd cmd
            with CommandError s ->
              Printf.fprintf stderr "Command error: %s\n%!" s
          done
        with
        | End_of_file | Unix.Unix_error (Unix.EBADF, _, _) ->
            ()
        | Csexp.Parse_error msg ->
            Printf.fprintf stderr "Canonical SExp parse error: %s\n%!" msg
      and finally () =
        (try Unix.close fd with Unix.Unix_error (Unix.EBADF, _, _) -> ()) ;
        manager.clients <- FDSet.remove manager.clients fd
      in
      Exn.protect ~f ~finally
    done
  in
  try Exn.protect ~f ~finally
  with Unix.Unix_error (errno, f, _) ->
    raise
      (Error (Printf.sprintf "unable to %s: %s\n" f (Unix.error_message errno)))
