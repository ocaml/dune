open Dune_rpc.V1
open Lwt.Syntax

module V1 = struct
  module Fiber = struct
    include Lwt

    let fork_and_join_unit (x : unit -> unit Lwt.t) y =
      let open Lwt in
      Lwt.both (x ()) (y ()) >|= snd
    ;;

    let finalize f ~finally = Lwt.finalize f finally

    let parallel_iter ls ~f =
      let stream = Lwt_stream.from ls in
      Lwt_stream.iter_p f stream
    ;;

    module Ivar = struct
      type 'a t = 'a Lwt.t * 'a Lwt.u

      let create () = Lwt.task ()

      let fill (_, u) x =
        Lwt.wakeup u x;
        Lwt.return_unit
      ;;

      let read (x, _) = x
    end

    module O = Syntax
  end

  module Client =
    Client.Make
      (Fiber)
      (struct
        type t = Lwt_io.input_channel * Lwt_io.output_channel

        let read (i, o) =
          (* The input and output channels share the same file descriptor. If
             the output channel has been closed, reading from the input channel
             will result in an error. *)
          let is_channel_closed () = Lwt_io.is_closed o in
          let open Csexp.Parser in
          let lexer = Lexer.create () in
          let rec loop depth stack =
            if is_channel_closed ()
            then (
              Lexer.feed_eoi lexer;
              Lwt.return_none)
            else
              let* res = Lwt_io.read_char_opt i in
              match res with
              | None ->
                Lexer.feed_eoi lexer;
                Lwt.return_none
              | Some c ->
                (match Lexer.feed lexer c with
                 | Await -> loop depth stack
                 | Lparen -> loop (depth + 1) (Stack.open_paren stack)
                 | Rparen ->
                   let stack = Stack.close_paren stack in
                   let depth = depth - 1 in
                   if depth = 0
                   then (
                     let sexps = Stack.to_list stack in
                     sexps |> List.hd |> Lwt.return_some)
                   else loop depth stack
                 | Atom count ->
                   if is_channel_closed ()
                   then (
                     Lexer.feed_eoi lexer;
                     Lwt.return_none)
                   else
                     let* atom =
                       let bytes = Bytes.create count in
                       let+ () = Lwt_io.read_into_exactly i bytes 0 count in
                       Bytes.to_string bytes
                     in
                     loop depth (Stack.add_atom atom stack))
          in
          loop 0 Stack.Empty
        ;;

        let write (_, o) = function
          | None -> Lwt_io.close o
          | Some csexps ->
            Lwt_list.iter_s (fun sexp -> Lwt_io.write o (Csexp.to_string sexp)) csexps
        ;;
      end)

  module Where =
    Where.Make
      (Fiber)
      (struct
        let read_file s : (string, exn) result Lwt.t =
          Lwt.catch
            (fun () -> Lwt_result.ok (Lwt_io.with_file ~mode:Input s Lwt_io.read))
            Lwt_result.fail
        ;;

        let analyze_path s =
          Lwt.try_bind
            (fun () -> Lwt_unix.stat s)
            (fun stat ->
               Lwt.return
                 (match stat.st_kind with
                  | Unix.S_SOCK -> Ok `Unix_socket
                  | S_REG -> Ok `Normal_file
                  | _ -> Ok `Other))
            (fun e -> Lwt.return (Error e))
        ;;
      end)

  let connect_chan where =
    let+ fd =
      let domain, sockaddr =
        match where with
        | `Unix socket -> Unix.PF_UNIX, Unix.ADDR_UNIX socket
        | `Ip (`Host host, `Port port) ->
          let addr = Unix.inet_addr_of_string host in
          Unix.PF_INET, Unix.ADDR_INET (addr, port)
      in
      let fd = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
      let+ () = Lwt_unix.connect fd sockaddr in
      fd
    in
    let fd mode = Lwt_io.of_fd fd ~mode in
    fd Input, fd Output
  ;;
end
