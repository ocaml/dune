open Dune_rpc.V1
open Lwt.Syntax

module V1 = struct
  module Fiber = struct
    include Lwt

    let collect_errors f =
      Lwt.catch
        (fun () -> f () |> Lwt.map (fun x -> Ok x))
        (fun exn -> Lwt.return_error [ exn ])
    ;;

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

        let read (i, _) =
          let open Csexp.Parser in
          let lexer = Lexer.create () in
          let eoi () =
            Lexer.feed_eoi lexer;
            Lwt.return_none
          in
          let rec loop depth stack =
            let open Lwt.Infix in
            Lwt_io.read_char_opt i
            >>= function
            | None -> eoi ()
            | Some c ->
              (match Lexer.feed lexer c with
               | Await -> loop depth stack
               | Lparen -> loop (depth + 1) (Stack.open_paren stack)
               | Rparen ->
                 let stack = Stack.close_paren stack in
                 let depth = depth - 1 in
                 if depth = 0
                 then Stack.to_list stack |> List.hd |> Lwt.return_some
                 else loop depth stack
               | Atom count ->
                 let* atom =
                   let bytes = Bytes.create count in
                   let+ () = Lwt_io.read_into_exactly i bytes 0 count in
                   Bytes.to_string bytes
                 in
                 loop depth (Stack.add_atom atom stack))
          in
          Lwt.catch
            (fun () -> loop 0 Stack.Empty)
            (function
              | Lwt_io.Channel_closed _ -> eoi ()
              | exn -> Lwt.fail exn)
        ;;

        let close (i, o) =
          Lwt.finalize (fun () -> Lwt_io.close o) (fun () -> Lwt_io.close i)
        ;;

        let write (_, o) csexps =
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
    let output =
      Lwt_io.of_fd fd ~mode:Output ~close:(fun () ->
        Lwt_unix.shutdown fd Unix.SHUTDOWN_SEND;
        Lwt.return_unit)
    in
    let input =
      Lwt_io.of_fd fd ~mode:Input ~close:(fun () ->
        Lwt.finalize (fun () -> Lwt_io.close output) (fun () -> Lwt_unix.close fd))
    in
    input, output
  ;;

  module Action_plugin = struct
    module Glob = Dune_rpc.V1.Action_plugin.Glob
    module Core = Dune_rpc.V1.Action_plugin

    module Chan = struct
      type t = Lwt_io.input_channel * Lwt_io.output_channel
    end

    module Plugin = Dune_rpc.V1.Action_plugin.Make (Fiber) (Chan) (Client)

    type t = Plugin.t

    module Error = Core.Error

    let outside_of_dune = Plugin.outside_of_dune
    let read_file = Plugin.read_file
    let read_directory_with_glob = Plugin.read_directory_with_glob

    let connection_error exn =
      match exn with
      | Unix.Unix_error (error, syscall, arg) ->
        let message =
          Stdune.Unix_error.Detailed.create error ~syscall ~arg
          |> Stdune.Unix_error.Detailed.to_string_hum
        in
        Error.E ("unable to connect to dune rpc server: " ^ message)
      | exn -> exn
    ;;

    let connect where =
      Lwt.catch
        (fun () -> connect_chan where)
        (fun exn -> Lwt.fail (connection_error exn))
    ;;

    let report_error message =
      prerr_endline message;
      exit 1
    ;;

    let run f =
      try
        let computation =
          match Core.run_context () with
          | Outside_of_dune -> f outside_of_dune
          | Under_dune { action_id; where } ->
            let* chan = connect where in
            Plugin.run chan ~action_id ~f
        in
        Lwt_main.run computation;
        exit 0
      with
      | Error.E message -> report_error message
    ;;
  end
end
