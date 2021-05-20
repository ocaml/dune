open Stdune
open Dune_rpc.V1
open Lwt.Syntax

module V1 = struct
  module Client =
    Client
      (struct
        include Lwt

        let fork_and_join_unit (x : unit -> unit Lwt.t) y =
          let open Lwt in
          Lwt.both (x ()) (y ()) >|= snd

        let rec parallel_iter ls ~f =
          let open Lwt.Syntax in
          let* res = ls () in
          match res with
          | None -> Lwt.return_unit
          | Some x ->
            let+ () = f x
            and+ () = parallel_iter ls ~f in
            ()

        module Ivar = struct
          type 'a t = 'a Lwt.t * 'a Lwt.u

          let create () = Lwt.task ()

          let fill (_, u) x =
            Lwt.wakeup u x;
            Lwt.return_unit

          let read (x, _) = x
        end

        module O = Syntax
      end)
      (struct
        type t = Lwt_io.input_channel * Lwt_io.output_channel

        let read (i, _) =
          let open Csexp.Parser in
          let lexer = Lexer.create () in
          let rec loop depth stack =
            let* res = Lwt_io.read_char_opt i in
            match res with
            | None ->
              Lexer.feed_eoi lexer;
              Lwt.return_none
            | Some c -> (
              match Lexer.feed lexer c with
              | Await -> loop depth stack
              | Lparen -> loop (depth + 1) (Stack.open_paren stack)
              | Rparen ->
                let stack = Stack.close_paren stack in
                let depth = depth - 1 in
                if depth = 0 then
                  let sexps = Stack.to_list stack in
                  sexps |> List.hd |> Lwt.return_some
                else
                  loop depth stack
              | Atom count ->
                let* atom =
                  let bytes = Bytes.create count in
                  let+ () = Lwt_io.read_into_exactly i bytes 0 count in
                  Bytes.to_string bytes
                in
                loop depth (Stack.add_atom atom stack))
          in
          loop 0 Stack.Empty

        let write (_, o) = function
          | None -> Lwt_io.close o
          | Some csexp -> Lwt_io.write o (Csexp.to_string csexp)
      end)
end
