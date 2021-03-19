(* end to end tests of dune_rpc. Verify that whatever is exposed to the client
   is usable *)

open Stdune
open Dune_rpc.V1
open Lwt.Syntax

module Client = Client (struct
  module Fiber = struct
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
  end

  module Chan = struct
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
  end
end)

let%expect_test "run and connect" =
  let open Lwt.Syntax in
  let initialize = Initialize.create ~id:(Id.make (Csexp.Atom "test")) in
  Lwt_main.run
    (let* root_dir = Lwt_io.create_temp_dir () in
     let build =
       Lwt_process.open_process_none
         ( "dune"
         , [| "dune"
            ; "build"
            ; "--no-print-directory"
            ; "--root"
            ; root_dir
            ; "-w"
            ; "@install"
           |] )
     in
     let rpc =
       let+ () = Lwt_unix.sleep 0.5 in
       Lwt_process.open_process_full
         ("dune", [| "dune"; "rpc"; "init"; "--root"; root_dir |])
     in
     let client =
       let* rpc = rpc in
       let chan = (rpc#stdout, rpc#stdin) in
       Client.connect chan initialize ~f:(fun t ->
           print_endline "started session";
           let* res = Client.request t Request.ping () in
           match res with
           | Error _ -> failwith "unexpected"
           | Ok () ->
             print_endline "received ping. shutting down.";
             Client.notification t Notification.shutdown ())
     in
     let rpc =
       let* rpc = rpc in
       let+ res = rpc#status in
       match res with
       | WEXITED i -> printfn "rpc init finished with %i" i
       | _ -> assert false
     in
     let build =
       let+ res = build#status in
       match res with
       | WEXITED i -> printfn "dune build finished with %i" i
       | _ -> assert false
     in
     Lwt.catch
       (fun () ->
         let+ () =
           Lwt_unix.with_timeout 3.0 (fun () ->
               let+ _ = Lwt.all [ client; rpc; build ] in
               ())
         in
         print_endline "success")
       (fun exn ->
         (match exn with
         | Lwt_unix.Timeout -> print_endline "timeout"
         | _ -> ());
         Lwt.return_unit));
  [%expect
    {|
    started session
    received ping. shutting down.
    rpc init finished with 0
    dune build finished with 0
    success |}]
