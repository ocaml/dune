open Stdune

let emit trace ~buffered =
  match
    Dune_trace.Out.emit ~buffered trace (Dune_trace.Event.spawn_thread ~name:"test")
  with
  | () -> print_endline "ok"
  | exception Unix.Unix_error (error, _, _) ->
    printfn "error: %s" (Unix.error_message error)
;;

let%expect_test "trace write failure" =
  let read, write = Unix.pipe () in
  Unix.close write;
  let fd = Fd.unsafe_of_unix_file_descr read in
  let trace = Dune_trace.Out.create (`Fd fd) in
  emit trace ~buffered:true;
  [%expect {| ok |}];
  emit trace ~buffered:false;
  [%expect {| error: Bad file descriptor |}];
  emit trace ~buffered:true;
  [%expect {| ok |}];
  emit trace ~buffered:false;
  [%expect {| error: Bad file descriptor |}];
  Fd.close fd
;;

let%expect_test "fd counting" =
  let module Fd_count = Dune_trace.Private.Fd_count in
  let get () =
    match Fd_count.get () with
    | This n -> n
    | Unknown -> failwith "no fd counting available"
  in
  let base = get () in
  let r, w = Unix.pipe () in
  let log () = printfn "fd count: %d" (get () - base) in
  log ();
  [%expect {| fd count: 2 |}];
  Unix.close r;
  log ();
  [%expect {| fd count: 1 |}];
  Unix.close w;
  log ();
  [%expect {| fd count: 0 |}]
;;
