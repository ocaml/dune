open Stdune

let%expect_test "writer failure without a scheduler" =
  printfn "handler registered: %b" (Option.is_some (Fdecl.get_opt Dune_trace.on_error));
  [%expect {| handler registered: false |}];
  let read, write = Unix.pipe () in
  Unix.close write;
  let fd = Fd.unsafe_of_unix_file_descr read in
  Exn.protect
    ~finally:(fun () -> Fd.close fd)
    ~f:(fun () ->
      let trace = Dune_trace.Out.create (`Fd fd) in
      Dune_trace.Out.emit trace (Dune_trace.Event.spawn_thread ~name:"test");
      printfn "closed: %b" (Fd.is_closed fd);
      [%expect {| closed: true |}])
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
