open Stdune

let%expect_test "fd counting" =
  let module Fd_count = Dune_stats.Private.Fd_count in
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
