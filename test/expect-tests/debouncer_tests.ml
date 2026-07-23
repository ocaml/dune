open Stdune
module Debouncer = Dune_engine.For_tests.Debouncer

let print_latest t =
  print_endline
    (match Debouncer.latest t with
     | None -> "None"
     | Some _ -> "Some")
;;

let%expect_test "empty debouncer has no latest token" =
  let t = Debouncer.create () in
  print_latest t;
  [%expect {| None |}]
;;

let%expect_test "latest token can consume pending work" =
  let t = Debouncer.create () in
  let (_ : Debouncer.token) = Debouncer.push t in
  (match Debouncer.latest t with
   | None -> print_endline "None"
   | Some token ->
     printfn "%b" (Debouncer.take_if_latest t token);
     printfn "%b" (Debouncer.is_pending t));
  [%expect
    {|
    true
    false
    |}]
;;

let%expect_test "only the latest token consumes pending work" =
  let t = Debouncer.create () in
  let first = Debouncer.push t in
  let second = Debouncer.push t in
  printfn "%b" (Debouncer.take_if_latest t first);
  [%expect {| false |}];
  print_latest t;
  [%expect {| Some |}];
  printfn "%b" (Debouncer.take_if_latest t second);
  [%expect {| true |}];
  print_latest t;
  [%expect {| None |}];
  printfn "%b" (Debouncer.take_if_latest t second);
  [%expect {| false |}]
;;

let%expect_test "can be reused after pending work is consumed" =
  let t = Debouncer.create () in
  let first = Debouncer.push t in
  printfn "%b" (Debouncer.take_if_latest t first);
  [%expect {| true |}];
  printfn "%b" (Debouncer.is_pending t);
  [%expect {| false |}];
  let second = Debouncer.push t in
  printfn "%b" (Debouncer.is_pending t);
  [%expect {| true |}];
  printfn "%b" (Debouncer.take_if_latest t first);
  [%expect {| false |}];
  printfn "%b" (Debouncer.take_if_latest t second);
  [%expect {| true |}];
  printfn "%b" (Debouncer.is_pending t);
  [%expect {| false |}]
;;

let%expect_test "multiple stale tokens do not consume pending work" =
  let t = Debouncer.create () in
  let first = Debouncer.push t in
  let second = Debouncer.push t in
  let third = Debouncer.push t in
  printfn "%b" (Debouncer.take_if_latest t first);
  [%expect {| false |}];
  printfn "%b" (Debouncer.take_if_latest t second);
  [%expect {| false |}];
  printfn "%b" (Debouncer.is_pending t);
  [%expect {| true |}];
  printfn "%b" (Debouncer.take_if_latest t third);
  [%expect {| true |}];
  printfn "%b" (Debouncer.is_pending t);
  [%expect {| false |}]
;;

let%expect_test "pending state" =
  let t = Debouncer.create () in
  printfn "%b" (Debouncer.is_pending t);
  let token = Debouncer.push t in
  printfn "%b" (Debouncer.is_pending t);
  [%expect
    {|
    false
    true
    |}];
  printfn "%b" (Debouncer.take_if_latest t token);
  [%expect {| true |}];
  printfn "%b" (Debouncer.is_pending t);
  [%expect {| false |}]
;;
