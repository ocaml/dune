open Stdune
open Fiber.O
module Caml_lazy = Lazy
open Memo
open Dune_tests_common

let () = init ()

let string_fn_create name =
  Memo.create name
    ~input:(module String)
    ~visibility:(Public Dune_lang.Decoder.string) Async

let int_fn_create name =
  Memo.create name
    ~input:(module Int)
    ~visibility:(Public Dune_lang.Decoder.int) Async

(* to run a computation *)
let run f v =
  let exn = ref None in
  try
    Fiber.run
      (Fiber.with_error_handler
         (fun () -> Memo.exec f v)
         ~on_error:(fun e -> exn := Some e))
  with Fiber.Never -> (
    match !exn with
    | Some exn -> Exn_with_backtrace.reraise exn
    | None -> raise Fiber.Never )

(* the trivial dependencies are simply the identity function *)
let compdep x = Fiber.return (x ^ x)

(* our two dependencies are called some and another *)
let mcompdep1 =
  string_fn_create "some" ~output:(Allow_cutoff (module String)) compdep

let mcompdep2 =
  string_fn_create "another" ~output:(Allow_cutoff (module String)) compdep

(* compute the dependencies once so they are present in the global hash table *)
let () =
  ignore (run mcompdep1 "a");
  ignore (run mcompdep2 "a")

(* define a counter so we can track how often our computation has been run *)
let counter = ref 0

(* our computation increases the counter, adds the two dependencies, "some" and
   "another" and works by multiplying the input by two *)
let comp x =
  Fiber.return x >>= Memo.exec mcompdep1 >>= Memo.exec mcompdep2
  >>= fun a ->
  counter := !counter + 1;
  String.sub a ~pos:0 ~len:(String.length a |> min 3) |> Fiber.return

let mcomp = string_fn_create "test" ~output:(Allow_cutoff (module String)) comp

(* running it the first time should increase the counter, running it again
   should not, but should still return the same result *)
let%expect_test _ =
  Format.printf "%d@." !counter;
  print_endline (run mcomp "a");
  Format.printf "%d@." !counter;
  print_endline (run mcomp "a");
  Format.printf "%d@." !counter;
  [%expect {|
0
aaa
1
aaa
1
|}]

let%expect_test _ =
  let open Dyn.Encoder in
  Memo.get_deps mcomp "a"
  |> option (list (pair string (fun x -> x)))
  |> print_dyn;
  [%expect {|
Some [ ("another", "aa"); ("some", "a") ]
|}]

let%expect_test _ =
  (* running it on a new input should cause it to recompute the first time it is
     run *)
  print_endline (run mcomp "hello");
  Format.printf "%d@." !counter;
  print_endline (run mcomp "hello");
  Format.printf "%d@." !counter;
  [%expect {|
hel
2
hel
2
|}]

let%expect_test _ =
  (* updating the first dependency should require recomputation of mcomp 7 *)
  print_endline (run mcompdep1 "testtest");
  print_endline (run mcomp "hello");
  Format.printf "%d@." !counter;
  print_endline (run mcomp "hello");
  Format.printf "%d@." !counter;
  [%expect {|
testtesttesttest
hel
2
hel
2
|}]

let stack = ref []

let dump_stack v =
  let s = get_call_stack () in
  stack := s;
  Fiber.return v

let mcompcycle =
  let mcompcycle = Fdecl.create Dyn.Encoder.opaque in
  let compcycle x =
    Fiber.return x >>= dump_stack
    >>= fun x ->
    counter := !counter + 1;
    if !counter < 20 then
      (x + 1) mod 3 |> Memo.exec (Fdecl.get mcompcycle)
    else
      failwith "cycle"
  in
  let fn =
    int_fn_create "cycle" ~output:(Allow_cutoff (module String)) compcycle
  in
  Fdecl.set mcompcycle fn;
  fn

let%expect_test _ =
  counter := 0;
  try run mcompcycle 5 |> ignore
  with Cycle_error.E err ->
    let cycle =
      Cycle_error.get err
      |> List.filter_map ~f:(Memo.Stack_frame.as_instance_of ~of_:mcompcycle)
    in
    print (Pp.enumerate cycle ~f:(Pp.textf "%d"));
    print (Pp.textf "%d" !counter);
    !stack
    |> List.map ~f:(fun st ->
           let open Dyn.Encoder in
           pair string (fun x -> x) (Stack_frame.name st, Stack_frame.input st))
    |> Dyn.Encoder.list (fun x -> x)
    |> print_dyn;
    [%expect
      {|
- 2
- 1
- 0
- 2
4
[ ("cycle", 2); ("cycle", 1); ("cycle", 0); ("cycle", 5) ]
|}]

let mfib =
  let mfib = Fdecl.create Dyn.Encoder.opaque in
  let compfib x =
    let mfib = Memo.exec (Fdecl.get mfib) in
    counter := !counter + 1;
    if x <= 1 then
      Fiber.return x
    else
      mfib (x - 1) >>= fun r1 -> mfib (x - 2) >>| fun r2 -> r1 + r2
  in
  let fn = int_fn_create "fib" ~output:(Allow_cutoff (module Int)) compfib in
  Fdecl.set mfib fn;
  fn

let%expect_test _ =
  counter := 0;
  Format.printf "%d@." (run mfib 2000);
  Format.printf "%d@." !counter;
  Format.printf "%d@." (run mfib 1800);
  Format.printf "%d@." !counter;
  [%expect {|
2406280077793834213
2001
3080005411477819488
2001
|}]

let sync_int_fn_create name =
  Memo.create name
    ~input:(module Int)
    ~visibility:(Public Dune_lang.Decoder.int) Sync

let counter = ref 0

let sync_fib =
  let mfib = Fdecl.create Dyn.Encoder.opaque in
  let compfib x =
    let mfib = Memo.exec (Fdecl.get mfib) in
    counter := !counter + 1;
    if x <= 1 then
      x
    else
      mfib (x - 1) + mfib (x - 2)
  in
  let fn =
    sync_int_fn_create "sync-fib" ~output:(Allow_cutoff (module Int)) compfib
  in
  Fdecl.set mfib fn;
  fn

let%expect_test _ =
  Format.printf "%d@." (Memo.exec sync_fib 2000);
  Format.printf "%d@." !counter;
  Format.printf "%d@." (Memo.exec sync_fib 1800);
  Format.printf "%d@." !counter;
  [%expect {|
  2406280077793834213
  2001
  3080005411477819488
  2001
  |}]

let make_f name f ~input ~output =
  Memo.create name ~input ~visibility:Hidden ~output:(Allow_cutoff output)
    ~doc:"" Sync f

let id =
  let f =
    make_f "id" (fun s -> s) ~input:(module String) ~output:(module String)
  in
  Memo.exec f

module Test_lazy (Lazy : sig
  type 'a t

  val create : (unit -> 'a) -> 'a t

  val force : 'a t -> 'a
end) =
struct
  module Lazy_string = struct
    type t = string Lazy.t

    let to_dyn s =
      ignore (Lazy.force s);
      Dyn.Encoder.string "opaque"

    let equal x y = String.equal (Lazy.force x) (Lazy.force y)

    let hash s = String.hash (Lazy.force s)
  end

  let lazy_memo =
    let f =
      make_f "lazy_memo"
        (fun s -> Lazy.create (fun () -> id ("lazy: " ^ s)))
        ~input:(module String)
        ~output:(module Lazy_string)
    in
    Memo.exec f

  let f1_def, f1 =
    let f =
      make_f "f1"
        (fun s -> "f1: " ^ Lazy.force (lazy_memo s))
        ~input:(module String)
        ~output:(module String)
    in
    (f, Memo.exec f)

  let f2_def, f2 =
    let f =
      make_f "f2"
        (fun s -> "f2: " ^ Lazy.force (lazy_memo s))
        ~input:(module String)
        ~output:(module String)
    in
    (f, Memo.exec f)

  let run () = (f1 "foo", f2 "foo")

  let deps () =
    let open Dyn.Encoder in
    let conv = option (list (pair string (fun x -> x))) in
    pair conv conv (get_deps f1_def "foo", get_deps f2_def "foo")
end

module Builtin_lazy = Test_lazy (struct
  include Caml_lazy

  let create = from_fun
end)

let%expect_test _ =
  Builtin_lazy.run () |> Dyn.Encoder.(pair string string) |> print_dyn;
  [%expect {|
("f1: lazy: foo", "f2: lazy: foo")
|}]

let%expect_test _ =
  (* Bug: dependency on [lazy] is only registered by one of the dependants. This
     means we should never use [lazy] together with [Memo]. We can use
     [Memo.lazy_] though (see below) *)
  Builtin_lazy.deps () |> print_dyn;
  [%expect
    {|
(Some [ ("lazy_memo", "foo") ],
Some [ ("id", "lazy: foo"); ("lazy_memo", "foo") ])
|}]

module Memo_lazy = Test_lazy (Memo.Lazy)

let%expect_test _ =
  Memo_lazy.run () |> Dyn.Encoder.(pair string string) |> print_dyn;
  [%expect {|
("f1: lazy: foo", "f2: lazy: foo")
|}]

let%expect_test _ =
  Memo_lazy.deps () |> print_dyn;
  [%expect
    {|
(Some [ ("lazy-0", ()); ("lazy_memo", "foo") ],
Some [ ("lazy-0", ()); ("lazy_memo", "foo") ])
|}]

(* Tests for depending on the current run*)

let depends_on_run =
  Memo.create "foobar" ~doc:"foo123"
    ~input:(module Unit)
    ~output:(Allow_cutoff (module Unit))
    ~visibility:Hidden Sync
    (fun () ->
      let (_ : Memo.Run.t) = Memo.current_run () in
      print_endline "running foobar")

let%expect_test _ =
  Memo.exec depends_on_run ();
  Memo.exec depends_on_run ();
  print_endline "resetting memo";
  Memo.reset ();
  Memo.exec depends_on_run ();
  [%expect {|
    running foobar
    resetting memo
    running foobar |}]

(* Tests for Memo.Cell *)

let%expect_test _ =
  let f x = "*" ^ x in
  let memo =
    Memo.create "for-cell"
      ~input:(module String)
      ~visibility:(Public Dune_lang.Decoder.string)
      ~output:(Allow_cutoff (module String))
      ~doc:"" Sync f
  in
  let cell = Memo.cell memo "foobar" in
  print_endline (Cell.get_sync cell);
  print_endline (Cell.get_sync cell);
  [%expect {|
    *foobar
    *foobar |}]

let printf = Printf.printf

let%expect_test "fib linked list" =
  let module Element = struct
    type t =
      { prev_cell : (int, t, int -> t) Memo.Cell.t
      ; value : int
      ; next_cell : (int, t, int -> t) Memo.Cell.t
      }

    let to_dyn t = Dyn.Int t.value
  end in
  let force cell : Element.t = Memo.Cell.get_sync cell in
  let memo_fdecl = Fdecl.create Dyn.Encoder.opaque in
  let compute_element x =
    let memo = Fdecl.get memo_fdecl in
    printf "computing %d\n" x;
    let prev_cell = Memo.cell memo (x - 1) in
    let value =
      if x < 1 then
        0
      else if x = 1 then
        1
      else
        (force prev_cell).value + (force (force prev_cell).prev_cell).value
    in
    { Element.next_cell = Memo.cell memo (x + 1); prev_cell; value }
  in
  let memo =
    Memo.create "fib"
      ~input:(module Int)
      ~visibility:Hidden Sync
      ~output:(Simple (module Element))
      compute_element ~doc:""
  in
  Fdecl.set memo_fdecl memo;
  let fourth = Memo.exec memo 4 in
  printf "4th: %d\n" fourth.value;
  printf "next: %d\n" (force fourth.next_cell).value;
  let seventh = Memo.exec memo 7 in
  printf "7th: %d\n" seventh.value;
  printf "prev: %d\n" (force seventh.prev_cell).value;
  printf "prev: %d\n" (force (force seventh.prev_cell).prev_cell).value;
  [%expect
    {|
    computing 4
    computing 3
    computing 2
    computing 1
    computing 0
    4th: 3
    computing 5
    next: 5
    computing 7
    computing 6
    7th: 13
    prev: 8
    prev: 5 |}]
