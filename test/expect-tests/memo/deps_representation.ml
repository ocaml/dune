open! Stdune
open! Memo.O
open Test_helpers.Make ()

(* These tests assert the *shape* of a memoized computation's recorded dependencies - the
   series-parallel Seq/Par/Singleton/Empty structure - by driving each shape through the
   public Memo combinators and reading it back with [Memo.For_tests.get_deps_structured]. *)

let read = Memo.Node.read

let print_deps label m =
  let cell = Memo.lazy_node ~name:"top" (fun () -> m) in
  let () = run (read cell) in
  match Memo.For_tests.get_deps_structured cell with
  | None -> printfn "%s: <none>" label
  | Some dyn -> printfn "%s: %s" label (Dyn.to_string dyn)
;;

let%expect_test "map_reduce_seq consumes a one-shot sequence once" =
  let nodes =
    List.map
      [ "a", 1; "b", 2; "c", 3 ]
      ~f:(fun (name, value) -> Memo.lazy_node ~name (fun () -> Memo.return value))
  in
  let remaining = ref nodes in
  let pulls = ref 0 in
  let rec next () =
    incr pulls;
    match !remaining with
    | [] -> Seq.Nil
    | node :: rest ->
      remaining := rest;
      Seq.Cons (node, next)
  in
  print_deps
    "one-shot"
    (let+ sum = Memo.map_reduce_seq next ~f:read ~empty:0 ~combine:( + ) in
     printfn "sum: %d" sum);
  printfn "pulls: %d" !pulls;
  [%expect
    {|
    sum: 6
    one-shot: Seq
      [ Singleton (Some "a", ())
      ; Singleton (Some "b", ())
      ; Singleton (Some "c", ())
      ]
    pulls: 4
    |}]
;;

let%expect_test "map_reduce_seq preserves branch order and sequential sections" =
  let node name = Memo.lazy_node ~name (fun () -> Memo.return ()) in
  let before, after = node "before", node "after" in
  let a, b, c, d = node "a", node "b", node "c", node "d" in
  print_deps
    "empty"
    (Memo.map_reduce_seq Seq.empty ~f:read ~empty:() ~combine:(fun () () -> ()));
  print_deps
    "singleton"
    (Memo.map_reduce_seq (Seq.return a) ~f:read ~empty:() ~combine:(fun () () -> ()));
  print_deps
    "sections"
    (let* () = read before in
     let* () =
       Memo.map_reduce_seq
         (List.to_seq [ a, b; c, d ])
         ~empty:()
         ~combine:(fun () () -> ())
         ~f:(fun (first, second) ->
           let* () = read first in
           let* () =
             if first == a
             then Memo.of_reproducible_fiber (Scheduler.yield ())
             else Memo.return ()
           in
           read second)
     in
     read after);
  [%expect
    {|
    empty: Empty
    singleton: Singleton (Some "a", ())
    sections: Seq
      [ Singleton (Some "before", ())
      ; Singleton (Some "a", ())
      ; Singleton (Some "c", ())
      ; Singleton (Some "d", ())
      ; Singleton (Some "b", ())
      ; Singleton (Some "after", ())
      ]
    |}]
;;

let%expect_test "dependency structure of Memo combinators" =
  let a = Memo.lazy_node ~name:"a" (fun () -> Memo.return ())
  and b = Memo.lazy_node ~name:"b" (fun () -> Memo.return ())
  and c = Memo.lazy_node ~name:"c" (fun () -> Memo.return ()) in
  (* No dependencies. *)
  print_deps "empty" (Memo.return ());
  [%expect {| empty: Empty |}];
  (* A single dependency is a [Singleton], whether discovered by map or bind. *)
  print_deps "map" (Memo.map (read a) ~f:(fun () -> ()));
  [%expect {| map: Singleton (Some "a", ()) |}];
  print_deps "bind" (Memo.bind (read a) ~f:(fun () -> Memo.return ()));
  [%expect {| bind: Singleton (Some "a", ()) |}];
  (* Sequential dependencies form a [Seq] in chronological order. *)
  print_deps
    "seq"
    (let* () = read a in
     let* () = read b in
     read c);
  [%expect
    {|
    seq: Seq
      [ Singleton (Some "a", ())
      ; Singleton (Some "b", ())
      ; Singleton (Some "c", ())
      ]
    |}];
  (* A parallel combinator forms a [Par]. *)
  print_deps "par" (Memo.parallel_map [ a; b; c ] ~f:read |> Memo.map ~f:ignore);
  [%expect
    {|
    par: Par
      [ Singleton (Some "a", ())
      ; Singleton (Some "b", ())
      ; Singleton (Some "c", ())
      ]
    |}];
  (* A single-thread parallel section is not wrapped in [Par], and nested [Seq]s are
     flattened, so the result is one flat [Seq] in chronological order. *)
  print_deps
    "flatten_seqs"
    (let* () = read a in
     let* (_ : unit list) =
       Memo.parallel_map [ () ] ~f:(fun () ->
         let* () = read b in
         read c)
     in
     Memo.return ());
  [%expect
    {|
    flatten_seqs: Seq
      [ Singleton (Some "a", ())
      ; Singleton (Some "b", ())
      ; Singleton (Some "c", ())
      ]
    |}];
  (* Empty parallel and sequential sections collapse away, even when nested. *)
  let e = Memo.return () in
  let par m = Memo.map (Memo.all_concurrently [ m; e ]) ~f:ignore in
  let seq m = Memo.map m ~f:(fun () -> ()) in
  print_deps "nested_empty" (seq (par (seq (par e))));
  [%expect {| nested_empty: Empty |}]
;;
