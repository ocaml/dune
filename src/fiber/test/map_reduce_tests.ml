open Stdune
open Fiber.O

module Scheduler = struct
  let t = Test_scheduler.create ()
  let run f = Test_scheduler.run t f
  let yield () = Test_scheduler.yield t
end

let map_reduce_variants () =
  [ ( "seq"
    , fun input ~f ~empty ~combine ->
        Fiber.map_reduce_seq (List.to_seq input) ~f ~empty ~combine )
  ; ( "array"
    , fun input ~f ~empty ~combine ->
        Fiber.map_reduce_array (Array.of_list input) ~f ~empty ~combine )
  ; ("list", fun input ~f ~empty ~combine -> Fiber.map_reduce input ~f ~empty ~combine)
  ; ( "appendable_list"
    , fun input ~f ~empty ~combine ->
        Fiber.map_reduce_appendable_list
          (Appendable_list.of_list input)
          ~f
          ~empty
          ~combine )
  ; ( "appendable_tree"
    , fun input ~f ~empty ~combine ->
        let tree =
          let open Appendable_list in
          List.fold_left input ~init:empty ~f:(fun acc x ->
            concat [ empty; acc @ cons x (concat [ empty; empty ]); empty ])
        in
        Fiber.map_reduce_appendable_list tree ~f ~empty ~combine )
  ]
;;

let failure_messages errors =
  List.map errors ~f:(fun { Exn_with_backtrace.exn; _ } ->
    match exn with
    | Failure message -> message
    | exn -> Printexc.to_string exn)
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:","
;;

let%expect_test "map_reduce" =
  let test =
    Fiber.sequential_iter (map_reduce_variants ()) ~f:(fun (name, map_reduce) ->
      printfn "%s:" name;
      let+ res =
        map_reduce
          [ 1; 2; 3 ]
          ~f:(fun x ->
            printfn "x: %d" x;
            Fiber.return x)
          ~empty:0
          ~combine:( + )
      in
      printfn "final: %d" res)
  in
  Scheduler.run test;
  [%expect
    {|
    seq:
    x: 1
    x: 2
    x: 3
    final: 6
    array:
    x: 1
    x: 2
    x: 3
    final: 6
    list:
    x: 1
    x: 2
    x: 3
    final: 6
    appendable_list:
    x: 1
    x: 2
    x: 3
    final: 6
    appendable_tree:
    x: 1
    x: 2
    x: 3
    final: 6 |}]
;;

let%expect_test "map_reduce waits for all fibers" =
  let test =
    Fiber.sequential_iter (map_reduce_variants ()) ~f:(fun (name, map_reduce) ->
      printfn "%s:" name;
      let ivars = List.init 3 ~f:(fun _ -> Fiber.Ivar.create ()) in
      Fiber.fork_and_join_unit
        (fun () ->
           let+ res =
             map_reduce
               ivars
               ~f:(fun ivar ->
                 let+ x = Fiber.Ivar.read ivar in
                 printfn "x: %d" x;
                 x)
               ~empty:0
               ~combine:( + )
           in
           printfn "final: %d" res)
        (fun () ->
           let i = ref 0 in
           Fiber.parallel_iter ivars ~f:(fun ivar ->
             incr i;
             printfn "filling ivar %d" !i;
             Fiber.Ivar.fill ivar !i)))
  in
  Scheduler.run test;
  [%expect
    {|
    seq:
    filling ivar 1
    filling ivar 2
    filling ivar 3
    x: 1
    x: 2
    x: 3
    final: 6
    array:
    filling ivar 1
    filling ivar 2
    filling ivar 3
    x: 1
    x: 2
    x: 3
    final: 6
    list:
    filling ivar 1
    filling ivar 2
    filling ivar 3
    x: 1
    x: 2
    x: 3
    final: 6
    appendable_list:
    filling ivar 1
    filling ivar 2
    filling ivar 3
    x: 1
    x: 2
    x: 3
    final: 6
    appendable_tree:
    filling ivar 1
    filling ivar 2
    filling ivar 3
    x: 1
    x: 2
    x: 3
    final: 6 |}]
;;

let%expect_test "map_reduce may combine in completion order" =
  let test =
    Fiber.sequential_iter (map_reduce_variants ()) ~f:(fun (name, map_reduce) ->
      printfn "%s:" name;
      let iv1 = Fiber.Ivar.create () in
      let iv2 = Fiber.Ivar.create () in
      let iv3 = Fiber.Ivar.create () in
      Fiber.fork_and_join_unit
        (fun () ->
           let+ res =
             map_reduce
               [ 1, iv1; 2, iv2; 3, iv3 ]
               ~f:(fun (n, ivar) ->
                 let+ () = Fiber.Ivar.read ivar in
                 printfn "completed: %d" n;
                 [ n ])
               ~empty:[]
               ~combine:List.append
           in
           let res = List.map res ~f:string_of_int |> String.concat ~sep:"; " in
           printfn "result: [%s]" res)
        (fun () ->
           let* () = Fiber.Ivar.fill iv3 () in
           let* () = Fiber.Ivar.fill iv2 () in
           Fiber.Ivar.fill iv1 ()))
  in
  Scheduler.run test;
  [%expect
    {|
    seq:
    completed: 3
    completed: 2
    completed: 1
    result: [3; 2; 1]
    array:
    completed: 3
    completed: 2
    completed: 1
    result: [3; 2; 1]
    list:
    completed: 3
    completed: 2
    completed: 1
    result: [3; 2; 1]
    appendable_list:
    completed: 3
    completed: 2
    completed: 1
    result: [3; 2; 1]
    appendable_tree:
    completed: 3
    completed: 2
    completed: 1
    result: [3; 2; 1] |}]
;;

let%expect_test "map_reduce collects worker and combiner exceptions" =
  let test =
    Fiber.sequential_iter (map_reduce_variants ()) ~f:(fun (name, map_reduce) ->
      let workers_run = ref 0 in
      let* worker_result =
        Fiber.collect_errors (fun () ->
          map_reduce
            [ 1; 2; 3 ]
            ~f:(fun x ->
              let* () = Scheduler.yield () in
              incr workers_run;
              if x mod 2 = 1
              then failwith (Printf.sprintf "worker %d" x)
              else Fiber.return x)
            ~empty:0
            ~combine:( + ))
      in
      (match worker_result with
       | Error errors ->
         printfn
           "%s worker errors: %s (ran=%d)"
           name
           (failure_messages errors)
           !workers_run
       | Ok _ -> printfn "%s worker errors: unexpected success" name);
      workers_run := 0;
      let+ combine_result =
        Fiber.collect_errors (fun () ->
          map_reduce
            [ 1; 2; 3 ]
            ~f:(fun x ->
              let+ () = Scheduler.yield () in
              incr workers_run;
              x)
            ~empty:0
            ~combine:(fun acc x -> if x = 2 then failwith "combine" else acc + x))
      in
      match combine_result with
      | Error errors ->
        printfn
          "%s combine errors: %s (ran=%d)"
          name
          (failure_messages errors)
          !workers_run
      | Ok _ -> printfn "%s combine errors: unexpected success" name)
  in
  Scheduler.run test;
  [%expect
    {|
    seq worker errors: worker 1,worker 3 (ran=3)
    seq combine errors: combine (ran=3)
    array worker errors: worker 1,worker 3 (ran=3)
    array combine errors: combine (ran=3)
    list worker errors: worker 1,worker 3 (ran=3)
    list combine errors: combine (ran=3)
    appendable_list worker errors: worker 1,worker 3 (ran=3)
    appendable_list combine errors: combine (ran=3)
    appendable_tree worker errors: worker 1,worker 3 (ran=3)
    appendable_tree combine errors: combine (ran=3) |}]
;;

let%expect_test "map_reduce empty and singleton inputs" =
  Scheduler.run
    (Fiber.sequential_iter (map_reduce_variants ()) ~f:(fun (name, map_reduce) ->
       let calls = ref 0 in
       let f x =
         incr calls;
         Fiber.return x
       in
       let* empty = map_reduce [] ~f ~empty:17 ~combine:( + ) in
       let+ singleton = map_reduce [ 7 ] ~f ~empty:0 ~combine:( + ) in
       printfn "%s: empty=%d singleton=%d calls=%d" name empty singleton !calls));
  [%expect
    {|
    seq: empty=17 singleton=7 calls=1
    array: empty=17 singleton=7 calls=1
    list: empty=17 singleton=7 calls=1
    appendable_list: empty=17 singleton=7 calls=1
    appendable_tree: empty=17 singleton=7 calls=1
    |}]
;;

let%expect_test "map_reduce_appendable_list traverses mixed nodes in order" =
  let open Appendable_list in
  let left = concat [ empty; of_list [ 1; 2; 3 ]; cons 4 empty ] in
  let right =
    of_list [ 5; 6 ] @ concat [ cons 7 (singleton 8); empty; of_list [ 9; 10 ] ]
  in
  let tree = concat [ left @ right; empty; singleton 11 ] in
  let seen = ref [] in
  let computation =
    Fiber.map_reduce_appendable_list tree ~empty:0 ~combine:( + ) ~f:(fun n ->
      seen := n :: !seen;
      let+ () = Scheduler.yield () in
      n)
  in
  for _ = 1 to 2 do
    seen := [];
    let sum = Scheduler.run computation in
    printfn
      "sum=%d order=%s"
      sum
      (List.rev !seen |> List.map ~f:Int.to_string |> String.concat ~sep:",")
  done;
  [%expect
    {|
    sum=66 order=1,2,3,4,5,6,7,8,9,10,11
    sum=66 order=1,2,3,4,5,6,7,8,9,10,11
    |}]
;;

let%expect_test "map_reduce_appendable_list is stack safe across suspensions" =
  let open Appendable_list in
  let shapes =
    [ ("cons", fun acc n -> cons n acc)
    ; ("left append", fun acc n -> acc @ of_list [ n; n ])
    ; ("right append", fun acc n -> of_list [ n; n ] @ acc)
    ; ("concat", fun acc n -> concat [ acc; empty; singleton n ])
    ; ("empty", fun acc _ -> concat [ acc; empty ])
    ]
  in
  Scheduler.run
    (Fiber.sequential_iter shapes ~f:(fun (name, add) ->
       let rec make n acc = if n = 100_000 then acc else make (n + 1) (add acc n) in
       let tree = make 0 empty in
       let+ count =
         Fiber.map_reduce_appendable_list tree ~empty:0 ~combine:( + ) ~f:(fun n ->
           if n mod 10_000 = 0 then Scheduler.yield () >>| fun () -> 1 else Fiber.return 1)
       in
       printfn "%s: %d" name count));
  [%expect
    {|
    cons: 100000
    left append: 200000
    right append: 200000
    concat: 100000
    empty: 0
    |}]
;;
