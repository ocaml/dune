open Stdune
open Fiber.O

module Scheduler = struct
  let t = Test_scheduler.create ()
  let run f = Test_scheduler.run t f
end

let map_reduce_variants () =
  [ ( "seq"
    , fun input ~f ~empty ~combine ->
        Fiber.map_reduce_seq (List.to_seq input) ~f ~empty ~combine )
  ; ( "array"
    , fun input ~f ~empty ~combine ->
        Fiber.map_reduce_array (Array.of_list input) ~f ~empty ~combine )
  ; ("list", fun input ~f ~empty ~combine -> Fiber.map_reduce input ~f ~empty ~combine)
  ]
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
    result: [3; 2; 1] |}]
;;
