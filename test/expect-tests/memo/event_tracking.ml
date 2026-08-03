(* Tests for tracking Memo node events. *)

open! Stdune
open! Memo.O
open Test_helpers.Make ()

let%expect_test _ =
  let divisor = Memo.Var.create ~name:"divisor" 2 in
  let table =
    Memo.create_rec
      "division via subtraction"
      ~input:(module Int)
      ~cutoff:Int.equal
      ~on_event:(fun input event ->
        let event =
          match (event : Memo.Event.t) with
          | Live -> "Live"
          | Validated -> "Validated"
        in
        printf "[on_event %d %s] called\n" input event)
      (fun f input ->
         let* divisor = Memo.Var.read divisor in
         if divisor < 0 then failwith "Negative divisors are not allowed!";
         if input < divisor
         then Memo.return 0
         else if input = divisor
         then Memo.return 1
         else
           let+ result = f (input - divisor) in
           result + 1)
  in
  List.iter [ 4; 5; 6; 4; 5; 6 ] ~f:(evaluate_and_print table);
  (* Nodes {1..6} become live; [Live] then [Validated] fire once for each live node. *)
  [%expect
    {|
    [on_event 4 Live] called
    [on_event 2 Live] called
    [on_event 2 Validated] called
    [on_event 4 Validated] called
    f 4 = Ok 2
    [on_event 5 Live] called
    [on_event 3 Live] called
    [on_event 1 Live] called
    [on_event 1 Validated] called
    [on_event 3 Validated] called
    [on_event 5 Validated] called
    f 5 = Ok 2
    [on_event 6 Live] called
    [on_event 6 Validated] called
    f 6 = Ok 3
    f 4 = Ok 2
    f 5 = Ok 2
    f 6 = Ok 3
    |}];
  Memo.reset (Memo.Var.set divisor 3);
  List.iter [ 3; 6; 9 ] ~f:(evaluate_and_print table);
  [%expect
    {|
    [on_event 3 Live] called
    [on_event 3 Validated] called
    f 3 = Ok 1
    [on_event 6 Live] called
    [on_event 6 Validated] called
    f 6 = Ok 2
    [on_event 9 Live] called
    [on_event 9 Validated] called
    f 9 = Ok 3
    |}];
  Memo.reset (Memo.Var.set divisor (-5));
  List.iter [ 8; 9; 9 ] ~f:(evaluate_and_print table);
  (* Liveness tracking works for nodes whose outputs are errors. *)
  [%expect
    {|
    [on_event 8 Live] called
    [on_event 8 Validated] called
    f 8 = Error
            [ { exn = "Failure(\"Negative divisors are not allowed!\")"
              ; backtrace = ""
              }
            ]
    [on_event 9 Live] called
    [on_event 9 Validated] called
    f 9 = Error
            [ { exn = "Failure(\"Negative divisors are not allowed!\")"
              ; backtrace = ""
              }
            ]
    f 9 = Error
            [ { exn = "Failure(\"Negative divisors are not allowed!\")"
              ; backtrace = ""
              }
            ]
    |}];
  Memo.reset (Memo.Var.set divisor 0);
  List.iter [ 9; 10; 9 ] ~f:(evaluate_and_print table);
  (* Liveness tracking works with dependency cycles too. *)
  [%expect
    {|
    [on_event 9 Live] called
    [on_event 9 Validated] called
    Dependency cycle detected:
    - ("division via subtraction", 9)
    f 9 = Error
            [ { exn = "Cycle_error.E [ (\"division via subtraction\", 9) ]"
              ; backtrace = ""
              }
            ]
    [on_event 10 Live] called
    [on_event 10 Validated] called
    Dependency cycle detected:
    - ("division via subtraction", 10)
    f 10 = Error
             [ { exn = "Cycle_error.E [ (\"division via subtraction\", 10) ]"
               ; backtrace = ""
               }
             ]
    Dependency cycle detected:
    - ("division via subtraction", 9)
    f 9 = Error
            [ { exn = "Cycle_error.E [ (\"division via subtraction\", 9) ]"
              ; backtrace = ""
              }
            ]
    |}]
;;
