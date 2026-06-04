open Stdune

let%expect_test "Tuple.T2.hash distinguishes trivial inputs" =
  (* Bool.hash false = 0, so a 2-tuple of all-zeros would collide with
     ([], false), etc. if the combiner forgot to seed the accumulator.
     The Hash module's [create ()] seed of 1 keeps these distinct. *)
  let h = Tuple.T2.hash Bool.hash Bool.hash in
  let print xy = Printf.printf "%d\n" (h xy) in
  print (false, false);
  print (false, true);
  print (true, false);
  print (true, true);
  [%expect
    {|
    961
    962
    992
    993
    |}]
;;

let%expect_test "Tuple.T3.hash distinguishes trivial inputs" =
  let h = Tuple.T3.hash Bool.hash Bool.hash Bool.hash in
  let print xyz = Printf.printf "%d\n" (h xyz) in
  print (false, false, false);
  print (false, false, true);
  print (false, true, false);
  print (true, false, false);
  [%expect
    {|
    29791
    29792
    29822
    30752
    |}]
;;
