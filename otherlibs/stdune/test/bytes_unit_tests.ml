open Stdune

let () =
  (* We assert some properties of the conversion table here. It should be sorted
     by the values in the second component and the suffix list must be
     non-empty. *)
  let rec loop = function
    | [] -> ()
    | [ (units, _) ] -> assert (List.length units >= 1)
    | (units, value) :: ((_, value') :: _ as l) ->
      assert (List.length units >= 1);
      assert (value <= value');
      loop l
  in
  loop Bytes_unit.conversion_table
;;

let test bytes = List.iter ~f:(fun x -> Bytes_unit.pp x |> print_endline) bytes

let%expect_test "Testing significant digit boundaries" =
  test
    [ 0L
    ; 1L
    ; 12L
    ; 123L
    ; 1234L
    ; 12345L
    ; 123456L
    ; 1234567L
    ; 12345678L
    ; 123456789L
    ; 1234567890L
    ; 12345678901L
    ; 123456789012L
    ; 1234567890123L
    ; 12345678901234L
    ; 123456789012345L
    ; 1234567890123456L
    ];
  [%expect
    {|
      0B
      1B
      12B
      123B
      1.23kB
      12.35kB
      123.46kB
      1.23MB
      12.35MB
      123.46MB
      1.23GB
      12.35GB
      123.46GB
      1.23TB
      12.35TB
      123.46TB
      1234.57TB |}]
;;

(* CR-someday alizter: we should raise an exception here rather than giving a meaningless value. *)
(* Negative units get truncated but still printed as a negative. *)
let%expect_test "Negative units" =
  test [ -1L; -10L ];
  [%expect {|
    -0.00TB
    -0.00TB
    |}]
;;
