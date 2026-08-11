open Stdune

let print_json_string_bytes string =
  Json.to_string (`String string)
  |> String.iter ~f:(fun char -> Printf.printf "%02x " (Char.code char));
  print_newline ()
;;

let%expect_test "non-finite floats are rejected" =
  List.iter [ Stdlib.infinity; Stdlib.neg_infinity; Stdlib.nan ] ~f:(fun float ->
    match Json.to_string (`Float float) with
    | _ -> print_endline "accepted non-finite float"
    | exception Code_error.E _ -> print_endline "rejected non-finite float");
  [%expect
    {|
    rejected non-finite float
    rejected non-finite float
    rejected non-finite float
    |}]
;;

let%expect_test "invalid UTF-8 is replaced in JSON strings" =
  List.iter
    [ "\xc0\x80" (* overlong two-byte encoding *)
    ; "\xe0\x80\x80" (* overlong three-byte encoding *)
    ; "\xed\xa0\x80" (* UTF-16 surrogate *)
    ; "\xf4\x90\x80\x80" (* code point above U+10FFFF *)
    ; "\xf5\x80\x80\x80" (* invalid four-byte leader *)
    ]
    ~f:print_json_string_bytes;
  [%expect
    {|
    22 c0 80 22
    22 e0 80 80 22
    22 ed a0 80 22
    22 f4 90 80 80 22
    22 f5 80 80 80 22
    |}]
;;
